;;;
;;; Bytecode2C translator
;;;

;; this library is used to translate bytecode fragments to pieces of C-code. 
;; owl translates code to cps-form before compiling it eventually to bytecode.
;; as a consequence of this, the bytecode only runs forwards, can only make 
;; jumps forwards, and thus runs in O(n) being the the number of instructions.
;; these code fragments can be translated to equivalent fragments of C-code, 
;; which can be used to automatically build another program-specific custom 
;; vm alongside the small default one. naturally, as this removes most of the 
;; instruction dispatch overhead, this increases speed of the resulting 
;; programs.

;; todo: keep all fixnum variables in registers unboxed with a special type, and add guards to saves and calls to tag them lazily. this would remove a lot of payload shifting from math code.

(define-module lib-cgen
	(export 
		compile-to-c            ;; obj extras → False | (arity . c-code-string)
		code->bytes             ;; obj extras → False | (byte ...)
	)

   (import lib-assemble inst->op)

   (define alloc-types
      (list->ff
         '((1 . pair))))


	(define (renderer o t) (render render o t))

   ;; represent some immediate as a string in C
	(define (represent val fail)
		(cond
			((eq? val null) "INULL")
			((eq? val True) "ITRUE")
			((eq? val False) "IFALSE")
			((and (teq? val fix+) (< val 256))
				(bytes->string
					(foldr renderer null
						(list "fixnum(" val ")"))))
			(else 
				(show "represent: cannot yet handle " val)
				(fail))))

	; -> list of bytes | False
	(define (code->bytes code extras)
		(if (bytecode? code)
			(let ((bytes (map (λ (p) (refb code p)) (iota 0 1 (sizeb code)))))
				(if (eq? (cadr bytes) 0) ;; (<arity> 0 <hi8> <lo8>) == call extra instruction
               (lets
                  ((opcode (+ (<< (caddr bytes) 8) (car (cdddr bytes))))
                   (bytecode (get extras opcode False)))
                  (if bytecode
                     (code->bytes bytecode extras) ;; <- vanilla bytecode (modulo boostrap bugs)
                     (error "code->bytes: cannot find original bytecode for opcode " opcode)))
               bytes))
			False))

   (define (unknown bs regs fail)
      ;(show " - cgen does not grok opcode " (car bs))
      (fail))

   (define (get2 l) ; (a b . tl)
      (let ((tl (cdr l))) 
         (values (car l) (car tl) (cdr tl))))

   (define (get3 l)
      (lets ((b c tl (get2 (cdr l))))
         (values (car l) b c tl)))

   (define (get4 l)
      (lets ((b c d tl (get3 (cdr l))))
         (values (car l) b c d tl)))

   (define (get5 l)
      (lets ((b c d e tl (get4 (cdr l))))
         (values (car l) b c d e tl)))

   (define (get6 l)
      (lets ((b c d e f tl (get5 (cdr l))))
         (values (car l) b c d e f tl)))

   ;; register values
   ;;    False | not set = no idea what is here
   ;;    one of immediate (bool, fixnum) -> immediate (of this type)
   ;;    one of alloc (pair) -> allocated object (of this type)

   (define (alloc? v)
      (cond
         ((not v) F)
         ((has? '(pair alloc) v) T)
         (else F)))

   ;; drop code to check that value in ref is a pointer (not immediate) unless this is already known in regs
   (define (assert-alloc regs reg op tl)
      (if (alloc? (get regs reg False))
         (begin
            ;(print " >>> no need to assert <<<")
            tl)
         (ilist "assert(allocp(R["reg"]),R["reg"],"op");" tl)))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; translator functions


   ;; lraw lst-reg type-reg flipp-reg to
   (define (cify-sysprim bs regs fail) 
      (lets ((op a1 a2 a3 ret bs (get5 (cdr bs))))
         (values
            (list "R["ret"]=prim_sys(fixval(R["op"]), R["a1"], R["a2"], R["a3"]);")
            bs (del regs ret))))

   (define (cify-type bs regs fail) 
      (lets ((ob to bs (get2 (cdr bs))))
         (values (list "R["to"]=(allocp(R["ob"]))?V(R["ob"]):R["ob"];R["to"]=fixnum(R["to"]&4095);") bs 
            (put (put regs ob 'alloc) to 'fixnum))))

   ;; lraw lst-reg type-reg flipp-reg to
   (define (cify-sizeb bs regs fail) 
      (lets ((ob to bs (get2 (cdr bs))))
         (values 
            (list "if(immediatep(R["ob"])){R["to"]=fixnum(0);}else{word h=V(R[" ob "]);R["to"]=fixnum((hdrsize(h)-1)*W-((h>>8)&7));}")
            bs (put regs to 'fixnum)))) ;; output is always a fixnum

   ;; fftoggle node to
   (define (cify-fftoggle bs regs fail) 
      (lets ((ob to bs (get2 (cdr bs))))
         (cond
            (else 
               (values 
                  (list "R["to"]=(immediatep(R["ob"]))?IFALSE:prim_cast((word *)R["ob"],(V(R["ob"])>>3)^FFRED);")
                  bs (put regs to (get regs ob False)))))))

   ;; lraw lst-reg type-reg flipp-reg to
   (define (cify-size bs regs fail) 
      (lets ((ob to bs (get2 (cdr bs))))
         (cond
            (else 
               (values 
                  (list "R["to"]=(immediatep(R["ob"]))?fixnum(0):fixnum(imm_val(V(R["ob"]))-1);")
                  bs (put regs to 'fixnum))))))

   ;; lraw lst-reg type-reg flipp-reg to
   (define (cify-lraw bs regs fail) 
      (lets ((lr tr fr to bs (get4 (cdr bs))))
         (values (list "R["to"]=prim_lraw(R["lr"],fixval(R["tr"]),R["fr"]);") bs    
            (del regs to)))) ; <- lraw can fail

   ;; ref ob pos to
   (define (cify-ref bs regs fail) 
      (lets ((ob pos to bs (get3 (cdr bs))))
         (values (list "R["to"]=prim_ref(R["ob"],R["pos"]);") bs 
            (del regs to)))) 

   ; fx+ a b r o?
   (define (cify-fxadd bs regs fail) 
      (lets ((a b r o bs (get4 (cdr bs))))
         (cond
            (else 
               (values
                  (list "{word res=fixval(R["a"])+fixval(R["b"]);R["r"]=fixnum(res&0xffff);R["o"]=(res>>16)?ITRUE:IFALSE;}")
                  bs (put (put regs r 'fixnum) o 'bool))))))

   ; fxband a b r
   (define (cify-fxband bs regs fail) 
      (lets ((a b r bs (get3 (cdr bs))))
         (values (list "R["r"]=R["a"]&R["b"];") bs 
            (put regs r 'fixnum))))

   ; fxbor a b r
   (define (cify-fxbor bs regs fail) 
      (lets ((a b r bs (get3 (cdr bs))))
         (values (list "R["r"]=R["a"]|R["b"];") bs 
            (put regs r 'fixnum))))
   
   ; fxbxor a b r
   (define (cify-fxbxor bs regs fail) 
      (lets ((a b r bs (get3 (cdr bs))))
         (values (list "R["r"]=R["a"]^(R["b"]^2);") bs 
            (put regs r 'fixnum))))

   ; fx* a b l h
   (define (cify-fxmul bs regs fail) 
      (lets ((a b l h bs (get4 (cdr bs))))
         (values
            (list "{word res=fixval(R["a"])*fixval(R["b"]);R["l"]=fixnum(res&0xffff);R["h"]=fixnum(res>>16);}")
            bs (put (put regs h 'fixnum) l 'fixnum))))

   ; fx- a b r u?
   (define (cify-fxsub bs regs fail) 
      (lets ((a b r u bs (get4 (cdr bs))))
         (values
            (list "{word a=fixval(R["a"]);word b=fixval(R["b"]);if(b>a){R["r"]=fixnum((a|0x10000)-b);R["u"]=ITRUE;}else{R["r"]=fixnum(a-b);R["u"]=IFALSE;}}")
            bs (put (put regs r 'fixnum) u 'bool))))

   ; fx<< a b hi lo
   (define (cify-fxleft bs regs fail) 
      (lets ((a b hi lo bs (get4 (cdr bs))))
         (values
            (list "{word res=fixval(R["a"])<<fixval(R["b"]);R["hi"]=fixnum(res>>16);R["lo"]=fixnum(res&0xffff);}")
            bs (put (put regs lo 'fixnum) hi 'fixnum))))

   ; fx>> a b hi lo
   (define (cify-fxright bs regs fail)
      (lets ((a b hi lo bs (get4 (cdr bs))))
         (values
            (list "{word r=fixval(R["a"])<<(16-fixval(R["b"]));R["hi"]=fixnum(r>>16);R["lo"]=fixnum(r&0xffff);}")
            bs (put (put regs lo 'fixnum) hi 'fixnum))))

   ; fxqr ah al b qh ql rem, for (ah<<16 | al) = (qh<<16 | ql)*b + rem
   (define (cify-fxqr bs regs fail)
      (lets ((ah al b qh ql rem bs (get6 (cdr bs))))
         (values
            (list "{word a=fixval(R["ah"])<<16|fixval(R["al"]);word b=fixval(R["b"]);word q=a/b;R["qh"]=fixnum(q>>16);R["ql"]=fixnum(q&0xffff);R["rem"]=fixnum(a-q*b);}")
            bs (put (put (put regs qh 'fixnum) ql 'fixnum) rem 'fixnum))))

   ; fxqr ah al b qh ql rem, for (ah<<16 | al) = (qh<<16 | ql)*b + rem
   (define (cifyer-mkff type)
      (λ (bs regs fail)
         (lets ((l k v r to bs (get5 (cdr bs))))
            (values (list "R["to"]=prim_mkff("type",R["l"],R["k"],R["v"],R["r"]);") bs 
               (put regs to 'alloc)))))

   ; red? ob to
   (define (cify-red? bs regs fail)
      (lets ((ob to bs (get2 (cdr bs))))
         (values 
            (list "R["to"]=(allocp(R["ob"])&&(V(R["ob"])&(FFRED<<3)))?ITRUE:IFALSE;") bs 
               (put regs to 'bool))))

   ; bind tuple n r0 .. rn 
   (define (cify-bind bs regs fail)
      (lets 
         ((ob n bs (get2 (cdr bs)))
          (targets (take bs n))
          (bs (drop bs n)))
         (values
            (ilist "{word *ob=(word *)R["ob"];word hdr;"
               (assert-alloc regs ob "IFALSE" 
                  (ilist "hdr=*ob;assert_not((rawp(hdr)||imm_val(hdr)!="(+ 1 n)"),ob,IFALSE);"
                     (foldr
                        (λ (n tl) ;; n = (reg . pos)
                           (ilist "R[" (car n) "]=ob[" (cdr n) "];" tl))
                        (list "}")
                        (zip cons targets (iota 1 1 (+ n 1)))))))
            bs 
            (fold del regs targets))))

      
   ; bind node left key val right, filling in False when implicit
   (define (cify-bindff bs regs fail)
      ;; note, may overwrite n while binding
      (lets ((n l k v r bs (get5 (cdr bs))))
         (values ;; would probably be a bad idea to use prim_withff(&l, &r, ...), as those have at 
                 ;; least earlier caused an immense slowdown in compiled code
            (assert-alloc regs n 1049 
               (list "{word *ob=(word *)R["n"];word hdr=*ob>>3;if((hdr&31)!=TFF){error(1049,ob,INULL);};R["k"]=ob[1];R["v"]=ob[2];if(hdr&FFLEFT){R["l"]=ob[3];R["r"]=(hdr&FFRIGHT)?ob[4]:IFALSE;}else{R["l"]=IFALSE;R["r"]=(hdr&FFRIGHT)?ob[3]:IFALSE;}};"))
            bs 
            (fold del regs (list l k v r)))))

   (define (cify-cast bs regs fail)
      (lets ((ob type to bs (get3 (cdr bs))))
         (values 
            (list "R["to"]=prim_cast((word *)R["ob"],fixval(R["type"])&0xff);") bs 
            (del regs to))))

   (define (cify-mkt bs regs fail)
      (lets 
         ((type sp bs (get2 (cdr bs))) ; object size is nfields + 1, being the header
          (nfields (+ sp 1))
          (fields (take bs nfields))
          (bs (drop bs nfields))
          (to bs bs))
         (values
            (ilist "*fp=make_header(" (+ nfields 1)","type");"
                (foldr ; <- fixme: switch to foldr to write in-order
                  (λ (p tl) ; <- (pos . reg)
                     (ilist "fp[" (car p) "]=R[" (cdr p) "];" tl))
                  (list "R[" to "]=(word)fp;fp+=" (+ nfields 1) ";")
                  (zip cons (iota 1 1 (+ nfields 1)) fields)))
            bs 
            (put regs to 'alloc))))

   (define (cify-closer type)
      (λ (bs regs fail)
         (lets 
            ((size litp litoff bs (get3 (cdr bs)))
             (nfields (- size 2)) ;; #[hdr <code> ...]
             (_ (if (<= (length bs) nfields) (fail) 42)) ;; <-- broken bytecode in heap?
             (fields (take bs nfields))
             (bs (drop bs nfields))
             (to bs bs))
            (values
               (ilist "*fp=make_header(" size "," type ");fp[1]=G(R["litp"],"litoff");"
                   (fold
                     (λ (tl p) ; <- (pos . reg)
                        (ilist "fp[" (car p) "]=R[" (cdr p) "];" tl))
                     (list "R[" to "]=(word)fp;fp+=" size ";")
                     (zip cons (iota 2 1 (+ size 1)) fields)))
               bs 
               (put regs to 'alloc)))))

   ;; == cify-closer, but with implicit 1 as litp
   (define (cify-closer-1 type)
      (λ (bs regs fail)
         (lets 
            ((size litoff bs (get2 (cdr bs)))
             (litp 1)
             (nfields (- size 2)) ;; #[hdr <code> ...]
             (fields (take bs nfields))
             (bs (drop bs nfields))
             (to bs bs))
            (values
               (ilist "*fp=make_header(" size "," type ");fp[1]=G(R["litp"],"litoff");"
                   (fold
                     (λ (tl p) ; <- (pos . reg)
                        (ilist "fp[" (car p) "]=R[" (cdr p) "];" tl))
                     (list "R[" to "]=(word)fp;fp+=" size ";")
                     (zip cons (iota 2 1 (+ size 1)) fields)))
               bs (put regs to 'alloc)))))

   (define (cify-jrt8 bs regs fail)
      (lets ((a type jump-len bs (get3 (cdr bs))))
         (cond
            (else 
               (values 'branch 
                  (tuple 
                     (list "allocp(R["a"])&&(V(R["a"])&2296)=="(bor 2048 (<< type 3)))
                     (drop bs jump-len) (put regs a 'alloc)
                     bs regs)
                  regs)))))

   (define (cify-jump-imm val)
      (λ (bs regs fail)
         (lets 
            ((a lo8 hi8 bs (get3 (cdr bs)))
             (jump-len (bor (<< hi8 8) lo8)))
            (values 'branch (tuple (list "R[" a "]==" (represent val fail)) (drop bs jump-len) regs bs regs) regs))))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; translator function dispatch ff

	(define translators 
      (list->ff
         (list
            (cons 1 ;; indirect-ref from-reg offset to-reg
               (λ (bs regs fail)
                  (lets ((from offset to bs (get3 (cdr bs))))
                     (cond
                        (else (values (list "R[" to "]=G(R[" from "],"offset");") bs (del regs to)))))))
            (cons 2 ;; goto <rator> <nargs>
               (λ (bs regs fail)
                  (lets ((rator nargs bs (get2 (cdr bs))))
                     (let ((code (list "ob=(word *)R[" rator "];acc=" nargs ";" )))
                        (values code null regs)))))
            (cons 3 (cify-closer "TCLOS"))
            (cons 4 (cify-closer "TPROC"))
            (cons 5 ;; ref1 offset to (read register 1, usually closure or immediates)
               (λ (bs regs fail)
                  (lets ((offset to bs (get2 (cdr bs))))
                     (cond
                        (else 
                           (values 
                              (list "R[" to "]=G(R[1]," offset ");")
                              bs (del regs to)))))))
            (cons 6 (cify-closer-1 "TCLOS"))
            (cons 7 (cify-closer-1 "TPROC"))
            (cons 8 ;; jump-if-equal a b lo8 hi8
               (λ (bs regs fail)
                  (lets 
                     ((a b lo8 hi8 bs (get4 (cdr bs)))
                      (jump-len (bor (<< hi8 8) lo8)))
                     (values 'branch (tuple (list "R[" a "]==R[" b "]") (drop bs jump-len) regs bs regs) regs))))
            (cons 9 ;; move to from
               (λ (bs regs fail)
                  (lets ((from to bs (get2 (cdr bs))))
                     (cond ;                                                        .--> note, maybe have False value set
                        (else (values (list "R[" to "]=R[" from "];") bs (put regs to (get regs from False))))))))
            (cons 10 cify-type)
            (cons 11 ;; jump-if-immediate-type a type lo8 hi8
               (λ (bs regs fail)
                  (lets 
                     ((a type lo8 hi8 bs (get4 (cdr bs)))
                      (jump-len (bor (<< hi8 8) lo8)))
                     (cond
                        (else (values 'branch 
                           (tuple 
                              (list "immediatep(R["a"])&&((((word)R["a"])>>3)&0xff)==" type)
                              (drop bs jump-len) (put regs a 'immediate)
                              bs (put regs a 'alloc)) regs))))))
            (cons 12 ;; jump-if-allocated-type a type lo8 hi8
               (λ (bs regs fail)
                  (lets 
                     ((a type lo8 hi8 bs (get4 (cdr bs)))
                      (jump-len (bor (<< hi8 8) lo8)))
                     (cond
                        (else (values 'branch 
                           (tuple 
                              (list "allocp(R["a"])&&(((V(R["a"]))>>3)&0x1ff)==" type)
                              (drop bs jump-len) 
                              (put regs a (get alloc-types type 'alloc))
                              bs regs) regs)))))) ; <- raw or immediate
            ;; 13=ldi, see higher ops
            (cons 14 ;; ldfix <n> <to>
               (λ (bs regs fail)
                  (lets ((n to bs (get2 (cdr bs))))
                     (cond
                        (else (values (list "R["to"]=fixnum(" n ");") bs (put regs to 'fixnum)))))))
            (cons 18 ;; goto-code <p>
               (λ (bs regs fail)
                  (let ((fun (cadr bs)))
                     (cond
                        (else 
                           (values 
                              (list "ob=(word *)R[" fun"];ip=((unsigned char *)ob)+W+1;goto invoke;")
                              null regs))))))
            (cons 19 ;; goto-proc <p>
               (λ (bs regs fail)
                  (let ((fun (cadr bs)))
                     (cond
                        (else 
                           (values 
                              (list "R[1]=R["fun"];ob=(word *)G(R[1],1);ip=((unsigned char *)ob)+W+1;goto invoke;")
                              null regs))))))
            (cons 21 ;; goto-clos <p>
               (λ (bs regs fail)
                  (let ((fun (cadr bs)))
                     (cond
                        (else 
                           (values 
                              (list "R[1]=R[" fun"];R[2]=G(R[1],1);ob=(word *)G(R[2],1);ip=((unsigned char *)ob)+W+1;goto invoke;")
                              null regs))))))
            (cons 22 cify-cast)
            (cons 23 cify-mkt)
            (cons 24 ;; ret r == call R3 with 1 argument at Rr
               (λ (bs regs fail)
                  (let ((res (cadr bs)))
                     (cond
                        (else
                           (values 
                              (list "ob=(word *)R[3];R[3]=R[" res "];acc=1;") ; the goto apply is automatic
                              null regs)))))) ;; <- always end compiling (another branch may continue here)
            (cons 26 cify-fxqr)
            (cons 28 cify-sizeb)
            (cons 29 ;; ncons car cdr to
               (λ (bs regs fail)
                  (lets ((a b to bs (get3 (cdr bs))))
                     (cond
                        (else (values (list "*fp=NUMHDR;fp[1]=R["a"];fp[2]=R["b"];R["to"]=(word)fp;fp+=3;") bs (put regs to 'bignum)))))))
            (cons 30 ;; ncar ob to (raw ref first), UNSAFE
               (λ (bs regs fail)
                  (lets ((from to bs (get2 (cdr bs))))
                     (cond
                        (else (values (list "R[" to "]=G(R[" from "],1);") bs (put regs to 'fixnum)))))))
            (cons 31 ;; ncdr ob to (raw ref first), UNSAFE
               (λ (bs regs fail)
                  (lets ((from to bs (get2 (cdr bs))))
                     (cond
                        (else (values (list "R[" to "]=G(R[" from "],2);") bs (del regs to)))))))
            (cons 32 cify-bind)
            (cons 33 cify-jrt8)
            (cons 36 cify-size)
            (cons 38 cify-fxadd)
            (cons 39 cify-fxmul)
            (cons 40 cify-fxsub)
            (cons 41 cify-red?)
            (cons 42 (cifyer-mkff "TFF"))
            (cons 43 (cifyer-mkff "TFF|FFRED"))
            (cons 44 ;; less a b r
               (λ (bs regs fail)
                  (lets ((a b to bs (get3 (cdr bs))))
                     (cond
                        (else (values (list "R["to"]=prim_less(R["a"],R["b"]);") bs (put regs to 'bool)))))))
            (cons 45 ;; set obj offset value to ;; TODO <- was adding this one
               (λ (bs regs fail)
                  (lets ((ob pos val to bs (get4 (cdr bs))))
                     (cond
                        (else (values (list "R["to"]=prim_set(R["ob"],R["pos"],R["val"]);") bs 
                           (put regs to (get regs ob 'alloc))))))))
            (cons 46 cify-fftoggle)
            (cons 47 cify-ref)
            (cons 48 cify-ref) ;; use refb (being merged)
            (cons 49 cify-bindff)
            (cons 51 ;; cons car cdr to
               (λ (bs regs fail)
                  (lets ((a b to bs (get3 (cdr bs))))
                     (cond
                        (else 
                           (values 
                              ;; cons directly to free area to avoid register overwriting
                              (list "*fp=PAIRHDR;fp[1]=R["a"];fp[2]=R["b"];R["to"]=(word)fp;fp+=3;")
                              bs (put regs to 'pair)))))))
            (cons 52 ;; car ob to <- use this to test whether the compiler type handling
               (λ (bs regs fail)
                  (lets 
                     ((from to bs (get2 (cdr bs)))
                      (known-type (get regs from False)))
                     (cond
                        ((eq? 'pair known-type)
                           ;(print " >>> omitting pair check from car <<< ")
                           (values (list "R[" to "]=G(R[" from "],1);") bs (del regs to)))
                        ((eq? 'alloc known-type)
                           ;(print " >>> omitting immediate check from car  <<< ")
                           (values
                              (list "assert((G(R[" from "])==PAIRHDR),R[" from "],1052);R[" to "]=G(R[" from "],1);")
                              bs (del (put regs from 'pair) to))) ;; upgrade to pair
                        (else
                           ;(if known-type (show " >>> car on unknown type <<< " known-type))
                           ;; check that it is a pointer and an object of correct type
                           (values 
                              (list "assert(pairp(R[" from "]),R[" from "],1052);R[" to "]=G(R[" from "],1);")
                              bs (del (put regs from 'pair) to)))))))
            (cons 53 ;; cdr ob to
               (λ (bs regs fail)
                  (lets 
                     ((from to bs (get2 (cdr bs)))
                      (known-type (get regs from False)))
                     (cond
                        ((eq? 'pair known-type)
                           ;(print " >>> omitting pair check from cdr <<< ")
                           (values (list "R[" to "]=G(R[" from "],2);") bs (del regs to)))
                        ((eq? 'alloc known-type)
                           ;(print " >>> omitting immediate check from cdr  <<< ")
                           (values
                              (list "assert((G(R[" from "])==PAIRHDR),R[" from "],1053);R[" to "]=G(R[" from "],2);")
                              bs (del (put regs from 'pair) to))) ;; upgrade to pair
                        (else
                           ;(if known-type (show " >>> cdr on unknown type <<< " known-type))
                           ;; check that it is a pointer and an object of correct type
                           (values 
                              (list "assert(pairp(R[" from "]),R[" from "],1053);R[" to "]=G(R[" from "],2);")
                              bs (del (put regs from 'pair) to)))))))
            (cons 54 ;; eq a b to
               (λ (bs regs fail)
                  (lets ((a b to bs (get3 (cdr bs))))
                     (cond
                        (else 
                           (values 
                              (list "R["to"]=(R["a"]==R["b"])?ITRUE:IFALSE;")
                              bs regs))))))
            (cons (+ 16 (<< 0 6)) (cify-jump-imm 0))
            (cons (+ 16 (<< 1 6)) (cify-jump-imm null))
            (cons (+ 16 (<< 2 6)) (cify-jump-imm True))
            (cons (+ 16 (<< 3 6)) (cify-jump-imm False))
            (cons 55 cify-fxband)
            (cons 56 cify-fxbor)
            (cons 57 cify-fxbxor)
            (cons 58 cify-fxright)
            (cons 59 cify-fxleft)
            (cons 60 cify-lraw)
            (cons 63 cify-sysprim)
            ;; below are lower primop + extra info (like 13=ldi<what>)
            (cons 77 ;; ldn r
               (λ (bs regs fail)
                  (let ((res (cadr bs)))
                     (cond
                        (else (values (list "R[" res "]=INULL;") (cddr bs) (put regs res 'null)))))))
            (cons 141 ;; ldt r
               (λ (bs regs fail)
                  (let ((res (cadr bs)))
                     (cond
                        (else (values (list "R[" res "]=ITRUE;") (cddr bs) (put regs res 'bool)))))))
            (cons 205 ;; ldf r
               (λ (bs regs fail)
                  (let ((res (cadr bs)))
                     (cond
                        (else (values (list "R[" res "]=IFALSE;") (cddr bs) (put regs res 'bool)))))))
            )))

   ;; regs is a ff of partial knowledge going downwards about things currently in registers
   ;; → (obs ... . tail)
   (define (emit-c ops regs fail tail)
      ;(show "emit-c: " (list 'ops ops 'types regs))
      (if (null? ops)
         tail
         (lets ((res tl regs ((get translators (car ops) unknown) ops regs fail)))
            (cond
               ;((eq? res True) ;; introduce missing local register for writing
               ;   (let ((reg tl)) ;; needed register
               ;      (ilist "{word r" reg ";" 
               ;         (emit-c ops (put regs reg reg) fail (cons "}" tail)))))
               ;((eq? res False) ;; read the register from vm register array
               ;   (let ((reg tl))
               ;      (ilist "{word r" reg "=R[" reg "];" 
               ;         (emit-c ops (put regs reg reg) fail (cons "}" tail)))))
               ((eq? res 'branch) ; 'branch #(<test> <then-bytecode> <else-bytecode>)
                  (lets ((condition then-bs then-regs else-bs else-regs tl))
                     (cons "if(" 
                        (append condition 
                           (cons "){"
                              (emit-c then-bs then-regs fail
                                 (cons "}else{"
                                    (emit-c else-bs else-regs fail (cons "}" tail)))))))))
                     
               (else ;; instruction compiled, handle the rest
                  (append res (emit-c tl regs fail tail)))))))

   ;; obj extras → False | (arity . c-code-string), to become #[arity 0 hi8 lo8] + c-code in vm
	(define (compile-to-c code extras)
		(if (bytecode? code)
         (let ((ops (code->bytes code extras)))
            ;(show " ************************************************** " ops)
            (call/cc
					(λ (ret)
                  (cons (car ops)
                     (list->string
                        (foldr renderer null
                           (emit-c (cdr ops) False (λ () (ret False)) null)))))))
         False))
  
)

