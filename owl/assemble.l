;;;
;;; Bytecode assembly
;;;

; ast -> bytecode, and keep a collection of the primitives

;; todo: split primops away to lib-primop, which should be imporable early on in repl build
;; todo: handle allocation and retargeting in a separate pass

,r "owl/register.l"
,r "owl/memuse.l"

(define-module lib-assemble

   (export assemble-code inst->op)

   (import lib-register allocate-registers n-registers)
   (import lib-memuse count-allocs)
   (import lib-primop)

   (define (self x) x)

   ;; fixme: partially deprecated
   (define vm-instructions
      (list->ff
      `((move . 9)      ; move a, t:      Rt = Ra
        (refi . 1)      ; refi a, p, t:   Rt = Ra[p], p unsigned
        (goto . 2)      ; jmp a, nargs    call Ra with nargs args
        (clos . 3)      ; clos lp, o, nenv, e0 ... en, t: 
        (cloc . 4)      ; cloc lp, o, nenv, e0 ... en, t: 
        (ref1 . 5)       ; ref1 p, t
        (clos1 . 6)
        (cloc1 . 7)
        ; 8 = jlq 
        (jit   . 9)       ; jit r type jmp 
        (jit2 . 11)       ; jit r type jmp1 jmp2 -> 
        (jat2 . 12)       ; jat r type jmp1 jmp2
        (movh . 13)       ; 
        (goto-code . 18)
        (goto-proc . 19)
        (goto-clos . 21)
        (igoto . 26)   ; indirect goto
        (jrt  . 33)       ; jrt r type jmp 
        (cons . 51)     ; cons a, b, t:   Rt = mkpair(a, b)
        (car  . 52)     ; car a, t:       Rt = car(a);
        (cdr  . 53)     ; cdr a, t:       Rt = cdr(a);
        (eq   . 54)     ; eq a, b, t:     Rt = (Ra == Rb) ? true : false;
        (jlq  . 8)      ; jlq a b o1 o2
        (mk   . 9)      ; mk n, a0, ..., an, t, size up to 256
        (mki  . 11)     ; mki size, type, v1, ..., vn, to
        (ref  . 12)     ; ref a, p, t     Rt = Ra[p] + checks, unsigned
        (ld   . 14)     ; ld a, t:        Rt = a, signed byte
        ;; ldi = 13
        (jz   . ,(+ 16 (<< 0 6)))     ; jump-imm[0], zero
        (jn   . ,(+ 16 (<< 1 6)))     ; jump-imm[1], null
        (jt   . ,(+ 16 (<< 2 6)))     ; jump-imm[2], true
        (jf   . ,(+ 16 (<< 3 6)))     ; jump-imm[3], false
        (ldn  . 77)     ; 13 + 1<<6
        (ldf  . 205)     ; ldf t:          Rt = false
        (ldt  . 141)     ; ldt t:          Rt = true
        (jeq  . 20)     ; jeq a, b, o:    ip += o if Ra == Rb      ; jump if eq?
        (ret  . 24)     ; ret a:          call R3 (usually cont) with Ra
        (jf2  . 25)     ; jf a, ol, oh
        (set . 25)     ; set a, p, b     Ra[Rp] = Rb
        (jbf . 26)     ; jump-binding tuple n f offset ... r1 ... rn
        )))

   (define (inst->op name)
      (or
         (get vm-instructions name False)
         (error "inst->op: unknown instruction " name)))

   (define (reg a)
      (if (teq? a fix+)
         (if (< a n-registers)
            a
            (error "register too high: " a))
         (error "bad register: " a)))


   ;;;
   ;;; Bytecode assembly
   ;;;

   (define (output-code op lst)
      (if (eq? op (fxband op #xff))
         (cons op lst)
         (output-code
            (>> op 8)
            (cons (band op #xff) lst))))

   ; rtl -> list of bytes
   (define (assemble code fail)
      (tuple-case code
         ((ret a)
            (list (inst->op 'ret) (reg a)))
         ((move a b more)
            (ilist (inst->op 'move) (reg a) (reg b) (assemble more fail)))
         ((prim op args to more)
            (cond
               ;; fixme: handle mk differently, this was supposed to be a temp hack
               ((> op #xff)
                  (output-code op
                     (cons (reg (length (cdr args))) ; tuple size
                        (cons (reg (car args)) ; type
                           (append (map reg (cdr args))
                              (cons (reg to)
                                 (assemble more fail)))))))
               ((variable-input-arity? op)
                  (cons op
                     (cons (length args)
                        (append (map reg args)
                           (cons (reg to)
                              (assemble more fail))))))
               ((fixnum? to)
                  (cons op
                     (append (map reg args)
                        (cons to
                           (assemble more fail)))))
               ((list? to)
                  (if (has? multiple-return-variable-primops op)    
                     (cons op
                        (append (map reg args)      
                           ; <- nargs implicit, FIXME check nargs opcode too
                           (append (map reg to)
                              (assemble more fail))))
                     (cons op
                        (append (map reg args)
                           (cons (length to)          ; <- prefix with output arity
                              (append (map reg to)
                                 (assemble more fail)))))))
               (else
                  (error "bad case of primop in assemble: " op))))
         ;; fixme: closures should have just one RTL node instead of separate ones for clos-proc and clos-code
         ((clos-proc lpos offset env to more)
            ;; make a 2-level closure
            (if (= lpos 1)
               (cons (inst->op 'clos1)
                  (cons (+ 2 (length env))      
                     ;; size of object (hdr code e0 ... en) 
                     (cons offset
                        (append (map reg env)
                           (cons (reg to)
                              (assemble more fail))))))
               (cons (inst->op 'clos)
                  (cons (+ 2 (length env))      
                     ;; size of object (hdr code e0 ... en) 
                     (cons (reg lpos)
                        (cons offset
                           (append (map reg env)
                              (cons (reg to)
                                 (assemble more fail)))))))))
         ((clos-code lpos offset env to more)      ;; make a 1-level closure
            (if (= lpos 1)
               (cons (inst->op 'cloc1)
                  (cons (+ 2 (length env))   
                     ;; size of object (hdr code e0 ... en) 
                     (cons offset
                        (append (map reg env)
                           (cons (reg to)
                              (assemble more fail))))))
               (cons (inst->op 'cloc)
                  (cons (+ 2 (length env))   
                     ;; size of object (hdr code e0 ... en) 
                     (cons (reg lpos)
                        (cons offset
                           (append (map reg env)
                              (cons (reg to)
                                 (assemble more fail)))))))))
         ((ld val to cont)
            (cond
               ;; todo: add implicit load values to free bits of the instruction
               ((eq? val null)
                  (ilist (inst->op 'ldn) (reg to)
                     (assemble cont fail)))
               ((fixnum? val)
                  (let ((code (assemble cont fail)))
                     (if (or (> val 126) (< val -126)) ; would be a bug
                        (error "ld: big value: " val))
                     (ilist (inst->op 'ld) 
                        (if (< val 0) (+ 256 val) val)
                        (reg to) code)))
               ((eq? val False)
                  (ilist (inst->op 'ldf) (reg to)
                     (assemble cont fail)))
               ((eq? val True)
                  (ilist (inst->op 'ldt) (reg to)
                     (assemble cont fail)))
               (else
                  (error "cannot assemble a load for " val))))
         ((refi from offset to more)
            (ilist 
               (inst->op 'refi) (reg from) offset (reg to) 
               (assemble more fail)))
         ((goto op nargs)
            (list (inst->op 'goto) (reg op) nargs))
         ((goto-code op n)
            (list (inst->op 'goto-code) (reg op)))
         ((goto-proc op n)
            (list (inst->op 'goto-proc) (reg op)))
         ((goto-clos op n)
            (list (inst->op 'goto-clos) (reg op)))
         ;; todo: all jumps could have parameterized lengths (0 = 1-byte, n>0 = 2-byte, being the max code length)
         ((jeq a b then else)
            (lets
               ((then (assemble then fail))
                (else (assemble else fail))
                (len (length else)))
               (cond
                  ((< len #xffff) (ilist (inst->op 'jlq) (reg a) (reg b) (band len #xff) (>> len 8) (append else then)))
                  (else (error "need a bigger jump instruction: length is " len)))))
         ((jz a then else)
            (lets
               ((then (assemble then fail))
                (else (assemble else fail))
                (len (length else)))
               (cond
                  ((< len #xffff) (ilist (inst->op 'jz) (reg a) (band len #xff) (>> len 8) (append else then)))
                  (else (error "need a bigger jump instruction: length is " len)))))
         ((jf a then else)
            (lets
               ((then (assemble then fail))
                (else (assemble else fail))
                (len (length else)))
               (cond
                  ((< len #xffff) (ilist (inst->op 'jf) (reg a) (band len #xff) (>> len 8) (append else then)))
                  (else (error "need a bigger jump instruction: length is " len)))))
         ((jn a then else)
            (lets
               ((then (assemble then fail))
                (else (assemble else fail))
                (len (length else)))
               (cond
                  ((< len #xffff) (ilist (inst->op 'jn) (reg a) (band len #xff) (>> len 8) (append else then)))
                  (else (error "need a bigger jump instruction: length is " len)))))
         ;; todo: jit, jat and jrt should have a shared RTL node
         ((jit a type then else)
            (lets
               ((then (assemble then fail))
                (else (assemble else fail))
                (len (length else)))
               (cond
                  ((< len #xffff)
                     (ilist (inst->op 'jit2) (reg a) type (band len #xff) 
                        (>> len 8) (append else then)))
                  (else
                     (error "need a bigger jit instruction: length is " len)))))
         ((jat a type then else)
            (lets
               ((then (assemble then fail))
                (else (assemble else fail))
                (len (length else)))
               (cond
                  ((< len #xffff)
                     (ilist (inst->op 'jat2) (reg a) type (band len #xff) 
                        (>> len 8) (append else then)))
                  ;; if (jat a type2 ...), make (jatq a type jmp jatq  ...)
                  (else
                     (error "need a bigger jat instruction: length is " len)))))
         ((jrt a type then else)
            (lets
               ((then (assemble then fail))
                (else (assemble else fail))
                (len (length else)))
               (cond
                  ((< len #xff) ;; todo: jrt has only a short branch option
                     (ilist (inst->op 'jrt) (reg a) type len (append else then)))
                  (else
                     (error "need a bigger jrt instruction: length is " len)))))
         (else
            (show "assemble: what is " code)
            (fail False))))

   ;; this would be one place where shared (extended)code could be 
   ;; detected, other options being post-compilation search and replace,
   ;; and carrying a bytecode tree along in compilation and looking up 
   ;; results there. none of the options seem especially elegant..

   (define (code->list bcode)
      (map (λ (p) (refb bcode p))
         (iota 0 1 (sizeb bcode))))

   (define is-less False)
   (define is-equal True)
   (define is-greater null)

   (define (compare-bytes a b pos end)
      (if (eq? pos end)
         is-equal
         (let ((ab (refb a pos)) (bb (refb b pos)))
            (cond
               ((eq? ab bb) (compare-bytes a b (+ pos 1) end))
               ((lesser? ab bb) is-less)
               (else is-greater)))))

   ;; shorter is less, otherwase lexical comparison from start
   (define (compare-code a b)
      (lets
         ((as (sizeb a))
          (bs (sizeb b)))
         (cond
            ((eq? as bs) (compare-bytes a b 0 as))
            ((lesser? as bs) is-less)
            (else is-greater))))

   ;; codes bcode value → codes'
   (define (insert-code codes bcode value)
      (if codes
         (ff-bind codes
            (λ (l k v r)
               (let ((res (compare-code k bcode)))
                  (cond
                     ((eq? res is-equal)
                        (mkblack l bcode value r))
                     ((eq? res is-less)
                        (mkblack (insert-code l bcode value) k v r))
                     (else
                        (mkblack l k v (insert-code r bcode value)))))))
         (mkblack False bcode value False)))

    ;; codes bcode → bcode(')
    (define (lookup-code codes bcode)
      (if codes
         (ff-bind codes
            (λ (l k v r)
               (let ((res (compare-code k bcode)))
                  (cond
                     ((eq? res is-equal) v)
                     ((eq? res is-less) (lookup-code l bcode))
                     (else (lookup-code r bcode))))))
         bcode))
 
   ;; lookup equal code from code tree in env, or make a new code fragments and add to tree
   (define (bytes->bytecode env bytes)
      (lets
         ((code (raw bytes 0 False))   ;; <- could be returned as such
          (val (lookup-code env code)))
         (if val
            (begin
               ;(if (eq? (refb val 1) 0)
               ;   (print* (list "S + shared native " val " = " bytes))
               ;   (print* (list "S + shared " val " = " bytes)))
               (values env val)) ;; <- equal, possibly native, code already there
            (begin
               ;(print* (list "S + added " bytes))
               (values (insert-code env code code) code))))) ;; add new code for sharing later

   ; code rtl object -> executable code
   (define (assemble-code env obj)
      (tuple-case obj
         ((code arity insts)
            (lets
               ((insts (allocate-registers insts))
                (insts (count-allocs insts)))
               ;(show "optimized code is " insts)
               (if (not insts)
                  (error "failed to allocate registers" "")
                  (bytes->bytecode env
                     (call/cc
                        (λ (ret)
                           (cons arity (assemble insts ret))))))))))

)
