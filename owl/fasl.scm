;;; This library implements serialization of objects to byte 
;;; lists, and parsing of the byte lists to corresponding 
;;; objects. The format is used internally for storing memory 
;;; images to disk. Files with .fasl suffix used in booting 
;;; up Owl are just fasl-encoded functions.
;;;
;;; ``` 
;;;   (fasl-encode 42) → '(0 0 42)
;;;   (fasl-encode 42) → '(0 0 42)
;;;   (fasl-encode 1/4+i) → '(1 42 2 0 0 1 0 0 4 1 43 2 1 0 0 1 0)
;;;   (fasl-encode (lambda (x) x)) →  '(2 16 7 34 2 0 2 24 4 17 0)
;;;   (fasl-decode '(0 0 0 0) 'bad) → 'bad
;;;   ((fasl-decode (fasl-encode prime?) 'bad) 13337) → #true
;;;   (eq? 'foo (fasl-decode (fasl-encode 'foo) #false)) → #true
;;; ``` 

; protocol
;	<obj> = 0 <type> <value> 				-- immediate object
;			= 1 <type> <size> <field> ... -- allocated
;			= 2 <type> <size> <byte>  ... -- allocated, raw data
; now used
;		00 - imm
;		01 - alloc
;		10 - alloc raw
;		11 - free -> use as tag for allocs where the type fits 6 bits (not in use atm)
;	
;	<field> = 0 <type> <val> -- immediate
;			  = <N> -- pointer to nth last object (see hack warning below)

(define-library (owl fasl)

   (export 
      fasl-encode          ; ; obj -> (byte ... 0)
      fasl-encode-cooked   ; obj cook -> (byte ... 0), with (cook alloc-obj) -> alloc-obj' 
      fasl-encode-stream   ; obj cook -> (bvec ...) stream
      fasl-decode          ; (byte ...) -> obj, input can be lazy
      decode-or            ; (byte ...) fail → object | (fail reason)
      encode               ; obj -> (byte ... 0), n-alloc-objs (mainly for bootstrapping)
      tuple->list          ; ; TEMPORARILY HERE
      objects-below        ; obj -> (obj ...), all allocated objects below obj
      decode-stream        ; ll failval → (ob ...) | (ob .. failval)
      sub-objects          ; obj wanted? -> ((obj . n-occurrences) ...)
      )

   (import
      (owl defmac)
      (owl vector)
      (owl math)
      (owl primop)
      (owl ff)
      (owl symbol)
      (owl lazy)
      (only (owl syscall) error)
      (owl proof)
      (owl list)
      (owl intern)
      (owl rlist))

   (begin

      (define enodata #false) ;; reason to fail if out of data (progressive readers want this)

      (define (read-tuple tuple pos lst)
         (if (= pos 0)
            lst
            (read-tuple tuple (- pos 1)
               (cons (ref tuple pos) lst))))

      (define (tuple->list tuple)
         (read-tuple tuple (size tuple) null))

      ;;;
      ;;; Encoder
      ;;;

      (define low7 #b1111111)

      (define (send-biggish-num num done)
         (if (< num 127)
            (cons (+ num 128) done)
            (send-biggish-num (>> num 7)
               (cons (+ 128 (band num low7)) done))))

      (define (send-number num tail)
         (if (< num 128)
            (cons num tail)
            (send-biggish-num (>> num 7)
               (cons (band num #b01111111) tail))))

      (define type-byte-of type)
         ;(type-byte-of val)
         ;(fxband (>> (type-old val) 3) 255)

      (define (enc-immediate val tail)
         (cons 0
            (cons (type-byte-of val)
               (send-number (cast val 0) tail))))

      (define (partial-object-closure seen obj)
         (cond
            ((immediate? obj) seen)
            ((getf seen obj) =>
               (λ (n) (fupd seen obj (+ n 1))))
            (else
               (let ((seen (put seen obj 1)))
                  (if (raw? obj)
                     seen
                     (fold partial-object-closure seen 
                        (tuple->list obj)))))))

      (define (sub-objects root pred)
         (ff->list
            (partial-object-closure empty root)))

      (define (object-closure obj)
         (partial-object-closure empty obj))

      (define (objects-below obj)	
         (ff-fold
            (λ (out obj _) (cons obj out))
            null (object-closure obj)))

      (define (index-closure clos) ; carry (fp . clos)
         (cdr
            (ff-fold
               (λ (fc val _)
                  (lets ((fp clos fc))
                     (cons (+ fp 1) (ff-update clos val fp))))
               (cons 0 clos) clos)))

      (define (render-field clos pos)
         (λ (elem out)
            (if (immediate? elem)
               (enc-immediate elem out)
               (let ((target (get clos elem "bug")))
                  ; hack warning: objects cannot refer to themselves and the 
                  ; heap is unidirectional, so pos - target > 0, so a 0 can 
                  ; be safely used as the immediate marker, and pointers can 
                  ; have the delta directly encoded as a natural, which always
                  ; starts with a nonzero byte when the natural > 0
                  (send-number (- pos target) out)))))

      (define (render-fields out lst pos clos)
         (foldr (render-field clos pos) out lst))

      (define (copy-bytes out vec p)
         (if (eq? p -1)
            out
            (copy-bytes (cons (refb vec p) out) vec (- p 1))))

      ;; todo - pack type to this now that they fit 6 bits
      (define (encode-allocated clos cook)
         (λ (out val-orig pos)
            (lets
               (;(val-orig (if (eq? val-orig <OLD>) <NEW> val-orig))  ; <- for heap search/replace
                (val (cook val-orig)))
               (if (raw? val)
                  (lets
                     ;; nuke padding bytes since the vm/decoder must fill these while loading
                     ;; (because different word size may require more/less padding)
                     ((t (fxband (type-byte-of val) #b11111))
                      (bs (sizeb val)))
                     (ilist 2 t
                        (send-number bs
                           (copy-bytes out val (- bs 1)))))
                  (lets
                     ((t (type-byte-of val))
                      (s (size val)))
                     ; options for optimization
                     ;	t and s fit in 6 bits -> pack (seems to be only about 1/20 compression)
                     ;	t fits in 6 bits -> (+ (<< t 2) 3) (ditto)
                     (ilist 1 t
                        (send-number s
                           (render-fields out (tuple->list val) pos clos))))))))

      (define fasl-finale (list 0)) ; stream end marker

      ;; produce tail-first eagerly
      ;(define (encoder-output clos cook)
      ;	(ff-foldr (encode-allocated clos cook) fasl-finale clos))

      (define (encoder-output clos cook)
         (let ((enc (encode-allocated clos cook)))
            (let loop ((kvs (ff-iter clos)))
               (cond
                  ((null? kvs) fasl-finale)
                  ((pair? kvs)
                     (lets ((kv (car kvs)))
                        (enc (lambda () (loop (cdr kvs))) (car kv) (cdr kv))))
                  (else (loop (kvs)))))))

      ; root cook-fn -> byte-stream
      (define (encoder obj cook)
         (encoder-output
            (index-closure
               (object-closure obj))
            cook))

      ; -> byte stream
      (define (encode obj cook)
         (if (allocated? obj)
            (encoder obj cook)
            (enc-immediate obj null)))

      ; dump the data, but cook each allocated value with cook just before dumping
      ; (to allow for example changing code from functions without causing
      ;  them to move in the heap, which would break object order)
      (define (fasl-encode-cooked obj cook)
         (force-ll (encode obj cook)))

      ; dump the data as such
      (define (fasl-encode obj)
         (force-ll (encode obj self)))

      (define chunk-size 32767)

      (define (chunk-stream bs n buff)
         (cond
            ((eq? n chunk-size)
               (cons 
                  (list->byte-vector (reverse buff))
                  (chunk-stream bs 0 null)))
            ((null? bs)
               (if (null? buff)
                  null
                  (list (list->byte-vector (reverse buff)))))
            ((pair? bs)
               (lets ((n _ (fx+ n 1)))
                  (chunk-stream (cdr bs) n (cons (car bs) buff))))
            (else
               (chunk-stream (bs) n buff))))

      (define (fasl-encode-stream obj cook)
         (chunk-stream (encode obj cook) 0 null))

      ;;; 
      ;;; Decoder
      ;;;

      (define (grab ll fail)
         (cond
            ((null? ll) (fail enodata))
            ((pair? ll) (values (cdr ll) (car ll)))
            (else (grab (ll) fail))))

      (define (get-nat ll fail top)
         (lets ((ll b (grab ll fail)))
            (if (eq? 0 (fxband b 128)) ; leaf case
               (values ll (bor (<< top 7) b))
               (get-nat ll fail (bor (<< top 7) (band b low7))))))

      (define (decode-immediate ll fail)
         (lets 
            ((ll type (grab ll fail))
             (ll val  (get-nat ll fail 0)))
            (values ll (cast val type))))

      (define nan "not here") ; eq?-unique

      (define (get-fields ll got size fail out)
         (if (eq? size 0)
            (values ll (reverse out))
            (lets ((ll fst (grab ll fail)))
               (if (eq? fst 0)
                  (lets ((ll val (decode-immediate ll fail)))
                     (get-fields ll got (- size 1) fail (cons val out)))
                  (lets
                     ((ll pos (get-nat (cons fst ll) fail 0)))
                     (if (eq? pos 0)
                        (fail "bad reference")
                        (let ((val (rget got (- pos 1) nan)))
                           (if (eq? val nan)
                              (fail "bad reference")
                              (get-fields ll got (- size 1) fail (cons val out))))))))))

      (define (get-bytes ll n fail out)
         (if (eq? n 0)
            (values ll out)
            (lets ((ll byte (grab ll fail)))
               (get-bytes ll (- n 1) fail (cons byte out)))))

      (define (ff-union a b)
         (ff-fold put b a))

      ; → ll value | (fail reason)
      (define (decoder ll got fail)
         (cond
            ((null? ll)
               ;; no terminal 0, treat as a bug
               (fail "no terminal zero"))
            ((pair? ll)
               (lets ((kind ll ll))
                  (cond
                     ((eq? kind 1) ; allocated, type SIZE
                        (lets
                           ((ll type (grab ll fail))
                            (ll size (get-nat ll fail 0))
                            (ll fields (get-fields ll got size fail null))
                            (obj (listuple type size fields)))
                           ;; could just rcons obj to got, but some thigns are special when
                           ;; doing just partial heap transfers
                           (cond
                              ((symbol? obj)
                                 ;; symbols must be (re)interned. they are only valid up to equalit within the fasl.
                                 (decoder ll 
                                    (rcons
                                       (string->symbol (symbol->string obj))
                                       got)
                                    fail))
                              ((ff? obj)
                                 ;; WARNING: this is expensive! but correct and used for now
                                 (ff-bind obj
                                    (λ (l k v r)
                                       (decoder ll
                                          (rcons (ff-union l (put r k v)) got)
                                          fail))))
                              (else
                                 (decoder ll (rcons obj got) fail)))))
                     ((eq? kind 2) ; raw, type SIZE byte ...
                        (lets
                           ((ll type (grab ll fail))
                            (ll size (get-nat ll fail 0))
                            (foo (if (> size 65535) (fail "bad raw object size")))
                            (ll rbytes (get-bytes ll size fail null))
                            (obj (raw (reverse rbytes) type)))
                           (decoder ll (rcons obj got) fail)))
                     ((eq? kind 0) ;; fasl stream end marker 
                        ;; object done
                        (values ll (rcar got)))
                     ((eq? (band kind 3) 3) ; shortcut allocated
                        (lets
                           ((type (>> kind 2))
                            (ll size (get-nat ll fail 0))
                            (foo (if (> size 65535) (fail "bad raw object size")))
                            (ll rbytes (get-bytes ll size fail null))
                            (obj (raw (reverse rbytes) type)))
                           (decoder ll (rcons obj got) fail)))
                     (else
                        (fail (list "unknown object tag: " kind))))))
            (else
               (decoder (ll) got fail))))

      (define (decode-or ll err) ; -> ll obj | null (err why)
         (call/cc ; setjmp2000
            (λ (ret)
               (lets ((fail (λ (why) (ret null (err why)))))
                  (cond
                     ((null? ll) (fail enodata))
                     ((pair? ll)
                        ; a leading 0 is special and means the stream has no allocated objects, just one immediate one
                        (if (eq? 0 (car ll)) 
                           (decode-immediate (cdr ll) fail)
                           (decoder ll null fail)))
                     (else (decode-or (ll) err)))))))

      ;; decode a full (possibly lazy) list of data, and succeed only if it exactly matches a fasl-encoded object

      (define failed "fail") ;; a unique object

      ;; ll fail → val | fail
      (define (decode ll fail-val)
         (lets ((ll ob (decode-or ll (λ (why) failed))))
            (cond
               ((eq? ob failed) fail-val)
               ((null? (force-ll ll)) ob)
               (else fail-val))))

      ;; byte-stream → (ob ...) | (ob ... err)
      (define (decode-stream ll err)
         (cond
            ((pair? ll)
               (lets ((ll ob (decode-or ll (λ (why) failed))))
                  (if (eq? ob failed)
                     (list err)
                     (pair ob (decode-stream ll err)))))
            ((null? ll) null)
            (else (decode-stream (ll) err))))

      (define fasl-decode decode)

      (example
         (fasl-decode '() 'x) = 'x
         (fasl-decode (fasl-encode 42) 'x) = 42
         (fasl-decode (fasl-encode '(1 2)) 'x) = '(1 2))
))
