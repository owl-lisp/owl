;; vm rimops

(define-module lib-primop
   (export 
      primops
      primitive?
      primop-of
      prim-opcodes
      multiple-return-variable-primops
      variable-input-arity?
      special-bind-primop?
      ;; primop wrapper functions
      run 
      set-ticker
      clock 
      sys-prim
      bind
      ff-bind
		mkt
      ;; extra ops
      set-memory-limit get-word-size get-memory-limit start-seccomp
      )

   (define (func lst) (raw lst 0 F))

	;; changing any of the below 3 primops is tricky. they have to be recognized by the primop-of of 
	;; the repl which builds the one in which the new ones will be used, so any change usually takes 
	;; 2 rebuilds. 

	; these 2 primops require special handling, mainly in cps
   (define ff-bind (func '(2 49)))
   (define bind (func '(2 32 4 5 24 5)))
	; this primop is the only one with variable input arity
   (define mkt (func '(4 23 3 4 5 6 7 24 7)))

	;; these rest are easy
   (define car         (func '(2 52 4 5 24 5))) 
   (define cdr         (func '(2 53 4 5 24 5))) 
   (define cons        (func '(3 51 4 5 6 24 6)))
   (define run         (func '(3 50 4 5 6 24 6)))
   (define set-ticker  (func '(2 62 4 5 24 5)))
   (define sys-prim    (func '(5 63 4 5 6 7 8 24 8)))
   (define clock       (func '(1 9 3 5 61 3 4 2 5 2)))
   (define call-native (func '(5 15 4 5 6 7 8 24 8)))
   (define sys         (func '(4 27 4 5 6 7 24 7)))
   (define sizeb       (func '(2 28 4 5 24 5)))
   (define raw         (func '(4 60 4 5 6 7 24 7)))
   (define _connect    (func '(3 34 4 5 6 24 6))) ;; <- remove and add to sys
   (define _sleep      (func '(2 37 4 5 24 5)))   ;; <- move to sys
   (define fxband      (func '(3 55 4 5 6 24 6)))
   (define fxbor       (func '(3 56 4 5 6 24 6)))
   (define fxbxor      (func '(3 57 4 5 6 24 6)))
   (define type        (func '(2 10 4 5 24 5)))
   (define size        (func '(2 36 4 5 24 5)))
   (define cast        (func '(3 22 4 5 6 24 6)))
   (define ref         (func '(3 47 4 5 6 24 6)))
   (define refb        (func '(3 48 4 5 6 24 6)))
   (define ff-toggle   (func '(2 46 4 5 24 5)))

   ;; from cps
   (define (special-bind-primop? op)
      (has? '(32 49) op))

   ;; fixme: handle multiple return value primops sanely (now a list)
   (define multiple-return-variable-primops
      '(49 26 38 39 40 58 59 37 61))

   (define (variable-input-arity? op) (eq? op 23)) ;; mkt

   (define primops-1
      (list
         ;;; input arity includes a continuation
         (tuple 'call-native  15 4 1 call-native) ;; (call-native <code> a1 a2 a3) -> r
         (tuple 'sys          27 4 1 sys)
         (tuple 'sizeb        28 1 1 sizeb)   ;; raw-obj -> numbe of bytes (fixnum)
         (tuple 'raw          60 3 1 raw)   ;; make raw object, and *add padding byte count to type variant*
         (tuple '_connect     34 2 1 _connect)   ;; (connect host port) -> False | socket-fd
         (tuple '_sleep       37 1 1 _sleep)   ;; (_sleep nms) -> True
         (tuple 'cons         51 2 1 cons)
         (tuple 'car          52 1 1 car)
         (tuple 'cdr          53 1 1 cdr)
         (tuple 'eq?          54 2 1 eq?)
         (tuple 'fxband       55 2 1 fxband)
         (tuple 'fxbor        56 2 1 fxbor)
         (tuple 'fxbxor       57 2 1 fxbxor)
         (tuple 'type         10 1 1 type)  ;;  get all the low the type bits
         (tuple 'size         36 1 1 size)  ;;  get object size (- 1)
         (tuple 'cast         22 2 1 cast)  ;; cast object type (works for immediates and allocated)
         (tuple 'ref          47 2 1 ref)   ;;
         (tuple 'refb         48 2 1 refb)      ;;
         (tuple 'mkt          23 'any 1 mkt)   ;; mkt type v0 .. vn t
         (tuple 'ff-toggle    46 1 1 ff-toggle)))  ;; (fftoggle node) -> node', toggle redness

   (define set (func '(4 45 4 5 6 7 24 7)))
   (define lesser? (func '(3 44 4 5 6 24 6)))
   (define listuple (func '(4 35 4 5 6 7 24 7)))
   (define mkblack (func '(5 42 4 5 6 7 8 24 8)))
   (define mkred (func '(5 43 4 5 6 7 8 24 8)))
   (define red? (func '(2 41 4 5 24 5)))
   (define fxqr (func '(4 26))) ;; <- placeholder 
   (define fx+ (func '(4 38 4 5 6 7 24 7)))
   (define fx- (func '(4 40 4 5 6 7 24 7)))
   (define fx>> (func '(4 58 4 5 6 7 24 7)))
   (define fx<< (func '(4 59 4 5 6 7 24 7)))

   (define primops-2
		(list
			(tuple 'bind         32 1 F bind)  ;; (bind thing (lambda (name ...) body)), fn is at CONT so arity is really 1
			(tuple 'set          45 3 1 set)   ;; (set tuple pos val) -> tuple'
			(tuple 'lesser?      44 2 1 lesser?)  ;; (lesser? a b)
			(tuple 'listuple     35 3 1 listuple)  ;; (listuple type size lst)
			(tuple 'mkblack      42 4 1 mkblack)   ; (mkblack l k v r)
			(tuple 'mkred        43 4 1 mkred)   ; ditto
			(tuple 'ff-bind      49 1 F ff-bind)  ;; SPECIAL ** (ffbind thing (lambda (name ...) body)) 
			(tuple 'red?         41 1 F red?)  ;; (red? node) -> bool
			(tuple 'fxqr         26 3 3 'fxqr)   ;; (fxdiv ah al b) -> qh ql r
			(tuple 'fx+          38 2 2 fx+)   ;; (fx+ a b)      ;; 2 out 
			(tuple 'fx*          39 2 2 fx*)   ;; (fx* a b)      ;; 2 out
			(tuple 'ncons        29 2 1 ncons)   ;;
			(tuple 'ncar         30 1 1 ncar)   ;;
			(tuple 'ncdr         31 1 1 ncdr)   ;;
			(tuple 'fx-          40 2 2 fx-)   ;; (fx- a b)       ;; 2 out
			(tuple 'fx>>         58 2 2 fx>>)   ;; (fx>> a b) -> hi lo, lo are the lost bits
			(tuple 'fx<<         59 2 2 fx<<)   ;; (fx<< a b) -> hi lo, hi is the overflow
			(tuple 'clock        61 0 2 clock) ; (clock) → posix-time x ms
			(tuple 'set-ticker   62 1 1 set-ticker)
			(tuple 'sys-prim     63 4 1 sys-prim)))

   (define primops 
      (append primops-1 primops-2))

   ;; ff of wrapper-fn → opcode
   (define prim-opcodes
      (for False primops
         (λ (ff node)
            (put ff (ref node 5) (ref node 2)))))

   ;; ff of opcode → wrapper
   (define opcode->wrapper
      (for False primops
         (λ (ff node)
            (put ff (ref node 2) (ref node 5)))))

   ;; later check type, get first opcode and compare to primop wrapper 
   (define (primop-of val)
		(cond
			((get prim-opcodes val False) => (lambda (op) op))
			((equal? val mkt) 23)
			((equal? val bind) 32)
			((equal? val ff-bind) 49)
			(else False)))
		
   (define primitive? primop-of)

   ;; special things exposed by the vm
   (define (set-memory-limit n) (sys-prim 7 n n n))
   (define (get-word-size) (sys-prim 8 F F F))
   (define (get-memory-limit) (sys-prim 9 F F F))
   (define (start-seccomp) (sys-prim 10 F F F)) ; not enabled by defa

)
