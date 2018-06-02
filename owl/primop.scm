;; VM primops

(define-library (owl primop)
   (export
      primops
      primop-name ;; primop → symbol | primop
      special-bind-primop?
      ;; primop wrapper functions
      run
      set-ticker
      bind
      mkt
      halt
      wait
      ;; extra ops
      set-memory-limit get-word-size get-memory-limit

      apply
      call/cc
      lets/cc
      create-type
      len
      )

   (import
      (owl defmac))

   (begin

      (define bytes->bytecode
         (C raw type-bytecode))

      (define (app a b)
         (if (eq? a '())
            b
            (cons (car a) (app (cdr a) b))))

      ;; l -> fixnum | #false if too long
      (define (len l)
         (let loop ((l l) (n 0))
            (if (eq? l '())
               n
               (lets ((n o (fx+ n 1)))
                  (if o #false (loop (cdr l) n))))))

      (define (func lst)
         (lets
            ((arity (car lst))
             (lst (cdr lst))
             (len (len lst)))
            (bytes->bytecode
               (ilist 34 arity 0 len
                  (app lst (list 17)))))) ;; fail if arity mismatch

      ;; changing any of the below 3 primops is tricky. they have to be recognized by the primop-of of
      ;; the repl which builds the one in which the new ones will be used, so any change usually takes
      ;; 2 rebuilds.

      ; these 2 primops require special handling, mainly in cps
      (define ff-bind ;; turn to badinst soon, possibly return later
         ; (func '(2 49))
         '__ff-bind__

         )
      (define bind
         ; (func '(2 32 4 5 24 5))
         '__bind__
         )
      ; this primop is the only one with variable input arity
      (define mkt
         '__mkt__
         ;(func '(4 23 3 4 5 6 7 24 7))
         )

      ;; these rest are easy
      (define car         (func '(2 105 4 5 24 5)))
      (define cdr         (func '(2 169 4 5 24 5)))
      (define cons        (func '(3 51 4 5 6 24 6)))
      (define run         (func '(3 50 4 5 6 24 6)))
      (define set-ticker  (func '(2 62 4 5 24 5)))
      (define sys-prim    (func '(5 63 4 5 6 7 8 24 8)))
      (define sys         (func '(4 27 4 5 6 7 24 7)))
      (define sizeb       (func '(2 28 4 5 24 5)))
      (define raw         (func '(3 37 4 5 6 24 6)))
      (define eq?         (func '(3 54 4 5 6 24 6)))
      (define fxband      (func '(3 55 4 5 6 24 6)))
      (define fxbor       (func '(3 56 4 5 6 24 6)))
      (define fxbxor      (func '(3 57 4 5 6 24 6)))

      (define type        (func '(2 15 4 5 24 5)))
      (define size        (func '(2 36 4 5 24 5)))
      (define ref         (func '(3 47 4 5 6 24 6)))

      ;; make thread sleep for a few thread scheduler rounds
      (define (wait n)
         (if (eq? n 0)
            n
            (lets ((n _ (fx- n 1)))
               (set-ticker 0) ;; allow other threads to run
               (wait n))))

      ;; from cps
      (define (special-bind-primop? op)
         (or (eq? op 32) (eq? op 49)))

      (define primops-1
         (list
            ;;; input arity includes a continuation
            (tuple 'sys          27 4 1 sys)
            (tuple 'sizeb        28 1 1 sizeb)   ;; raw-obj -> numbe of bytes (fixnum)
            (tuple 'raw          37 2 1 raw)   ;; make raw object, and *add padding byte count to type variant*
            (tuple 'cons         51 2 1 cons)
            (tuple 'car         105 1 1 car) ;; opcode: 1 << 6 | 41
            (tuple 'cdr         169 1 1 cdr) ;; opcode: 2 << 6 | 41
            (tuple 'eq?          54 2 1 eq?)
            (tuple 'fxband       55 2 1 fxband)
            (tuple 'fxbor        56 2 1 fxbor)
            (tuple 'fxbxor       57 2 1 fxbxor)
            (tuple 'type         15 1 1 type)
            (tuple 'size         36 1 1 size)  ;;  get object size (- 1)
            (tuple 'ref          47 2 1 ref)   ;;
            (tuple 'mkt          23 'any 1 mkt))) ;; mkt type v0 .. vn t

      (define set (func '(4 45 4 5 6 7 24 7)))
      (define lesser? (func '(3 44 4 5 6 24 6)))
      (define listuple (func '(4 35 4 5 6 7 24 7)))
      (define mkblack (func '(5 48 4 5 6 7 8 24 8)))
      (define mkred (func '(5 176 4 5 6 7 8 24 8)))
      (define fxqr (func '(4 26))) ;; <- placeholder
      (define fx+ (func '(4 38 4 5 6 7 24 7)))
      (define fx- (func '(4 40 4 5 6 7 24 7)))
      (define fx>> (func '(4 58 4 5 6 7 24 7)))

      (define apply (bytes->bytecode '(125))) ;; <- no arity, just call 64 | 61
      (define apply-cont (bytes->bytecode '(61)))

      (define primops-2
         (list
            (tuple 'bind         32 1 #false bind)  ;; (bind thing (lambda (name ...) body)), fn is at CONT so arity is really 1
            (tuple 'set          45 3 1 set)   ;; (set tuple pos val) -> tuple'
            (tuple 'lesser?      44 2 1 lesser?)  ;; (lesser? a b)
            (tuple 'listuple     35 3 1 listuple)  ;; (listuple type size lst)
            (tuple 'mkblack      48 4 1 mkblack) ;; (mkblack l k v r)
            (tuple 'mkred       176 4 1 mkred)   ;; ditto, opcode: FFRED << 6 | 48
            (tuple 'ff-bind      49 1 #false ff-bind)  ;; SPECIAL ** (ffbind thing (lambda (name ...) body))
            (tuple 'fxqr         26 3 3 'fxqr)   ;; (fxdiv ah al b) -> qh ql r
            (tuple 'fx+          38 2 2 fx+)   ;; (fx+ a b)      ;; 2 out
            (tuple 'fx*          39 2 2 fx*)   ;; (fx* a b)      ;; 2 out
            (tuple 'fx-          40 2 2 fx-)   ;; (fx- a b)       ;; 2 out
            (tuple 'fx>>         58 2 2 fx>>)   ;; (fx>> a b) -> hi lo, lo are the lost bits
            (tuple 'set-ticker   62 1 1 set-ticker)
            (tuple 'sys-prim     63 4 1 sys-prim)))

      (define primops
         (app primops-1
              primops-2))

      ;; special things exposed by the vm
      (define (set-memory-limit n) (sys-prim 7 n #f #f))
      (define (get-word-size) (sys-prim 8 1 #f #f))
      (define (get-memory-limit) (sys-prim 9 #f #f #f))

      ;; todo: add get-heap-metrics

      ;; stop the vm *immediately* without flushing input or anything else with return value n
      (define (halt n) (sys-prim 6 n #f #f))

      ;; a minimal definition would be this, but we also want variable arities here
      ; (define call/cc  ('_sans_cps (λ (k f) (f k (λ (r a) (k a))))))

      (define call/cc
         ('_sans_cps
            (λ (k f)
               (f k
                  (case-lambda
                     ((c a) (k a))
                     ((c a b) (k a b))
                     ((c . x) (apply-cont k x)))))))

      (define-syntax lets/cc
         (syntax-rules (call/cc)
            ((lets/cc (om . nom) . fail)
               (syntax-error "let/cc: continuation name cannot be " (quote (om . nom))))
            ((lets/cc var . body)
               (call/cc (λ (var) (lets . body))))))

      (define create-type
         (let ((get-header (raw '(1 4 0 5 24 5) type-bytecode)))
            (λ (type)
               (let ((hdr (get-header (raw '() type))))
                  (fxbxor hdr hdr)))))

      ;; non-primop instructions that can report errors
      (define (instruction-name op)
         (cond
            ((eq? op 17) 'arity-error)
            ((eq? op 32) 'bind)
            ((eq? op 50) 'run)
            (else #false)))

      (define (primop-name pop)
         (let ((pop (fxband pop 63))) ; ignore top bits which sometimes have further data
            (or (instruction-name pop)
               (let loop ((primops primops))
                  (cond
                     ((null? primops) pop)
                     ((eq? pop (ref (car primops) 2))
                        (ref (car primops) 1))
                     (else
                        (loop (cdr primops))))))))
))
