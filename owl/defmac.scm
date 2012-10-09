  
(define-library (owl defmac)

   (export
      λ syntax-error begin 
      quasiquote letrec let if 
      letrec* let*-values
      cond case define define*
      lets let* or and list
      ilist tuple tuple-case type-case 
      call-with-values do define-library
      case-lambda
      define-values
      not o i self

      immediate? allocated? raw?

      type-bytecode
      type-proc
      type-clos
      type-fix+
      type-fix-
      type-pair
      type-vector-dispatch
      type-vector-leaf
      type-vector-raw
      type-ff-black-leaf
      type-eof
      type-tuple
      type-symbol
      type-const
      type-rlist-spine
      type-rlist-node
      type-port 
      type-socket
      type-tcp-client
      type-ff               ;; k v, k v l or i v l r
      type-ff-right         ;; k v r
   )

   (begin

      (define-syntax λ 
         (syntax-rules () 
            ((λ . x) (lambda . x))))

      (define-syntax syntax-error
         (syntax-rules (error)
            ((syntax-error . stuff)
               (error "Syntax error: " (quote stuff)))))

      ;; expand case-lambda syntax to to (_case-lambda <lambda> (_case-lambda ... (_case-lambda <lambda> <lambda)))
      (define-syntax case-lambda
         (syntax-rules (lambda _case-lambda)
            ((case-lambda) #false) 
            ; ^ should use syntax-error instead, but not yet sure if this will be used before error is defined
            ((case-lambda (formals . body))
               ;; downgrade to a run-of-the-mill lambda
               (lambda formals . body))
            ((case-lambda (formals . body) . rest)
               ;; make a list of options to be compiled to a chain of code bodies w/ jumps
               ;; note, could also merge to a jump table + sequence of codes, but it doesn't really matter
               ;; because speed-sensitive stuff will be compiled to C where this won't matter
               (_case-lambda (lambda formals . body)
                  (case-lambda . rest)))))

      ;; note, no let-values yet, so using let*-values in define-values
      (define-syntax begin
         (syntax-rules (define define-syntax letrec define-values let*-values)
            ;((begin
            ;   (define-syntax key1 rules1)
            ;   (define-syntax key2 rules2) ... . rest)
            ;   (letrec-syntax ((key1 rules1) (key2 rules2) ...)
            ;      (begin . rest)))
            ((begin exp) exp)
            ((begin (define . a) (define . b) ... . rest)
               (begin 42 () (define . a) (define . b) ... . rest))
            ((begin (define-values (val ...) . body) . rest)
               (let*-values (((val ...) (begin . body))) . rest))
            ((begin 42 done (define ((op . args1) . args2) . body) . rest)
               (begin 42 done (define (op . args1) (lambda args2 . body)) . rest))
            ((begin 42 done (define (var . args) . body) . rest)
               (begin 42 done (define var (lambda args . body)) . rest))
            ((begin 42 done (define var exp1 exp2 . expn) . rest)
               (begin 42 done (define var (begin exp1 exp2 . expn)) . rest))
            ((begin 42 done (define var val) . rest)
               (begin 42 ((var val) . done) . rest))
            ((begin 42 done . exps)
               (begin 43 done () exps))
            ((begin 43 (a . b) c exps)
               (begin 43 b (a . c) exps))
            ((begin 43 () bindings exps)
               (letrec bindings (begin . exps)))
            ((begin first . rest)  
               ((lambda (free)
                  (begin . rest))
                  first))))

      (define-syntax letrec
         (syntax-rules (rlambda)
            ((letrec ((?var ?val) ...) ?body) (rlambda (?var ...) (?val ...) ?body))
            ((letrec vars body ...) (letrec vars (begin body ...)))))

      (define-syntax letrec*
         (syntax-rules ()
            ((letrec () . body)
               (begin . body))
            ((letrec* ((var val) . rest) . body)
               (letrec ((var val))
                  (letrec* rest . body)))))

      (define-syntax let
            (syntax-rules ()
               ((let ((var val) ...) exp . rest) 
                  ((lambda (var ...) exp . rest) val ...))
               ((let keyword ((var init) ...) exp . rest) 
                  (letrec ((keyword (lambda (var ...) exp . rest))) (keyword init ...)))))

      ; Temporary hack: if inlines some predicates.

      (define-syntax if
         (syntax-rules 
            (not eq? and null? pair? teq? imm alloc raw
               fix+ fix- int+ int- pair rat comp)
            ((if test exp) (if test exp #false))
            ((if (not test) then else) (if test else then))
            ((if (null? test) then else) (if (eq? test '()) then else))
            ((if (pair? test) then else) (if (teq? test (alloc 1)) then else))
            ((if (teq? q fix+) . c) (if (teq? q (imm    0)) . c))
            ((if (teq? q fix-) . c) (if (teq? q (imm   32)) . c))
            ((if (teq? q int+) . c) (if (teq? q (alloc  9)) . c))      ; num base type
            ((if (teq? q int-) . c) (if (teq? q (alloc 41)) . c))      ; num/1
            ((if (teq? q pair) . c) (if (teq? q (alloc  1)) . c))      
            ((if (teq? q rat) . c)  (if (teq? q (alloc 73)) . c))      ; num/2
            ((if (teq? q comp) . c)  (if (teq? q (alloc 105)) . c))   ; num/3
            ((if (teq? (a . b) c) then else) 
               (let ((foo (a . b)))
                  (if (teq? foo c) then else)))
            ((if (teq? a (imm b)) then else) (_branch 1 a b then else))   
            ((if (teq? a (alloc b)) then else) (_branch 2 a b then else))
            ((if (teq? a (raw b)) then else) (_branch 3 a b then else))
            ((if (eq? a b) then else) (_branch 0 a b then else))            
            ((if (a . b) then else) (let ((x (a . b))) (if x then else)))
            ((if (teq? a b) then else) (teq? a b then else))
            ;((if (eq? a a) then else) then) ; <- could be functions calls and become non-eq?
            ((if #false then else) else)
            ((if #true then else) then)
            ((if test then else) (_branch 0 test #false else then))))

      (define-syntax cond
         (syntax-rules (else =>)
            ((cond) #false)
            ((cond (else exp . rest))
               (begin exp . rest))
            ((cond (clause => exp) . rest) 
               (let ((fresh clause))
                  (if fresh
                     (exp fresh)
                     (cond . rest))))
            ((cond (clause exp . rest-exps) . rest) 
               (if clause
                  (begin exp . rest-exps)
                  (cond . rest)))))

      (define-syntax case
         (syntax-rules (else eqv? memv =>)
            ((case (op . args) . clauses)
               (let ((fresh (op . args)))
                  (case fresh . clauses)))
            ((case thing) #false)
            ((case thing ((a) => exp) . clauses)
               (if (eqv? thing (quote a))
                  (exp thing)
                  (case thing . clauses)))
            ((case thing ((a ...) => exp) . clauses)
               (if (memv thing (quote (a ...)))
                  (exp thing)
                  (case thing . clauses)))
            ((case thing ((a) . body) . clauses)
               (if (eqv? thing (quote a))
                  (begin . body)
                  (case thing . clauses)))
            ((case thing (else => func))
               (func thing))
            ((case thing (else . body))
               (begin . body))
            ((case thing ((a . b) . body) . clauses)
               (if (memv thing (quote (a . b)))
                  (begin . body)
                  (case thing . clauses)))
            ((case thing (atom exp) . clauses) ;; added for (case (type foo) (type-foo thenfoo) (type-bar thenbar) ...)
               (if (eq? thing atom)
                  exp
                  (case thing . clauses)))))

      (define-syntax define
         (syntax-rules (lambda λ)
            ((define op a b . c)
               (define op (begin a b . c)))
            ((define ((op . args) . more) . body)
               (define (op . args) (lambda more . body)))
            ((define (op . args) body)
               (define op
                  (letrec ((op (lambda args body))) op)))
            ((define name (lambda (var ...) . body))
               (_define name (rlambda (name) ((lambda (var ...) . body)) name)))
            ((define name (λ (var ...) . body))
               (_define name (rlambda (name) ((lambda (var ...) . body)) name)))
            ((define op val)
               (_define op val))))

      ;; not defining directly because rlambda doesn't yet do variable arity
      ;(define list ((lambda (x) x) (lambda x x)))

      ;; fixme, should use a print-limited variant for debugging

      (define-syntax define*
         (syntax-rules (show list)
            ((define* (op . args) . body)
               (define (op . args) 
                  (print " * " (list (quote op) . args))
                  .  body))
            ((define* name (lambda (arg ...) . body))
               (define* (name arg ...) . body))))

      (define-syntax lets
         (syntax-rules (<=)
            ((lets (((var ...) gen) . rest) . body)
               (receive gen (lambda (var ...) (lets rest . body))))
            ((lets ((var val) . rest-bindings) exp . rest-exps)
               ((lambda (var) (lets rest-bindings exp . rest-exps)) val))
            ((lets ((var ... (op . args)) . rest-bindings) exp . rest-exps)
               (receive (op . args)
                  (lambda (var ...) 
                     (lets rest-bindings exp . rest-exps))))
            ((lets ((var ... node) . rest-bindings) exp . rest-exps)
               (bind node
                  (lambda (var ...) 
                     (lets rest-bindings exp . rest-exps))))
            ((lets (((name ...) <= value) . rest) . code)
               (bind value
                  (lambda (name ...)
                     (lets rest . code))))
            ((lets ()) exp)
            ((lets () exp . rest) (begin exp . rest))))

      ;; the internal one is handled by begin. this is just for toplevel.
      (define-syntax define-values
         (syntax-rules (list)
            ((define-values (val ...) . body)
               (_define (val ...)
                  (lets ((val ... (begin . body)))
                     (list val ...))))))

      (define-syntax let*-values
         (syntax-rules ()
            ((let*-values (((var ...) gen) . rest) . body)
               (receive gen
                  (λ (var ...) (let*-values rest . body))))
            ((let*-values () . rest)
               (begin . rest))))
               
      ; i hate special characters, especially in such common operations.
      ; lets (let sequence) is way prettier and a bit more descriptive 

      (define-syntax let*
         (syntax-rules ()
            ((let* . stuff) (lets . stuff))))

      (define-syntax or
         (syntax-rules ()
            ((or) #false)
            ((or (a . b) . c)
               (let ((x (a . b)))
                  (or x . c)))
            ((or a . b)
               (if a a (or . b)))))

      (define-syntax and
         (syntax-rules ()
            ((and) #true)
            ((and a) a)
            ((and a . b)
               (if a (and . b) #false))))

      ;; now a function
      (define-syntax list
         (syntax-rules ()
            ((list) '())
            ((list a . b)
               (cons a (list . b)))))

      (define-syntax quasiquote
         (syntax-rules (unquote quote unquote-splicing append _work _sharp_vector list->vector)
                                                   ;          ^         ^
                                                   ;          '-- mine  '-- added by the parser for #(... (a . b) ...) -> (_sharp_vector ... )
            ((quasiquote _work () (unquote exp)) exp)
            ((quasiquote _work (a . b) (unquote exp))
               (list 'unquote (quasiquote _work b exp)))
            ((quasiquote _work d (quasiquote . e))
               (list 'quasiquote
                  (quasiquote _work (() . d) . e)))
            ((quasiquote _work () ((unquote-splicing exp) . tl))
               (append exp
                  (quasiquote _work () tl)))
            ((quasiquote _work () (_sharp_vector . es))
               (list->vector
                  (quasiquote _work () es)))
            ((quasiquote _work d (a . b))  
               (cons (quasiquote _work d a) 
                     (quasiquote _work d b)))
            ((quasiquote _work d atom)
               (quote atom))
            ((quasiquote . stuff)
               (quasiquote _work () . stuff))))

      (define-syntax ilist
         (syntax-rules ()
            ((ilist a) a)
            ((ilist a . b)
               (cons a (ilist . b)))))

      (define-syntax tuple
         (syntax-rules ()
            ((tuple a . bs) ;; there are no such things as 0-tuples
               (mkt 2 a . bs))))

      ; replace this with typed destructuring compare later on 

      (define-syntax tuple-case
         (syntax-rules (else _ is eq? bind div)
            ((tuple-case (op . args) . rest)
               (let ((foo (op . args)))
                  (tuple-case foo . rest)))
            ;;; bind if the first value (literal) matches first of pattern
            ((tuple-case 42 tuple type ((this . vars) . body) . others)
               (if (eq? type (quote this))
                  (bind tuple
                     (lambda (ignore . vars) . body))
                  (tuple-case 42 tuple type . others)))
            ;;; bind to anything
            ((tuple-case 42 tuple type ((_ . vars) . body) . rest)
               (bind tuple
                  (lambda (ignore . vars) . body)))
            ;;; an else case needing the tuple
            ((tuple-case 42 tuple type (else is name . body))
               (let ((name tuple))
                  (begin . body)))
            ;;; a normal else clause
            ((tuple-case 42 tuple type (else . body))
               (begin . body))
            ;;; throw an error if nothing matches
            ((tuple-case 42 tuple type)
               (syntax-error "weird tuple-case"))
            ;;; get type and start walking
            ((tuple-case tuple case ...)
               (let ((type (ref tuple 1)))
                  (tuple-case 42 tuple type case ...)))))

      ;; FIXME: now fetches type on each test. take it later just once after removing the (teq? ..) cases
      ;; FIXME: remove the new keyword later. now needed to differentiate between old immediates and type ids
      (define-syntax type-case
         (syntax-rules 
            (else -> teq? imm alloc new)
            
            ((type-case ob (else . more))
               (begin . more))
            ((type-case ob (else -> name . more))
               (let ((name ob)) . more))
            ((type-case (op . args) . rest)
               (let ((foo (op . args)))
                  (type-case foo . rest)))
            ((type-case ob ((new id) . then) . else) ;; new types
               (if (eq? id (type ob))
                  (begin . then)
                  (type-case ob . else)))
            ((type-case ob (type -> name . then) . more) ;; old, remove later
               (if (teq? ob type)
                  (let ((name ob)) . then)
                  (type-case ob . more)))
            ((type-case ob (type . then) . more) ;; old, remove later
               (if (teq? ob type)
                  (begin . then)
                  (type-case ob . more)))))

      (define-syntax call-with-values
         (syntax-rules ()
            ((call-with-values (lambda () exp) (lambda (arg ...) body))
               (receive exp (lambda (arg ...) body)))
            ((call-with-values thunk (lambda (arg ...) body))
               (receive (thunk) (lambda (arg ...) body)))))

      (define-syntax do
        (syntax-rules ()
          ((do 
            ((var init step) ...)
            (test expr ...)
            command ...)
           (let loop ((var init) ...)
            (if test 
               (begin expr ...)
               (loop step ...))))))

      (define-syntax define-library
         (syntax-rules (export import begin _define-library define-library)
            ;; push export to the end (should syntax-error on multiple exports before this)
            ((define-library x ... (export . e) term . tl)
             (define-library x ... term (export . e) . tl))

            ;; lift all imports above begins
            ;((define-library x ... (begin . b) (import-old . i) . tl)
            ; (define-library x ... (import-old . i) (begin . b) . tl))

            ;; convert to special form understood by the repl
            ;((define-library name (import-old . i) ... (begin . b) ... (export . e))
            ; (_define-library 'name '(import-old . i) ... '(begin . b) ... '(export . e)))

            ;; accept otherwise in whatever order
            ((define-library thing ...)
             (_define-library (quote thing) ...))

            ;; fail otherwise
            ((_ . wtf)
               (syntax-error "Weird library contents: " (quote . (define-library . wtf))))))

      ;; toplevel library operations expand to quoted values to be handled by the repl
      ;(define-syntax import  (syntax-rules (_import)  ((import  thing ...) (_import  (quote thing) ...))))
      ;(define-syntax include (syntax-rules (_include) ((include thing ...) (_include (quote thing) ...))))

      (define (not x)
         (if x #false #true))

      (define o (λ (f g) (λ (x) (f (g x)))))

      (define i (λ (x) x))

      (define self i)

      ; (define call/cc  ('_sans_cps (λ (k f) (f k (λ (r a) (k a))))))

      (define (i x) x)
      (define (k x y) x)


      ;;;
      ;;; TYPE TAGS
      ;;;
      ;
      ;                            .------------> 24-bit payload if immediate
      ;                            |      .-----> type tag if immediate
      ;                            |      |.----> immediateness
      ;   .------------------------| .----||.---> mark bit (can only be 1 during gc, removable?)
      ;  [pppppppp pppppppp pppppppp tttttti0]
      ;   '-------------------------------|
      ;                                   '-----> 4- or 8-byte aligned pointer if not immediate
      ;
      ; object headers are further
      ;
      ;                         RPPP
      ;  [ssssssss ssssssss ________ tttttt10]
      ;   '---------------| '------| '----|
      ;                   |        |      '-----> object type
      ;                   |        '------------> tags notifying about special freeing needs for gc (fds), rawness, padding bytes, etc non-type info
      ;                   '--------------------->
      ;  
      ;; note - there are 6 type bits, but one is currently wasted in old header position
      ;; to the right of them, so all types must be <32 until they can be slid to right 
      ;; position.

      ;; ALLOCATED
      (define type-bytecode          0) ;; going to 16
      (define type-proc             32) ;; going to 17
      (define type-clos             64) ;; going to 18
      (define type-pair              1)
      (define type-vector-dispatch  15)
      (define type-vector-leaf      11)
      (define type-vector-raw       19) ;; going 171 -> 19, see also TBYTES in c/ovm.c
      (define type-ff-black-leaf     8)
      (define type-symbol            4)
      (define type-tuple             2)
      (define type-symbol            4)
      (define type-rlist-node       14)
      (define type-rlist-spine      10)

      ;; transitional trees
      (define type-ff               24)   ;; black[key val] or black[key val left] or black[key val left right]
      (define type-ff-right         25)   ;; black[key val right]
      ; + type-ff-red, type-ff-right

      ; 8 - black ff leaf
      ;; IMMEDIATE
      (define type-fix+              0)
      (define type-fix-             32)
      (define type-eof              20) ;; moved from 4, clashing with symbols
      (define type-const            13) ;; old type-null, moved from 1, clashing with pairs
      (define type-port             12)
      (define type-socket           44)
      (define type-tcp-client       62)


      ;;           allocated/pointers     allocated/rawdata    immediate
      ;; (size  x)         n                       n               #false
      ;; (sizeb x)       #false                    n               #false

      (define (immediate? obj) (eq? #false (size obj)))
      (define allocated? size)
      (define raw?       sizeb)
))
