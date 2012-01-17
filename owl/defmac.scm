  
(define-library (owl defmac)

   (export
      λ syntax-error begin 
      quasiquote letrec let if 
      cond case define define*
      lets let* or and list
      ilist tuple tuple-case type-case 
      call-with-values do define-library
      lets/cc
      call/cc
      call/cc2
      call-with-current-continuation
      not o i self
      )

   (begin


      (define-syntax λ 
         (syntax-rules () 
            ((λ a) (lambda () a))
            ((λ (v ...) . body) (lambda (v ...) . body))
            ((λ v ... body) (lambda (v ...) body))))

      (define-syntax syntax-error
         (syntax-rules (error)
            ((syntax-error . stuff)
               (error "Syntax error: " (quote stuff)))))

      (define-syntax begin
         (syntax-rules (define define-syntax letrec)
            ;((begin
            ;   (define-syntax key1 rules1)
            ;   (define-syntax key2 rules2) ... . rest)
            ;   (letrec-syntax ((key1 rules1) (key2 rules2) ...)
            ;      (begin . rest)))
            ((begin exp) exp)
            ((begin (define . a) (define . b) ... . rest)
               (begin 42 () (define . a) (define . b) ... . rest))
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

      (define-syntax quasiquote
         (syntax-rules (unquote quote unquote-splicing append)
            ((quasiquote (unquote exp))
               exp)
            ((quasiquote ((unquote-splicing term) . tail))
               (append term (quasiquote tail)))
            ((quasiquote (op . args))
               (cons
                  (quasiquote op)
                  (quasiquote args)))
            ((quasiquote atom)
               (quote atom))
            ((quasiquote) '())))

      (define-syntax letrec
         (syntax-rules (rlambda)
            ((letrec ((?var ?val) ...) ?body) (rlambda (?var ...) (?val ...) ?body))
            ((letrec vars body ...) (letrec vars (begin body ...)))))

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
            ((if test exp) (if test exp False))
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
            ((if False then else) else)
            ((if True then else) then)
            ((if test then else) (_branch 0 test False else then))))

      (define-syntax cond
         (syntax-rules (else =>)
            ((cond) False)
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
            ((case thing) False)
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
                  (case thing . clauses)))))

      (define-syntax define
         (syntax-rules ()
            ((define (op . args) body)
               (define op
                  (letrec ((op (lambda args body))) op)))
            ((define op val)
               (_define op val))
            ((define op a . b)
               (define op (begin a . b)))))

      ;; fixme, should use a print-limited variant for debugging

      (define-syntax define*
         (syntax-rules (show list)
            ((define* (op . args) . body)
               (define (op . args) 
                  (show " * " (list (quote op) . args))
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

      ; i hate special characters, especially in such common operations.
      ; lets (let sequence) is way prettier and a bit more descriptive 

      (define-syntax let*
         (syntax-rules ()
            ((let* . stuff) (lets . stuff))))

      (define-syntax or
         (syntax-rules ()
            ((or) False)
            ((or (a . b) . c)
               (let ((x (a . b)))
                  (or x . c)))
            ((or a . b)
               (if a a (or . b)))))

      (define-syntax and
         (syntax-rules ()
            ((and) True)
            ((and a) a)
            ((and a . b)
               (if a (and . b) False))))

      (define-syntax list
         (syntax-rules ()
            ((list) '())
            ((list a . b)
               (cons a (list . b)))))

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

      (define-syntax type-case
         (syntax-rules 
            (else -> teq? imm alloc)
            
            ((type-case ob (else . more))
               (begin . more))
            ((type-case ob (else -> name . more))
               (let ((name ob)) . more))
            ((type-case (op . args) . rest)
               (let ((foo (op . args)))
                  (type-case foo . rest)))
            ((type-case ob (type -> name . then) . more)
               (if (teq? ob type)
                  (let ((name ob)) . then)
                  (type-case ob . more)))
            ((type-case ob (pat . then) . more)
               (if (teq? ob pat)
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

      (define-syntax lets/cc
         (syntax-rules (call/cc)
            ((lets/cc (om . nom) . fail)
               (syntax-error "let/cc: continuation name cannot be " (quote (om . nom))))
            ((lets/cc var . body)
               (call/cc (λ (var) (lets . body))))))


(define (not x)
   (if x False True))

(define o (λ f g (λ x (f (g x)))))

(define i (λ x x))

(define self i)

      ;; a few usual suspects
      (define call/cc ('_sans_cps (λ (c f) (f c (λ (r v) (c v))))))
      (define call/cc2 ('_sans_cps (λ (c f) (f c (λ (r a b) (c a b))))))
      (define call-with-current-continuation call/cc)
      (define (i x) x)
      (define (k x y) x)
))
