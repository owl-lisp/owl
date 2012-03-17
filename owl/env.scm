;; todo: rename old env-* to env-* 

(define-library (owl env)

	(export lookup env-bind env-ref env-set apply-env env-fold
      verbose-vm-error prim-opcodes opcode->wrapper primop-of primitive?
      poll-tag link-tag buffer-tag signal-tag signal-halt thread-quantum
      env-set-macro *tabula-rasa* env-del
      )

   (import
      (owl ff)
      (owl list)
      (owl symbol)
      (owl string)
      (owl render)
      (owl equal)
      (owl list-extra)
      (owl math)
      (scheme misc)
      (owl primop)
      (owl defmac))

   (begin

      (define poll-tag "mcp/polls")
      (define buffer-tag "mcp/buffs")
      (define link-tag "mcp/links")
      (define signal-tag "mcp/break")
      (define (signal-halt threads state controller) 
         (halt 42)) ;; exit owl with return value 1
      (define thread-quantum 10000)

      (define lookup
         (let ((undefined (tuple 'undefined)))
            (lambda (env key)
               (get env key undefined))))

      (define (env-ref env key def)
         (tuple-case (lookup env key)
            ((defined val)
               (tuple-case val
                  ((value v) v)
                  (else def)))
            (else def)))

      (define (env-set env key val)
         (put env key
            (tuple 'defined 
               (tuple 'value val))))

      (define (env-set-macro env key transformer)
         (put env key
            (tuple 'macro transformer)))

      (define-syntax invoke
         (syntax-rules ()
            ((invoke module name arg ...)
               ((env-ref module (quote name)
                  (lambda (arg ...)
                     (error "invoke: failed to invoke " 
                        (cons (quote name) 
                           (list arg ...)))))
                  arg ...))))

      (define env-bind
         (let ((bound (tuple 'bound)))
            (lambda (env keys)
               (fold
                  (lambda (env key) (put env key bound))
                  env keys))))

      ;;;
      ;;; apply-env
      ;;;

      ; this compiler pass maps sexps to sexps where each free 
      ; occurence of a variable is replaced by it's value

      ; this is functionally equivalent to making a big 
      ; (((lambda (name ..) exp) value)), but the compiler currently
      ; handles values occurring in the sexp itself a bit more efficiently

      (define (ok env exp) (tuple 'ok exp env))
      (define (fail reason) (tuple 'fail reason))

      (define (value-exp val)
         ; represent the literal value val safely as an s-exp
         (if (or (pair? val) (symbol? val))
            (list 'quote val)
            val))

      (define (handle-symbol exp env fail)
         ;(print (list 'handle-symbol exp 'being (lookup env exp)))
         (tuple-case (lookup env exp)
            ((bound) exp)
            ((defined defn)
               (tuple-case defn
                  ((value val) 
                     (value-exp val))
                  (else is funny
                     (fail (list "funny defined value: " funny)))))
            ((undefined)
               (fail (list "What is" 
                  (runes->string (foldr render '() (list "'" exp "'?"))))))
            (else is bad
               (fail (list "The symbol" exp "has a funny value: '" bad "'")))))

      (define (formals-cool? call)
         (let ((formals (cadr call)))
            (and (list? formals) (all symbol? formals))))

      (define (walker env fail)
         (define (walk exp)
            ; (print (list 'walk exp))
            (cond
               ((null? exp)
                  ; allow null as a self-evaluating form
                  (list 'quote exp))
               ((list? exp)
                  (case (car exp)
                     ((lambda)
                        (if (and (= (length exp) 3) (formals-cool? exp))
                           (list 'lambda (cadr exp)
                              ((walker (env-bind env (cadr exp)) fail)
                                 (caddr exp)))
                           (fail (list "funny lambda: " exp))))
                     ((rlambda)
                        (if (and (= (length exp) 4) (formals-cool? exp))
                           (let ((walk (walker (env-bind env (cadr exp)) fail)))
                              (list 'rlambda
                                 (cadr exp)
                                 (map walk (caddr exp))
                                 (walk (car (cdddr exp)))))
                           (fail (list "funny rlambda: " (list exp 'len (length exp) 'forms (formals-cool? exp))))))
                     ((values receive _branch)
                        (cons (car exp)
                           (map walk (cdr exp))))
                     ((quote) exp)
                     (else
                        (map walk exp))))
               ((symbol? exp)
                  (handle-symbol exp env fail))
               ((pair? exp)
                  (fail (list "improper code: " exp)))
               ((number? exp)
                  exp)
               (else
                  (list 'quote exp))))
         walk)

      ; drop definitions from env to unbound occurrences in exp
      ; after macros have been expanded

      (define (apply-env exp env)
         (call/cc 
            (lambda (ret)
               (ok env
                  ((walker env 
                     (lambda (reason) 
                        (ret (fail reason))))
                     exp)))))
         
      (define env-fold ff-fold)

      (define env-del del)

      ;;; these cannot be in primop since they use lists and ffs

      (define (verbose-vm-error opcode a b)
         (cond
            ((eq? opcode 256)
               ; fixme, add but got ...
               (list 'function b 'expected a 'arguments))
            ((eq? opcode 52) (list "car: bad pair: " a))
            ((eq? opcode 53) (list "cdr: bad pair: " a))
            (else
               (list "error: " 'instruction opcode 'info (tuple a b)))))

      ;; ff of wrapper-fn → opcode
      (define prim-opcodes
         (for #false primops
            (λ (ff node)
               (put ff (ref node 5) (ref node 2)))))

      ;; ff of opcode → wrapper
      (define opcode->wrapper
         (for #false primops
            (λ (ff node)
               (put ff (ref node 2) (ref node 5)))))

      ;; later check type, get first opcode and compare to primop wrapper
      (define (primop-of val)
         (cond
            ((get prim-opcodes val #false) => (lambda (op) op))
            ((equal? val mkt) 23)
            ((equal? val bind) 32)
            ((equal? val ff-bind) 49)
            (else #false)))

      ;; only special forms supported by the compiler, no primops etc
      (define *tabula-rasa*
         (list->ff
            (list
               ;; special forms.
               (cons 'lambda  (tuple 'special 'lambda))
               (cons 'quote   (tuple 'special 'quote))
               (cons 'rlambda (tuple 'special 'rlambda))
               (cons 'receive (tuple 'special 'receive))
               (cons '_branch (tuple 'special '_branch))
               (cons '_define (tuple 'special '_define))
               (cons 'values   (tuple 'special 'values)))))

   (define primitive? primop-of)

))
