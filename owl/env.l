; todo: 
;	- add primop functions in apply-env and inline in later passes


(define-module lib-env

	(export lookup env-bind env-set module-ref module-set apply-env)

	(define lookup
		(let ((undefined (tuple 'undefined)))
			(lambda (env key)
				(get env key undefined))))

	(define (module-ref env key def)
		(tuple-case (lookup env key)
			((defined val)
				(tuple-case val
					((value v) v)
					(else def)))
			(else def)))

   (define (module-set env key val)
      (put env key
         (tuple 'defined 
            (tuple 'value val))))

	(define-syntax invoke
		(syntax-rules ()
			((invoke module name arg ...)
				((module-ref module (quote name)
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
	; ((lambda (name ..) exp) value)), but the compiler currently
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
					(runes->string (foldr renderer '() (list "'" exp "'?"))))))
			(else is bad
				(fail (list "The symbol '" exp "' has a funny value: '" bad "'")))))

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
		
	(define env-set put)

)
