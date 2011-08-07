;;;
;;; Converting Sexps to a more compact and AST
;;;

,r "owl/env.l"

(define-module lib-ast

	(export call? var? value-of sexp->ast mkcall mklambda mkvar mkval)
	(import lib-env)

	(define (ok exp env) (tuple 'ok exp env))
	(define (fail reason) (tuple 'fail reason))

	(define (call? thing) (eq? (ref thing 1) 'call))
	(define (var? thing) (eq? (ref thing 1) 'var))
	(define (value-of node) (ref node 2))

	(define (mkval val)
		(tuple 'value val))

	(define (mklambda formals body)
		(tuple 'lambda formals body))

	(define (mkcall rator rands)
		(tuple 'call rator rands))

	;;; cps adds a cont + system
	(define (mkprim op args)
		(tuple 'prim op args))

	(define (mkvar sym)
		(tuple 'var sym))

	(define (unique? lst)
		(cond
			((null? lst) True)
			((has? (cdr lst) (car lst)) False)
			(else (unique? (cdr lst)))))

	(define (formals-ok? lst)
		(and (all symbol? lst) (unique? lst)))

	(define (translate-direct-call exp env fail translate)
		(tuple-case (lookup env (car exp))
			((special thing)
				(case thing
					((quote)
						(if (= (length exp) 2)
							(mkval (cadr exp))
							(list "Strange quote: " exp)))
					((lambda)
						(let ((len (length exp)))
							(cond
								((= len 3)
									(lets
										((formals (cadr exp))
										 (body (caddr exp)))
										(if (formals-ok? formals)
											(mklambda formals
												(translate body
													(env-bind env formals)
													fail))
											(fail (list "Bad formals in lambda: " exp)))))
								((> len 3)
									(let
										((formals (cadr exp))
										 (body (cddr exp)))
										(translate 
											(list 'lambda formals 
												(cons 'begin body)) env fail)))
								(else
									(fail (list "Bad lambda: " exp))))))
					((rlambda) ;;; (rlambda formals definitions body)
						(if (= (length exp) 4)
							(let
								((formals (lref exp 1))
								 (values (lref exp 2))
								 (body (lref exp 3)))
								(if
									(and
										(list? values)
										(formals-ok? formals)
										(= (length formals) (length values)))
									(let ((env (env-bind env formals)))
										(tuple 'rlambda formals
											(map
												(lambda (x) (translate x env fail))
												values)
											(translate body env fail)))
									(fail (list "Bad rlambda: " exp))))
							(fail (list "Bad rlambda " exp))))
					((_branch)
						(if (= (length exp) 6)
							(let
								((a (lref exp 2))
								 (b (lref exp 3))
								 (then (lref exp 4))
								 (else (lref exp 5)))
								(tuple 'branch
									(lref exp 1)					; type 
									(translate a env fail)
									(translate b env fail)
									(translate then env fail)
									(translate else env fail)))
							(fail (list "Bad branch: " exp))))
					((receive)	; (receive <exp> <receiver>)
						(tuple 'receive 
							(translate (lref exp 1) env fail)
							(translate (lref exp 2) env fail)))
					;; FIXME pattern
					((values)
						(tuple 'values
							(map (lambda (arg) (translate arg env fail)) (cdr exp))))
					(else
						(fail
							(list 
								"Unknown special operator in ast conversion: " 
								exp)))))
			((bound)
				(mkcall (mkvar (car exp))
					(map
						(lambda (x) (translate x env fail))
						(cdr exp))))
			;; both now handled by apply-env
			;((undefined)
			;	(fail (list "i do not know this function" exp)))
			; left here to handle primops temporarily
			((defined value)
				(mkcall value
					(map (lambda (x) (translate x env fail)) (cdr exp))))
			(else
				; could be useful for (eval (list + 1 2) env)
				; so just warn for now
				(show 
					"Unknown value type in ast conversion: " 
					(list 'name (car exp) 'value  (lookup env (car exp))))
				(mkval exp))))

	(define (translate exp env fail)
		(cond 
			((null? exp) (mkval exp))
			((list? exp)
				(if (symbol? (car exp))
					(translate-direct-call exp env fail translate)
					(mkcall
						(translate (car exp) env fail)
						(map
							(lambda (x)
								(translate x env fail))
							(cdr exp)))))
			((symbol? exp)
				(tuple-case (lookup env exp)
					((bound)
						(mkvar exp))
					;; should be already handled in apply-env
					((defined value)
						value)
					((special thing)
						(fail 
							(list "a special thing being used as an argument: " exp)))
					((undefined)
						(fail (list "what are '" exp "'?")))
					(else 
						(fail
							(list "Strange value in ast conversion: "
								(lookup env exp))))))
			(else (mkval exp))))

	; -> #(ok exp' env) | #(fail reason)

	(define (sexp->ast exp env)
		(call/cc
			(lambda (drop)
				(tuple 'ok
					(translate exp env
						(lambda (reason) (drop (fail reason))))
					env))))

)
