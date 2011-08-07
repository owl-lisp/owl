;;;
;;; Convert lambdas to closures where necessary
;;;

; - convert lambdas which do not occur as operators to closures
; - add information about required literal values and variables

,r "owl/ast.l"
,r "owl/assemble.l"

(define-module lib-closurize

	(export build-closures uncompiled-closure?)

	(import lib-ast)
	(import lib-assemble)

	(define (ok exp env) (tuple 'ok exp env))
	(define (fail reason) (tuple 'fail reason))

	(define (small-value? val)
		(or
			(and (fixnum? val) (>= val -127) (< val 127))
			(eq? val True)
			(eq? val False)
			(eq? val null)))

	(define (value-primop val)
		(and (tuple? val)
			(eq? 'value (ref val 1))
			(primitive? (ref val 2))))

	(define (closurize-list closurize exps used)
		(if (null? exps)
			(values null used)
			(lets
				((this used (closurize (car exps) used True))
				 (tail used (closurize-list closurize (cdr exps) used)))
				(values (cons this tail) used))))


	 (define (closurize-call closurize rator rands used)
		(let ((op (value-primop rator)))
			(if op
				(begin
					;(show " no clos for " rator)
					(tuple-case (car rands)
						((lambda formals body)
							(lets
								((cont used (closurize (car rands) used False))
								 (rands used (closurize-list closurize (cdr rands) used)))
								(values (mkcall rator (cons cont rands)) used)))
						((var name)
							(let
								((dummy-cont
									;;; FIXME, should check arity & gensym
									;;; used only once and called immediately
									(mklambda (list '_foo)
										(mkcall (mkvar name)
											(list (mkvar '_foo))))))
								(closurize-call closurize rator
									(cons dummy-cont (cdr rands))
									used)))
						(else
							(error "Bad primitive continuation: " (car rands)))))
				(lets
					((rator used (closurize rator used False))
					 (rands used (closurize-list closurize rands used)))
					(values (mkcall rator rands) used)))))

	(define (closurize exp used close?)
		(tuple-case exp
			((value val)
				(values exp used))
			((var sym)
				(if (has? used sym)
					(values exp used)
					(values exp (cons sym used))))
			((branch kind a b then else)
				; type 4 (binding compare) branches do not closurize then-case
				(lets
					((a used (closurize a used True))
					 (b used (closurize b used True))
					 (then used
						(closurize then used 
							(if (eq? 4 kind) 
								(begin (show "Not closurizing " then) False)
								True)))
					 (else used (closurize else used True)))
					(values
						(tuple 'branch kind a b then else)
						used)))
			((call rator rands)
				(closurize-call closurize rator rands used))
			((lambda formals body)
				(lets
					((body bused
						(closurize body null True))
					 (clos (diff bused formals)))
					(values
						(if close?
							(tuple 'closure formals body clos)
							(tuple 'lambda formals body))
						(union used clos))))
			(else
				(error "closurize: unknown exp type: " exp))))

	(define (literalize-list literalize exps used)
		(if (null? exps)
			(values null used)
			(lets
				((this used (literalize (car exps) used))
				 (tail used (literalize-list literalize (cdr exps) used)))
				(values (cons this tail) used))))

	(define (literalize-call literalize rator rands used)
		(lets
			((rator used 
				(if (value-primop rator)
					(values rator used)
					(literalize rator used)))
			 (rands used 
			 	(literalize-list literalize rands used)))
			(values (mkcall rator rands) used)))

	(define closure-tag (list 'uncompiled-closure))

	(define (uncompiled-closure? thing)
		(and (pair? thing) (eq? (car thing) closure-tag)))

	(define (literalize exp used)
		(tuple-case exp
			((value val)
				(values exp
					(if (or (has? used val) (small-value? val))
						used
						(append used (list val)))))
			((var sym)
				(values exp used))
			((branch kind a b then else)
				(lets
					((a used (literalize a used))
					 (b used (literalize b used))
					 (then used (literalize then used))
					 (else used (literalize else used)))
					(values
						(tuple 'branch kind a b then else)
						used)))
			((call rator rands)
				(literalize-call literalize rator rands used))
			((lambda formals body)
				(lets ((body used (literalize body used)))
					(values
						(tuple 'lambda formals body)
						used)))
			((closure formals body clos)
				;; note, the same closure exp (as in eq?) is stored to both literals 
				;; and code. the one in code will be used to make instructions 
				;; for closing it and the one in literals will be the executable 
				;; part to close against.
				(lets
					((body bused (literalize body null))
					 (closure-exp (tuple 'closure formals body clos bused))
					 (used (append used (list (cons closure-tag closure-exp)))))
					(values
						;;; literals will be #(header <code> l0 ... ln)
						(tuple 'make-closure (+ 1 (length used)) clos bused)
						;; also literals are passed, since the closure type 
						;; and calling protocol are different depending on 
						;; whether there are literals
						used)))
			(else
				(error "literalize: unknown exp type: " exp))))

	(define (build-closures exp env)
		(lets
			((exp used (closurize exp null True))
			 (exp lits (literalize exp null)))
			(if (and (pair? lits) (uncompiled-closure? (car lits)))
				(ok (cdar lits) env)
				(error "Bad closurize output: " 
					(list 'exp exp 'lits lits)))))

)

