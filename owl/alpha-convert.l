;;;
;;; Alpha conversion
;;;

; convert all variables to fresh symbols. makes compilation easier.

,r "owl/gensym.l"
,r "owl/ast.l"
,r "owl/env.l"

(define-module lib-alpha-convert

	(export alpha-convert)

	(import lib-gensym)
	(import lib-ast)
	(import lib-env lookup)

	(define (ok exp env) (tuple 'ok exp env))
	(define (fail reason) (tuple 'fail reason))

	(define (gensyms free n)
		(if (= n 0)
			(values null free)
			(lets ((gens next (gensyms (gensym free) (- n 1))))
				(values (cons free gens) next))))

	(define (alpha-list alpha exps env free)
		(if (null? exps) 
			(values null free)
			(lets
				((this free (alpha (car exps) env free))
				 (tail free (alpha-list alpha (cdr exps) env free)))
				(values (cons this tail) free))))

	(define (alpha exp env free)
		(tuple-case exp
			((var sym)
				(values (mkvar (lookup env sym)) free))
			((call rator rands)
				(lets
					((rator free (alpha rator env free))
					 (rands free (alpha-list alpha rands env free)))
					(values (mkcall rator rands) free)))
			((lambda formals body)
				(lets
					((new-formals free (gensyms free (length formals)))
					 (body free
						(alpha body
							(for env (zip cons formals new-formals)
								(lambda (env node)
									(put env (car node) (cdr node))))
							free)))
					(values (mklambda new-formals body) free)))
			((value val)
				(values exp free))
			((branch kind a b then else)
				(lets
					((a free (alpha a env free))
					 (b free (alpha b env free))
					 (then free (alpha then env free))
					 (else free (alpha else env free)))
					(values
						(tuple 'branch kind a b then else)
						free)))
			((receive from to)
				(lets
					((from free (alpha from env free))
					 (to free   (alpha to   env free)))
					(values (tuple 'receive from to) free)))
			((values vals)
				(lets ((vals free (alpha-list alpha vals env free)))
					(values (tuple 'values vals) free)))
			(else
				(error "alpha: unknown exp type: " exp))))

	(define (alpha-convert exp env)
		(lets 
			((exp free 
				(alpha exp empty-ff (gensym exp))))
			(ok exp env)))
)
