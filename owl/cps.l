;; todo: report errors via a failure cont

,r "owl/ast.l"
,r "owl/gensym.l"
,r "owl/assemble.l"
,r "owl/primop.l"

(define-module lib-cps

	(export cps)

	(import lib-ast)
	(import lib-gensym)
   (import lib-primop)

	;; fixme: information about cps-special primops could be stored elsewhere

	(define (ok exp env) (tuple 'ok exp env))
	(define (fail reason) (tuple 'fail reason))

	(define (cps-literal exp env cont free)
		(values
			(mkcall cont (list exp))
			free))

	(define (cps-just-lambda cps formals body env free)
		(lets
			((cont-sym free (fresh free))
			 (body free (cps body env (mkvar cont-sym) free)))
			(values
				(mklambda (cons cont-sym formals) body)
				free)))

	(define (cps-lambda cps formals body env cont free)
		(lets ((lexp free (cps-just-lambda cps formals body env free)))
			(values (mkcall cont (list lexp)) free)))

	(define (cps-args cps args call env free)
		(if (null? args)
			(lets
				((call (reverse call))
				 (rator (car call))
				 (rands (cdr call)))
				(values (mkcall rator rands) free))
			(tuple-case (car args)
				((lambda formals body)
					(lets ((lexp free (cps-just-lambda cps formals body env free)))
						(cps-args cps (cdr args) (cons lexp call) env free)))
				((value foo)
					(cps-args cps (cdr args) (cons (car args) call) env free))
				((var foo)
					(cps-args cps (cdr args) (cons (car args) call) env free))
				(else
					(lets
						((this free (fresh free))
						 (rest free 
							(cps-args cps (cons (mkvar this) (cdr args)) call env free)))
						(cps (car args)
							env
							(mklambda (list this) rest)
							free))))))

	(define (cps-values cps vals env cont free)
		(cps-args cps vals (list cont) env free))

	;; fixme: check - assuming tuple exp is already cps'd
	(define (cps-bind cps rator rands env cont free)
		(if (= (length rands) 2)
			(tuple-case (cadr rands)
				((lambda formals body)
					(lets ((body free (cps body env cont free)))
						(cps-args cps (list (car rands))
							(list (mklambda formals body) rator)
							env free)))
				(else
					(error "bad arguments for tuple bind: " rands)))
			(error "bad arguments for tuple bind: " rands)))


	(define (cps-call cps rator rands env cont free)
		(tuple-case rator
			((lambda formals body)
				(lets
					((body free (cps body env cont free)))
					(if (null? formals)
						;;; drop lambdas from ((lambda () X))
						(values body free)
						(cps-args cps rands
							(list (mklambda formals body))
							env free))))
			((call rator2 rands2)
				(lets
					((this free (fresh free))
					 (call-exp free 
					 	(cps-args cps rands (list cont (mkvar this)) env free)))
					(cps rator env
						(mklambda (list this) call-exp)
						free)))
			((branch kind a b then else)
				(lets ((this free (fresh free)))
					(cps
						(mkcall (mklambda (list this) (mkcall (mkvar this) rands))
							(list rator))
						env cont free)))
			((value val)
				(let ((pop (primop-of val)))
					(if (special-bind-primop? pop)
						(cps-bind cps rator rands env cont free)
						(cps-args cps rands (list cont rator) env free))))
			(else
				(cps-args cps rands (list cont rator) env free))))

	(define (cps-branch cps kind a b then else env cont free)
		(cond
			((not (var? cont))
				(lets
					((this free (fresh free))
					 (exp free
						(cps-branch cps kind a b then else env (mkvar this) free)))
					(values
						(mkcall
							(mklambda (list this) exp)
							(list cont))
						free)))
			((call? a)
				(lets
					((this free (fresh free))
					 (rest free
					 	(cps-branch cps kind (mkvar this) b then else env cont free)))
					(cps a env (mklambda (list this) rest) free)))
			((call? b)
				(lets
					((this free (fresh free))
					 (rest free 
					 	(cps-branch cps kind a (mkvar this) then else env cont free)))
					(cps b env (mklambda (list this) rest) free)))
			((eq? kind 4)
				; a binding type discrimination. matching branch is treated as in bind 
				; only cps-ing body to current continuation
				(tuple-case then
					((lambda formals body)
						(lets
							((then-body free (cps body env cont free))
							 (else free (cps else env cont free)))
							(values
								(tuple 'branch kind a b 
									(mklambda formals then-body)
									else)
								free)))
					(else
						(error "cps-branch: then is not a lambda: " then))))
			(else
				(lets
					((then free (cps then env cont free))
					 (else free (cps else env cont free)))
					(values
						(tuple 'branch kind a b then else)
						free)))))

	(define (cps-receive cps exp semi-cont env cont free)
		(tuple-case semi-cont
			((lambda formals  body)
				(lets ((body-cps free (cps body env cont free)))
					(cps exp env 
						(mklambda formals body-cps)
						free)))
			(else
				(error "cps-receive: receiver is not a lambda. " semi-cont))))

	(define (cps-exp exp env cont free)
		(tuple-case exp
			((value val)
				(cps-literal exp env cont free))
			((var sym)
				(cps-literal exp env cont free))
			((lambda formals body)
				(cps-lambda cps-exp formals body env cont free))
			((call rator rands)
				(cps-call cps-exp rator rands env cont free))
			((branch kind a b then else)
				(cps-branch cps-exp kind a b then else env cont free))
			((values vals)
			  (cps-values cps-exp vals env cont free))
			((receive exp target)
			  (cps-receive cps-exp exp target env cont free))
			(else
				(error "CPS does not do " exp))))

	(define (val-eq? node val)
		(tuple-case node
			((value this)
				(eq? this val))
			(else False)))

	; pass fail to cps later and exit via it on errors

	(define (cps exp env)
		(or
			(call/cc
				(lambda (fail)
					(let ((cont-sym (gensym exp)))	
						; a hack to be able to define code sans cps 	
						; a better solution would be ability to change the	
						; compiler chain interactively
						(if (and 			
								(call? exp) 
								(val-eq? (ref exp 2) '_sans_cps)	
								(= (length (ref exp 3)) 1))
							(ok
								(mklambda (list cont-sym) 
									(mkcall (mkvar cont-sym)
										(list (car (ref exp 3)))))
								env)
							(lets ((exp free (cps-exp exp env (mkvar cont-sym) (gensym cont-sym))))
								(ok
									(mklambda (list cont-sym) exp)
									env))))))
			(fail "cps failed")))
)

