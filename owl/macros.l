; already loaded when booting.

,r "owl/gensym.l"
,r "owl/env.l" 

(define-module lib-macros
	
	; remove make-transformer when it is no longer referred 
	(export macro-expand match make-transformer)

	(import lib-gensym)
	(import lib-env)

	;;; Misc

	(define (ok exp env) (tuple 'ok exp env))
	(define (fail reason) (tuple 'fail reason))

	(define symbols-of

		(define (walk exp found)
			(cond
				((pair? exp)
					(walk (cdr exp)
						(walk (car exp) found)))
				((and (symbol? exp) (not (has? found exp)))
					(cons exp found))
				(else found)))

		(lambda (exp)
			(walk exp null)))


	;;;
	;;; Basic pattern matching for matching the rule pattern against sexp
	;;;

	(define (? x) True)

	(define (match pattern exp)

		(define (match-pattern pattern exp vals)
			(cond
				((not vals) False)
				((pair? pattern)
					(if (pair? exp)
						(match-pattern (car pattern) (car exp)
							(match-pattern (cdr pattern) (cdr exp) vals))
						False))
				((eq? pattern exp)
					vals)
				((function? pattern)
					(if (pattern exp) (cons exp vals) False))
				(else False)))

		(match-pattern pattern exp null))


	;;;
	;;; Matching and rewriting based on rewrite rules
	;;;

	; fixme, there are ffs now

	; store nulls to variables in exp
	(define (init-variables exp literals dict)
		(fold 
			(lambda (dict key) (cons (cons key null) dict))
			dict
			(diff (symbols-of exp) literals)))

	(define (push dict key val)
		(cond
			((null? dict)
				(error "push: key not in dict: " key))
			((eq? (caar dict) key)
				(cons
					(append (car dict) (list val))
					(cdr dict)))
			(else
				(cons (car dict)
					(push (cdr dict) key val)))))

	(define (match-pattern pattern literals form fail)
		(let loop 
			((pattern pattern) (form form) (collect? False) 
				(fail fail) (dictionary null))
			(cond
				((symbol? pattern)
					(if (has? literals pattern)
						(if (eq? pattern form) dictionary (fail pattern))
						(if collect?
							;;; append to dictionary
							(push dictionary pattern form)
							(let ((binding (getq dictionary pattern)))
								(if binding
									(if (equal? (cadr binding) form) dictionary 
										(fail pattern))
									(cons (list pattern form) dictionary))))))
				((null? pattern)
					(if (null? form) dictionary (fail pattern)))
				((pair? pattern)
					(cond
						((and (pair? (cdr pattern)) (eq? (cadr pattern) '...))
							(let ((dictionary 
										(init-variables (car pattern) 
											literals  dictionary)))
								; each time a form is matched
								;	resume matching with a fail cont returning to
								;	process more
								(let next 
									((prev-dict dictionary) 
									 (old-form form) 
									 (new-dict dictionary) 
									 (form form))
									(call/cc
										(lambda (ret)
										(if (and new-dict (pair? form))
											(loop (cddr pattern) form False
												(lambda (argh)
													(ret 
														(next new-dict form
															(call/cc
																(lambda (ret)
																	(loop (car pattern) (car form) 
																		True (lambda (x) (ret False))
																		new-dict)))
															(cdr form))))
												new-dict)
										; no more matches
										(loop (cddr pattern) 
											(if new-dict form old-form) 
											False 
											fail 
											(if new-dict new-dict prev-dict))))))))
						((pair? form)
							(loop (cdr pattern) (cdr form) collect? fail
								(loop (car pattern) (car form) collect? fail 
									dictionary)))
						(else (fail form))))
				((equal? pattern form)
					dictionary)
				(else (fail form)))))

	(define (try-pattern pattern literals form)
		(call/cc
			(lambda (ret)
				(match-pattern pattern literals form 
					(lambda (argh) (ret False))))))

	(define (rewrite-ellipsis rewrite form vars)
		(call/cc
			(lambda (return)
				(let
					((currents
						(map
							(lambda (node)
								(if (null? (cdr node))
									(return null)
									(cons (car node) (list (cadr node)))))
							vars))
					 (rests
						(map
							(lambda (node)
								(cons (car node)
									(if (null? (cdr node))
										null
										(cddr node))))
							vars)))
					(cons
						(rewrite currents form)
						(rewrite-ellipsis rewrite form rests))))))

	(define (rewrite dictionary form)
		(let loop ((form form))
			(cond
				((symbol? form)
					(let ((binding (getq dictionary form)))
						(if (and binding (pair? (cdr binding)))
							(cadr binding)
							form)))
				((pair? form)
					(if (and (pair? (cdr form)) (eq? (cadr form) '...))
						(append
							(rewrite-ellipsis rewrite (car form)
								(let ((symbols (symbols-of (car form))))
									(keep 
										(lambda (node) (has? symbols (car node)))
										dictionary)))
							(loop (cddr form)))
						(cons
							(loop (car form))
							(loop (cdr form)))))
				(else form))))


	;;; Intermission

	; exp env free -> status exp' free'

	(define toplevel-macro-definition?
		(let 
			((pattern 
				`(quote syntax-operation add False (,symbol? ,list? ,list? ,list?))))
			;; -> keyword literals patterns templates
			(lambda (exp)
				(match pattern exp))))

	; -> tuple
	(define (fold2 op s1 s2 lst)
		(if (null? lst)
			(tuple s1 s2)
			(bind (op s1 s2 (car lst))
				(lambda (s1 s2)
					(fold2 op s1 s2 (cdr lst))))))

	(define (add-fresh-bindings names free dict)
		(fold2
			(lambda (free dict name)
				(tuple
					(gensym free)
					(cons (list name free) dict)))
			free dict names))

	(define (make-transformer literals rules)
		(lambda (form free)
			(some
				(lambda (rule)
					;; rule = (pattern gensyms template)
					(let ((dictionary (try-pattern (car rule) literals form)))
						(if dictionary
							(bind (add-fresh-bindings (cadr rule) free dictionary)
								(lambda (free dictionary)
									(let ((new (rewrite dictionary (caddr rule))))
										(tuple new free)
										)))
							False)))
				rules)))

	; add fresh symbol list -> ((pattern fresh template) ...)

	(define (make-pattern-list literals patterns templates unbound?)
		(zip
			(lambda (pattern template)
				(lets
					((pattern-symbols (symbols-of pattern))
					 (template-symbols (symbols-of template))
					 (fresh-symbols
						(keep
							(lambda (x) (and (unbound? x) (not (has? literals x))))
							(diff template-symbols pattern-symbols))))
					(list pattern fresh-symbols template)))
			patterns templates))


	;;;
	;;; Macro expansion in a given env
	;;;

	; expand all macros top to bottom
	; exp env free -> #(exp' free')

	(define (expand exp env free abort)

		;(show "expand: " exp)
		(define (expand-list exps env free)
			(if (null? exps)
				(values null free)
				(lets
					((this free (expand (car exps) env free abort))
					 (tail free (expand-list (cdr exps) env free)))
					(values (cons this tail) free))))

		(cond
			((null? exp)
				(values exp free))
			((list? exp)
				(cond
					((symbol? (car exp))
						(tuple-case (lookup env (car exp))
							((special thing)
								(case thing
									((quote) (values exp free))
									((_define)
										; (show " - expanding define body " (caddr exp))
										(lets
											((value free 
												(expand (caddr exp) env free abort)))
											(values
												(list '_define (cadr exp) value)
												free)))
									((lambda)
										(lets
											((formals (cadr exp))
											 (body-exps (cddr exp))
											 (body 
												(if (and (pair? body-exps) (null? (cdr body-exps)))
													(car body-exps)
													(cons 'begin body-exps)))
											 (body free
												(expand body (env-bind env formals) free abort)))
											(values (list 'lambda formals body) free)))
									((rlambda)
										(lets
											((formals (lref exp 1))
											 (definitions (lref exp 2))
											 (body (lref exp 3))
											 (env (env-bind env formals))
											 (definitions free
												(expand-list definitions env free))
											 (body free
												(expand body env free abort)))
											(values
												(list 'rlambda formals definitions body)
												free)))
									((receive)
										(expand-list exp env free))
									((_branch)
										(expand-list exp env free))
									((values)
										(expand-list exp env free))
									(else
										(abort 
											(list "expand: unknown special form: " exp)))))
							((bound) 			(expand-list exp env free))
							((defined value)  (expand-list exp env free))
							((undefined)
								;; can be a literal
								(values exp free))
							((macro transformer)
								(let ((result (transformer exp free)))
									(if result
										(expand (ref result 1) env (ref result 2) abort)
										(abort exp))))
							(else is node
								; usually bad module exports, since those are not checked atm
								(abort (list "expand: rator maps to unknown value " (car exp))))))
					(else
						(expand-list exp env free))))
			((symbol? exp)
				(tuple-case (lookup env exp)
					((macro transformer)
						(abort (list "Macro being used as a value: " exp)))
					((undefined)
						;; this can still be a literal used by a macro
						(values exp free))
					(else 
						(values exp free))))
			(else 
				(values exp free))))

	; maybe extend the env if a macro is being defined

	(define (post-macro-expand exp env fail)
		(cond
			((toplevel-macro-definition? exp) 
				(lets 
					((rules (lref exp 4))
					 (keyword (lref rules 0))
					 (literals (lref rules 1))
					 (patterns (lref rules 2))
					 (templates (lref rules 3))
					 (rules 
						(make-pattern-list literals patterns templates 
							(lambda (sym)
								(not (get env sym False)))))
					 (transformer 
						(make-transformer (cons keyword literals) rules)))
					(let ((env (env-set env keyword (tuple 'macro transformer))))
						(ok (list 'quote keyword) env))))
			(else
				(ok exp env))))

	;; bug: exported macros do not preserve bindinds

	(define (macro-expand exp env)
		(let ((task (list 'repl-macro-expand)))
			; run the compiler chain in a new task
			(fork-linked task
				(lambda  ()
					(call/cc
						(lambda (exit)
							(lets
								((abort (lambda (why) (exit (fail why))))
								 (free (gensym exp))
								 (exp free (expand exp env free abort)))
								(post-macro-expand exp env abort))))))
			; grab the result
			(tuple-case (ref (accept-mail (lambda (env) (eq? (ref env 1) task))) 2)
				((finished result not used)
					result) ; <- is already ok/fail
				((crashed opcode a b)
					(fail (verbose-vm-error opcode a b)))
				((error cont reason info) (fail (list reason info)))
				((breaked) (fail "breaked"))
				(else is foo (fail (list "Funny failure mode: " foo))))))


)

