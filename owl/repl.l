(define-module lib-repl

	(export 
		repl-file 
		repl-port
		repl-string 
		repl-trampoline 
		repl
		exported-eval						; fixme, here only temporarily
		print-repl-error
		bind-toplevel
		)

	(define definition? 
		(let ((pat (list '_define symbol? (λ x True))))
			(lambda (exp) (match pat exp))))

	; (bla bla (blo blo (blu blu))) ->
	; bla bla
	;     blo blo
	;         blu blu

   (define error-port stderr)

	(define (print-repl-error lst)
		(define (format-error lst ind)
			(cond
				((and (pair? lst) (null? (cdr lst)) (list? (car lst)))
               (cons 10
                  (let ((ind (+ ind 2)))
                     (append (map (λ (x) 32) (iota 0 1 ind))
                        (format-error (car lst) ind)))))
				((pair? lst)
               (render render (car lst)
                  (cons 32
                     (format-error (cdr lst) ind))))
				((null? lst) '(10))
				(else (render render lst '(10)))))
      (mail error-port
         (format-error lst 0)))

	; -> (ok value env), (error reason env)

	(define repl-op?
		(let ((pattern (list 'unquote symbol?)))	
			(λ exp (match pattern exp))))

	(define (mark-loaded env path)
		(let ((loaded (ref (ref (get env '*loaded* (tuple 'defined (mkval null))) 2) 2)))
			(if (mem string-eq? loaded path)
				env
				(put env '*loaded*
					(tuple 'defined
						(mkval
							(cons path loaded)))))))

	(define (env-get env name def)
		(let ((node (get env name False)))
			(if node
				(if (eq? (ref node 1) 'defined)
					(ref (ref node 2) 2)
					def)
				def)))

   (define (prompt env val)
      (let ((prompt (env-get env '*owl-prompt* F)))
         (if prompt
            (prompt val))))
         
	(define syntax-error-mark (list 'syntax-error))

	;; fixme: the input data stream is iirc raw bytes, as is parser error position, but that one is unicode-aware

	; lst -> n, being the number of things before next 10 or end of list
	(define (next-newline-distance lst)
		(let loop ((lst lst) (pos 0))
			(cond
				((null? lst) (values pos lst))
				((eq? (car lst) 10) (values (+ pos 1) (cdr lst)))
				(else (loop (cdr lst) (+ pos 1))))))

	(define (find-line data error-pos)
		;(print " - find-line")
		(let loop ((data data) (pos 0))
			;(print* (list "data " data " pos " pos  " error-pos " error-pos))
			(lets ((next datap (next-newline-distance data)))
				(cond
					((<= error-pos next)
						(runes->string (take data (- next 1)))) ; take this line
					((null? data)
						"(end of input)")
					(else
						(loop datap next))))))
		
	(define (syntax-fail pos info lst) 
		(list syntax-error-mark info 
			(list ">>> " (find-line lst pos) " <<<")))

	(define (syntax-error? x) (and (pair? x) (eq? syntax-error-mark (car x))))

	(define (repl-fail env reason) (tuple 'error reason env))
	(define (repl-ok env value) (tuple 'ok value env))

   ;; just be quiet
   (define repl-load-prompt 
      (tuple 'defined
         (tuple 'value 
            (λ (val) null))))

	;; load and save path to *loaded*

   ;; todo: should keep a list of documents *loading* and use that to detect circular loads (and to indent the load msgs)
	(define (repl-load repl path in env)
		(lets 	
			((exps ;; find the file to read
				(or 
					(file->exp-stream path "" sexp-parser syntax-fail)
					(file->exp-stream
						(string-append (env-get env '*owl* "NA") path)
						"" sexp-parser syntax-fail))))
			(if exps
            (begin
               (if (env-get env '*interactive* F)
                  (show " + " path))
               (lets
                  ((prompt (env-get env '*owl-prompt* F)) ; <- switch prompt during loading
                   (load-env 
                     (if prompt
                        (env-set env '*owl-prompt* repl-load-prompt) ;; <- switch prompt during load (if enabled)
                        env))
                   (outcome (repl load-env exps)))
                  (tuple-case outcome
                     ((ok val env)
                        (repl (mark-loaded (env-set env '*owl-prompt* (tuple 'defined (tuple 'value prompt))) path) in))
                     ((error reason partial-env)
                        ; fixme, check that the fd is closed!
                        (repl-fail env (list "Could not load" path "because" reason))))))
				(repl-fail env
					(list "Could not find any of" 
						(list path (string-append (env-get env '*owl* "") path))
						"for loading.")))))

	;; load unless already in *loaded*

	(define (repl-require repl path in env)
		(let ((node (ref (ref (get env '*loaded* (tuple 'defined (mkval null))) 2) 2)))
			(if (mem string-eq? node path)
            (repl env in)
				(repl-load repl path in env))))

	(define (repl-op repl op in env)
		(case op	
			((load l)
				(lets ((op in (uncons in False)))
					(cond
						((symbol? op)
							(repl-load repl (symbol->string op) in env))
						((string? op)
							(repl-load repl op in env))
						(else
							(repl-fail env (list "Not loadable: " op))))))
			((forget-all-but)
				(lets ((op in (uncons in False)))
					(if (and (list? op) (all symbol? op))
						(let ((nan (tuple 'defined (tuple 'value 'undefined))))
							(repl
								(ff-fold
									(λ env name val
										(tuple-case val
											((defined x)
												(cond
													((or (primop-of (ref x 2)) 
														(has? op name))
														;(show " + keeping " name)
														env)
													(else 
														;(show " - forgetting " name)
														(del env name))))
											(else env)))
									env env)
								in))
						(repl-fail env (list "bad word list: " op)))))
			((require r)
				; load unless already loaded
				(lets ((op in (uncons in False)))
					(cond
						((symbol? op)
							(repl-require repl (symbol->string op) in env)) 
						((string? op)
							(repl-require repl op in env))
						(else
							(repl-fail env (list "Not loadable: " op))))))
			((words)
				(show "Words: " 
					(ff-fold 
						(λ words key value (cons key words))
						null env))
				(repl env in))
			((quit)
				; this goes to repl-trampoline
				(tuple 'ok 'quitter env))
			(else
				(show "unknown repl op: " op)
				(repl env in))))

	(define (flush-stdout)
      (mail stdout 'flush))

	(define (build-export names env)
		(fold
			(λ module key
				(put module key (get env key 'undefined-lol)))
			False names))

	; fixme, use pattern matching...

	(define (symbol-list? l) (and (list? l) (all symbol? l)))

	(define export?
		(let ((pat `(export . ,symbol-list?)))
			(λ exp (match pat exp))))

	(define (import env mod names)
		(if (null? names)
			(import env mod (map car (ff->list mod)))
			(fold
				(λ env key
					;; could be a bit more descriptive here..
					(put env key (get mod key 'undefined-lol))) 
				env names)))

	(define import? 
		(let ((pattern `(import ,symbol? . ,(λ x True))))
			(λ exp (match pattern exp))))

	(define module-definition?
		(let ((pattern `(define-module ,symbol? . ,(λ x True))))
			(λ exp (match pattern exp))))

	;; a simple eval 

	(define (exported-eval exp env)
		(tuple-case (macro-expand exp env)
			((ok exp env)
				(tuple-case (evaluate-as exp env (list 'evaluating))
					((ok value env) 
						value)
					((fail reason)
						False)))
			((fail reason)
				False)))

	(define (bind-toplevel env)
		(put env '*toplevel* (tuple 'defined (mkval (del env '*toplevel*)))))

	; temp

	(define (push-exports-deeper lst)
		(cond
			((null? lst) lst)
			((export? (car lst))
				(append (cdr lst) (list (car lst))))
			(else
				(cons (car lst)
					(push-exports-deeper (cdr lst))))))

	;; make this threaded this next
	(define (eval-repl exp env repl)
		(tuple-case (macro-expand exp env)
			((ok exp env)
				(cond
					((definition? exp)
						(tuple-case (evaluate (caddr exp) env)
							((ok value env2)
								;; get rid of the meta thread later
								(mail 'meta (tuple 'set-name value (cadr exp)))
								(let ((env (put env (cadr exp) (tuple 'defined (mkval value)))))
									(ok (cadr exp) (bind-toplevel env))))
							((fail reason)
								(fail
									(list "Definition of" (cadr exp) "failed because" reason)))))
					((export? exp)
						(let ((module (build-export (cdr exp) env)))
							(ok module env)))
					((module-definition? exp)
						(tuple-case (repl env (push-exports-deeper (cddr exp)))
							((ok module module-env)
								(ok "module defined"
									(put env (cadr exp) (tuple 'defined (mkval module)))))
							((error reason broken-env)
								(fail
									(list "Module definition of" (cadr exp) "failed because" reason)))))
					((import? exp)
						(tuple-case (evaluate (cadr exp) env)
							((ok mod envx)
								(let ((new (import env mod (cddr exp))))
									(if new
										(ok "imported" new)
										(fail "import failed"))))
							((fail reason)
								(fail (list "library not available: " (cadr exp))))))
					(else
						(evaluate exp env))))
			((fail reason)
				(tuple 'fail 
					(list "Macro expansion failed: " reason)))))

	; (repl env in) -> #(ok value env) | #(error reason env)

	(define (repl env in)
		(let loop ((env env) (in in) (last 'blank))
			(cond
				((null? in)
					(repl-ok env last))
				((pair? in)
					(lets ((this in (uncons in False)))
						(cond
							((eof? this)
								(repl-ok env last))
							((syntax-error? this)
								(repl-fail env (cons "This makes no sense: " (cdr this))))
							((repl-op? this)
								(repl-op repl (cadr this) in env))
							(else
								(tuple-case (eval-repl this env repl)
									((ok result env) 
										(prompt env result)
										(loop env in result))
									((fail reason) 
										(repl-fail env reason)))))))
				(else
					(loop env (in) last)))))

				
	;; run the repl on a fresh input stream, report errors and catch exit

	; silly issue: fd->exp-stream pre-requests input from the fd, and when a syntax error comes, there 
	; already is a request waiting. therefore fd->exp-stream acceps an extra parameter with which 
	; the initial input request can be skipped.

   (define (stdin-sexp-stream bounced?)
      (λ () (fd->exp-stream (fd->id 0) "> " sexp-parser syntax-fail bounced?)))

	(define (repl-trampoline repl env)
		(let boing ((repl repl) (env env) (bounced? False))
			(lets
            ((stdin (stdin-sexp-stream bounced?))
             (stdin (if bounced? stdin (cons "You see a prompt" stdin)))
				 (env (bind-toplevel env)))
				(tuple-case (repl env stdin)
					((ok val env)
						; the end
                  (if (env-get env '*owl-prompt* F)
                     (print "bye bye _o/~"))
						0)
					((error reason env)
						; better luck next time
						(cond
							((list? reason)
								(print-repl-error reason)
								(boing repl env True))
							(else
								(print reason)
								(boing repl env True))))
					(else is foo
						(show "Repl is rambling: " foo)
						(boing repl env True))))))

	(define (repl-port env fd)
      (repl env
         (if (eq? fd stdin)
            (λ () (fd->exp-stream (fd->id 0) "> " sexp-parser syntax-fail F))
            (fd->exp-stream fd "> " sexp-parser syntax-fail F))))
		
	(define (repl-file env path)
		(let ((fd (open-input-file path)))
			(if fd
				(repl-port env fd)
				(tuple 'error "cannot open file" env))))

   (define (repl-string env str)
      (lets ((exps (try-parse (get-kleene+ sexp-parser) (str-iter str) False syntax-fail False)))
         ;; list of sexps
         (if exps
            (repl env exps)
            (tuple 'error "not parseable" env))))
)
