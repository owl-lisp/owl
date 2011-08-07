;;;
;;; COMMAND LINE ARGUMENT HANDLER
;;; 

;; todo: old code, revise

(define-module lib-args

	(export 
      process-arguments 
      format-rules         ;; cl-rules → str
      print-rules          ;; cl-rules → _
      cl-rules)            ;; sexp → cl-rules

	(define (select-rule string rules)
		(if (null? rules) 
			False
			(let ((this (car rules)))
				(if (or (equal? string (ref this 2))
						  (equal? string (ref this 3)))
					this
					(select-rule string (cdr rules))))))

	(define (self x) x)

	(define (explode str)
		(if 
			(and 
				(>= (string-length str) 3)
				(eq? (refb str 0) 45) (not (eq? (refb str 1) 45)))
			(map
				(λ (char)
					(runes->string (list 45 char)))
				(cdr (string->bytes str)))
			False))

	(define (fail fools)
      (mail stderr (foldr renderer '(10) fools))
		False)

	(define blank "nan") ; <- unique because allocated here

	(define (mandatory-args-given? args rules)
		(for True rules
			(λ (ok? rule)
				(if (and (ref rule 8)
						(eq? blank (get args (ref rule 1) blank)))
					(begin
						(show "mandatory option not given: " (ref rule 3))
						False)
					ok?))))

	(define (fill-defaults args rules)
		(for null rules
			(λ (out rule)
				(if (eq? blank (get args (ref rule 1) blank))
					(let ((def (ref rule 5)))
						(if def
							(ilist (ref rule 3) (ref rule 5) out)
							out))
					out))))

	(define (walk rules args dict others)
		(cond 
			((null? args)
				(if (mandatory-args-given? dict rules)
					(let ((args (fill-defaults dict rules)))
						(if (null? args)
							(tuple dict (reverse others))
							(walk rules args dict others)))
					False))
			((string-eq? (car args) "--")
				(walk rules null dict (append (reverse (cdr args)) others)))
			((select-rule (car args) rules) =>
				(λ (rule)
					(lets ((name s l c def cook pred mandatory? single? rule))
						(if cook
							(if (null? (cdr args))
								(fail (list "'" (car args) "' requires an argument."))
								(let ((value (cook (cadr args))))
									(if (pred value)
										; it's ok
										(walk rules (cddr args)
											(put dict name 
												(if single? value (append (get dict name null) (list value))))
											others)
										(fail
											(list "The argument '" (car args) "' did not accept '" (cadr args) "'.")))))
							(walk rules (cdr args) 
                        (put dict name (+ 1 (get dict name 0)))
                        others)))))
			((explode (car args)) =>
				(λ (opts)
					(walk rules (append opts (cdr args)) dict others)))
         ((string-eq? (car args) "-") ;; allow a solitary - to be used as an argument (usually to mean stdin/out)
            (walk rules (cdr args) dict (cons (car args) others)))
			((and 
				(not (string-eq? (car args) ""))
				(eq? (refb (car args) 0) 45))
				(fail (list "Unknown argument: " (car args))))
			(else
				;;; add this to other arguments
				(walk rules (cdr args) dict (cons (car args) others)))))

	(define (process-arguments args rules error-msg cont)
		(let ((res (walk rules args False null)))
			(if res
				(lets ((dict others res))
					(cont dict others))
				(begin
					(print-to error-msg stderr)
					False))))

	;; and now a friendlier way to define the rules 

	(define (cl-rule node lst)
		(if (null? lst)
			node
			(lets ((op lst (uncons lst F)))
				(cond
					((eq? op 'mandatory)
						(cl-rule (set node 8 True) lst))
					((eq? op 'plural)
						(cl-rule (set node 9 False) lst))
					((eq? op 'has-arg)
						(cl-rule node (cons 'cook (cons self lst))))
					((eq? op 'cook)
						(if (and (pair? lst) (function? (car lst)))
							(cl-rule (set node 6 (car lst)) (cdr lst))
							(error "cl-rule: cook is not a function: " (car lst))))
					((eq? op 'check)
						(if (and (pair? lst) (function? (car lst)))
							(cl-rule (set node 7 (car lst)) (cdr lst))
							(error "cl-rule: check is not a function: " (car lst))))
					((eq? op 'default)
						(if (and (pair? lst) (string? (car lst)))
							(cl-rule (set node 5 (car lst)) (cdr lst))
							(error "cl-rule: default is not a string: " (car lst))))
					((eq? op 'comment)
						(if (and (pair? lst) (string? (car lst)))
							(cl-rule (set node 4 (car lst)) (cdr lst))
							(error "cl-rule: comment is not a string: " (car lst))))
               ;((eq? op 'alias) ;; .. alias "-va --native"
               ;   ;; todo
               ;   ...)
					(else
						(error "cl-rule: i do not get this: " lst))))))

	;	(name short long comment default (cook) (predicate) (mandatory?) (single?))
	(define (cl-rules lst)
		(map
			(λ (lst)
				(if (and (>= (length lst) 3) (symbol? (car lst)))
					(cl-rule
						(tuple 
							(car lst) (cadr lst) (caddr lst)
							False
							False False self False True)
						(cdddr lst))
					(error "cl-rules: funny option: " lst)))
			lst))


	;; printing help based on the rules

	(define nl (runes->string '(10)))

   ;; format rule descriptions for printing
   ;; rules → string
   (define (format-rules rules)
		; (print "printing usage")
      (runes->string
         (foldr 
            (λ (rule tl) 
               (foldr (lambda (o t) (render render o t)) tl
                  (list "  " 
                     (let ((short (ref rule 2)))
                        (if short 
                           (string-append short " | ")
                           "     "))
                     (ref rule 3) 
                     (if (ref rule 6) " <arg>" "")
                     (if (ref rule 4)
                        (string-append ", " (ref rule 4))
                        "")
                     (if (ref rule 5)
                        (foldr string-append "]" 
                           (list " [" (ref rule 5)))
                        "")
                     (if (ref rule 8) " (mandatory)" "")
                     (if (not (ref rule 9)) " (can be several)" "")
                     nl)))
            null rules)))

	(define print-rules 
      (o print format-rules))


	;; test

	;(define test-rules
	;	(cl-rules
	;		`(
	;		  (output "-o" "--output" has-arg default "/here" mandatory
	;			  comment "where datas go")
	;		  (in     "-i" "--input" plural has-arg
	;				comment "files to read" )
	;		  (verbose "-v" "--verbose")
	;		  (new-moon "-n" "--new-moon" comment "refresh the moon")
	;		)))
	;
	;(print-rules test-rules)
	;
	;(process-arguments 
	;	(list "-vi" "eka.file" "--input" "toka.file" "-vo" "glurg/zot" "--" "-visual")
	;	test-rules "YOU LOSE"
	;	(lambda (args other) (show " - processed args " args) (show " - others " other)))

)




