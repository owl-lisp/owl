;;;
;;; COMMAND LINE ARGUMENT HANDLER
;;; 

(define-module lib-args

	(export 
      process-arguments    ;; sexp → cl-rules
      format-rules         ;; cl-rules → str
      print-rules          ;; cl-rules → _
      cl-rules)            ;; sexp → cl-rules

   ;; cl-rules is a ff of 
   ;;   'short -> -x
   ;;   'long  -> --xylitol

   (define (renderer a b) 
      (render render a b))

   ;; str (rule-ff ..) → False | rule-ff
	(define (select-rule string rules)
		(if (null? rules) 
			False
			(let ((this (car rules)))
            (if (or (equal? string (getf this 'short))
                    (equal? string (getf this 'long)))
					this
					(select-rule string (cdr rules))))))

	(define (self x) x)

   ;; "-foo" → ("-f" "-o" "-o") | False
	(define (explode str)
      (if (m/^-[^-]{2,}/ str)
			(map
				(λ (char) (runes->string (list 45 char)))
				(cdr (string->bytes str)))
			False))

	(define (fail fools)
      (mail stderr (foldr renderer '(10) fools))
		False)

	(define blank "nan") ; <- unique because allocated here

   (define (undefined? ff key)
      (eq? blank (get ff key blank)))

   (define (defined? ff key) 
      (not (undefined? ff key)))

   ;; check that all rules which are marked mandatory have the corresponding id defined in dict
	(define (mandatory-args-given? dict rules)
      (fold
         (λ (ok? rule)
            (if (getf rule 'mandatory) ;; this is mandatory
               (if (undefined? dict (getf rule 'id))
                  ok?
                  (begin
                     (mail stderr 
                        (foldr (λ (a b) (render render a b)) '(10) 
                           (list "mandatory option not given: " (get rule 'long "(missing)"))))
                     False))
               ok?))
         True rules))

   ;; set set all default which are not set explicitly
	(define (fill-defaults dict rules)
      (fold 
         (λ (dict rule)
            (let ((id (getf rule 'id)))
               (if (and (undefined? dict id) (defined? rule 'default))
                  (put dict id 
                     (let ((cookd ((getf rule 'cook) (getf rule 'default))))
                        (if (getf rule 'plural) (list cookd) cookd))) ; <- a single plural default value needs to be listed
                  dict)))
         dict rules))

   ;; a fast /^-/ to shave some startup ms for thousands of arguments, which are getting common for some tools
   (define (dashy? str)
      (let ((s (sizeb str)))
         (if (lesser? s 1)
            F
            (eq? 45 (refb str 0)))))
      
	(define (walk rules args dict others)
		(cond 
			((null? args)
				(if (mandatory-args-given? dict rules)
               (tuple (fill-defaults dict rules) (reverse others))
					False))
         ((dashy? (car args))
            (cond
               ((string-eq? (car args) "--")
                  (walk rules null dict (append (reverse (cdr args)) others)))
               ((select-rule (car args) rules) =>
                  (λ (rule)
                     (lets
                        ((cook (getf rule 'cook))
                         (id (getf rule 'id)))
                        (if cook ;; <- set if this expects an argument
                           (if (null? (cdr args))
                              (fail (list "'" (car args) "' requires an argument."))
                              (lets
                                 ((value (cook (cadr args)))
                                  (ok? ((get rule 'pred (λ (x) x)) value)))
                                 (if ok?
                                    (walk rules 
                                       ;; instert an implicit -- after terminal rules to stop
                                       (if (getf rule 'terminal) (cons "--" (cddr args)) (cddr args))
                                       (put dict id
                                          (if (getf rule 'plural) 
                                             ;; put values to a list if this is a multi argument
                                             (append (get dict id null) (list value))
                                             value))
                                       others)
                                    (fail
                                       (list "The argument '" (car args) "' did not accept '" (cadr args) "'.")))))
                           ;; this doesn't have an argument, just count them
                           (walk rules (cdr args) 
                              (put dict id (+ 1 (get dict id 0)))
                              others)))))
               ((explode (car args)) =>
                  (λ (opts) ;; --foo → -f -o -o
                     (walk rules (append opts (cdr args)) dict others)))
               ((string-eq? (car args) "-") ;; allow a solitary - to be used as an argument (usually to mean stdin/out)
                  (walk rules (cdr args) dict (cons (car args) others)))
               (else
                  (fail (list "Unknown argument: " (car args))))))
			(else
				;;; add this to other arguments
				(walk rules (cdr args) dict (cons (car args) others)))))

   ; + cook, pred, terminal, multi, id

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
					((eq? op 'mandatory) (cl-rule (put node op T) lst))
					((eq? op 'plural)    (cl-rule (put node 'plural T) lst))
					((eq? op 'terminal)  (cl-rule (put node op T) lst))
					((eq? op 'has-arg) ;; short for cook id
						(cl-rule node (ilist 'cook self lst)))
					((eq? op 'cook)
						(if (and (pair? lst) (function? (car lst)))
							(cl-rule (put node 'cook (car lst)) (cdr lst))
							(error "cl-rule: cook is not a function: " (car lst))))
					((eq? op 'check)
						(if (and (pair? lst) (function? (car lst)))
							(cl-rule (put node op (car lst)) (cdr lst))
							(error "cl-rule: check is not a function: " (car lst))))
					((eq? op 'default)
						(if (and (pair? lst) (string? (car lst)))
							(cl-rule (put node op (car lst)) (cdr lst))
							(error "cl-rule: default is not a string: " (car lst))))
					((eq? op 'comment)
						(if (and (pair? lst) (string? (car lst)))
							(cl-rule (put node op (car lst)) (cdr lst))
							(error "cl-rule: comment is not a string: " (car lst))))
					(else
						(error "cl-rule: i do not get this: " lst))))))

	;	(name short long comment default (cook) (predicate) (mandatory?) (single?) (terminal?))
	(define (cl-rules lst)
		(map
			(λ (lst)
				(if (and (>= (length lst) 3) (symbol? (car lst)))
					(cl-rule 
                  (list->ff (zip cons '(id short long) lst))
                  (cdddr lst))
					(error "cl-rules: funny option: " lst)))
			lst))

	;; printing help based on the rules

	(define nl (runes->string '(10)))

   ;; format rule descriptions for printing
   ;; rules → string
   (define (format-rules rules)
      (runes->string
         (foldr 
            (λ (rule tl) 
               (foldr 
                  (lambda (o t) 
                     (render render o t))
                  tl
                  (list "  " 
                     (let ((short (getf rule 'short)))
                        (if short 
                           (string-append short " | ")
                           "     "))
                     (getf rule 'long)
                     (if (getf rule 'cook) " <arg>" "")
                     (if (getf rule 'comment)
                        (string-append ", " (getf rule 'comment))
                        "")
                     (if (getf rule 'default)
                        (foldr string-append "]" 
                           (list " [" (getf rule 'default)))
                        "")
                     (if (getf rule 'mandatory) " (mandatory)" "")
                     (if (getf rule 'plural) " (can be several)" "")
                     (if (getf rule 'terminal) " (terminal)" "")
                     nl)))
            null rules)))

	(define print-rules 
      (o display format-rules))

)


(import-old lib-args)

(define cli-args
   (cl-rules
       `((output "-o" "--output" has-arg default "/where/to"
            comment "path or pattern where to write the data.")
         (count "-n" "--count" cook ,string->integer check ,(λ (x) (> x 0))
            default "1"
            comment "how many files to generate.")
         (seed "-j" "--jiffy" has-arg plural comment "the jiffies")
         (help "-h" "--help")
         (verbose "-v" "--verbose")
         )))

(define (main args)
   (print "Trying to parse using:")
   (for-each (λ (x) (show " - " x)) cli-args)
   (process-arguments (cdr args) cli-args "madness!!1" 
      (λ (dict others)
         (print "args ok.")
         (show " - dict " dict)
         (show " - others " others)
         (if (getf dict 'help)
            (print-rules cli-args)
         ))))

;(main '("ol" "-hhvn" "10"))
main
