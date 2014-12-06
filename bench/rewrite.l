;;;
;;; a simple term rewriter benchmark
;;;

; tags: macro

(define (show a b)
	(display a)
	(display b)
	(display "
"))

(define (error* messages)
	(for-each display messages)
	#f)

(define (memqx sym lst)
	(cond
		((null? lst) #f)
		((eq? (car lst) sym) lst)
		(else (memqx sym (cdr lst)))))

(define (assqx key lst)
	(if (null? lst) 
		#f
		(let ((this (car lst)))
			(if (eq? (car this) key)
				this
				(assqx key (cdr lst))))))

(define (mapx fn lst)
	(if (null? lst)
		lst
		(cons
			(fn (car lst))
			(mapx fn (cdr lst)))))

(define (_equal? a b)
	(or (eq? a b) 
		(and (pair? a) 
			(and (pair? b)
				(and (_equal? (car a) (car b))
					(_equal? (cdr a) (cdr b)))))))
			
(define (unify literals pattern form)
	(let loop ((pattern pattern) (form form) (dictionary '()))
		(cond
			((not dictionary) #f)
			((null? pattern)
				(if (null? form)
					dictionary
					#f))
			((pair? pattern)
				(if (pair? form)
					(loop (cdr pattern) (cdr form)
						(loop (car pattern) (car form) dictionary))
					#f))
			((symbol? pattern)
				(if (memqx pattern literals)
					(if (eq? pattern form) dictionary #f)
					(let ((binding (assqx pattern dictionary)))
						(if binding
							(if (_equal? (cdr binding) form) dictionary #f)
							(cons
								(cons pattern form)
								dictionary)))))
			(else #f))))

(define (rewrite template literals dictionary)
	(let loop ((template template))
		(cond
			((pair? template)
				(cons
					(loop (car template))
					(loop (cdr template))))
			((symbol? template)
				(if (memqx template literals)
					template
					(let ((value (assqx template dictionary)))
						(if value
							(cdr value)
							(error* (list "Unbound variable '" template "' in rewrite template."))))))
			(else template))))

(define (make-rewriter literals rules)
	(lambda (input)
		(let process ((expression input))
			(if (pair? expression)
				(let match-rules ((left rules) (expression (mapx process expression)))
					(if (null? left)
						(let ((new (mapx process expression)))
							(if (_equal? new expression)
								expression
								(process new)))
						(let ((dictionary (unify literals (caar left) expression)))
							(if dictionary
								(match-rules rules (rewrite (cadar left) literals dictionary))
								(match-rules (cdr left) expression)))))
				expression))))

(define-syntax rewriter
	(syntax-rules (->)
		((rewriter ?literals (?from -> ?to) ...)
			(make-rewriter (quote ?literals)
				(list
					(quote (?from ?to)) ...)))))

(define acker
	(rewriter (succ zero ack)
		((ack zero y) -> (succ y))
		((ack (succ x) zero) -> (ack x (succ zero)))
		((ack (succ x) (succ y)) -> (ack x (ack (succ x) y)))))

(define task 
	'(ack (succ (succ (succ zero))) (succ zero)))

(define result
   '(succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ zero))))))))))))))

(define (test args)
	(display "running: ")
	(let loop ((last #f) (i 0))
		(display "* ")
		(if (= i 7)
         (list (if (_equal? last result) 42 0))
			(loop 
				(acker task)
				(+ i 1)))))

test

