;;;
;;; benchmark continuations and function calls
;;;

; tags: cont

(define i (lambda (x) x))

(define levels '(o o o o o o o o o o o o o o))

(define (op f)
	(lambda (c a)
		(call/cc
			(lambda (r)
				(c 
					((call/cc (lambda (c) (f c r)))
						((call/cc (lambda (c) (f c i)))
						 (call/cc (lambda (c) (f c a))))))))))

(define (gen n)
	(if (null? n)
		(lambda (c a) (c a))
		(op (gen (cdr n)))))

(define (test args)
   (list ((gen levels) i 42)))

test

