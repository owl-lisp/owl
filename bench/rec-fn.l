
;;; simple function calls 

(define (z f) (lambda (x) (f (f (f x)))))

(define levels '(o o o o o o o o o o o o o o x))

(define (gen f l)
	(if (null? l)
		f
		(gen (z f) (cdr l))))

(define func (gen (lambda (x) x) levels))

(define (test args)
	(list (func 42)))

test


