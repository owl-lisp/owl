;; fixnum benchmars

(define limit 3000)

(define (f1 a b)
	(if (= a 0)
		b
		(f1 (- a 1) (+ b a))))

(define (f2 n) ; n -> n
	(- (f1 n 0)
		(/ (* n (- n 1)) 2)))

(define (loop n)
	(if (= n limit)
		n
		(loop (+ 1 (f2 n)))))
			
(define (test args)
   (list (- (loop 0) 2958)))

test


