;;; 
;;; test cost per bit for operations
;;; 

; todo
;	- calibrate blank run times with (lambda (a b) 42)
;		+ these cause some skew especially for the initial small numbers

(import-old lib-random)

(define (rand-nbits rst n)
	(lets
		((max (<< 1 (- n 1)))
		 (rst n (rand rst max)))
		(values rst (bor n max))))

(define (random-nbit-numbers rst n len)
	(let loop ((rst rst) (out null) (len len))
		(if (= len 0)
			out
			(lets ((rst x (rand-nbits rst n)))
				(loop rst (cons x out) (- len 1))))))

(define (ms) (lets ((s ms (clock))) (+ (* s 1000) ms)))

(define (try-pairs op l)
	(for-each 
		(lambda (a)
			(for-each (lambda (b) (op a b)) l))
		l))

(define (test op bits)
	(let loop ((n 1))
		(lets
			((nums (random-nbit-numbers (* (ms) bits) bits n))
			 (start (ms))
			 (run (try-pairs op nums))
			 (elapsed (- (ms) start))
			 (runs (* n n)))
			;(print* (list elapsed " ms for bits " bits " and len " n))
			(if (and (> elapsed 1000) (> runs 10))
				(print* (list " * " bits " => " (div runs elapsed) " runs/ms and " (div elapsed runs) "ms/run, " (div (* runs (* bits 2)) elapsed) " bits/ms"))
				(loop (* n 2))))))

;(let loop ((bits 16)) (test + bits) (loop (+ bits 1)))
;(let loop ((bits 16)) (test * bits) (loop (+ bits 1)))
;(let loop ((bits 16)) (test ediv bits) (loop (+ bits 1)))
;(let loop ((bits 16)) (test gcd bits) (loop (+ bits 1)))
(let loop ((bits 16)) (test / bits) (loop (+ bits 1)))

