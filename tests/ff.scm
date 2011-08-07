;; a generic finite function (red-black key-value-tree) insertion/deletion test


(define (lput lst k v)
	(if (null? lst)
		(list (cons k v))
		(let ((this (caar lst)))
			(cond
				((eq? this k)
					(cons (cons k v) (cdr lst)))
				((lesser? this k)
					(cons (car lst)
						(lput (cdr lst) k v)))
				(else
					(cons (cons k v) lst))))))

(define (ldel lst k)
	(if (null? lst)
		lst
		(let ((this (caar lst)))
			(cond
				((eq? this k) (cdr lst))
				((lesser? this k)
					(cons (car lst) (ldel (cdr lst) k)))
				(else lst)))))

(define (check-equal? ff lst alpha)
	(if (equal? (ff->list ff) lst)
		True
		(begin
			(print (list 'badness 'alpha alpha 'ff ff 'lst lst))
			False)))

(define (work rst ff lst end alpha)
	(if (> (time-ms) end)
		(if (check-equal? ff lst alpha)
			(print "ok"))
		(lets ((rst n (rnd rst 3)))
			(cond
				((eq? n 0) ;; insert
					(lets 
						((rst k (rnd rst alpha))
						 (rst v (rnd rst alpha)))
						(work rst (put ff k v) (lput lst k v) end alpha)))
				((eq? n 1) ;; delete
					(lets ((rs k (rnd rst alpha)))
						(work rst (del ff k) (ldel lst k) end alpha)))
				((eq? n 2) ;; compare
					(if (check-equal? ff lst alpha)
						(work rst ff lst end alpha)))))))

(lets
	((rst (seed->rands (time-ms)))
	 (rst seed (rnd rst 100000000000))
	 (rst (seed->rands seed))
	 (rst n (rnd-range rst 4 15))
	 (rst alpha (rnd-nbit rst n)))
	(work rst False null (+ (time-ms) 100) alpha))
		
