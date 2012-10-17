(define seed (* (time-ms) (expt (time-ms) 4)))

; (print seed)

(define (try-perm rst)
	(lets
		((rst n (rand-range rst 1 200))
		 (keys (iota 0 1 n)) 

		 ;; create a few permutations of numbers
		 (rst perm-1 (random-permutation rst keys))
		 (rst perm-2 (random-permutation rst keys))

		 ;; create a read-black tree in random order
		 (ff (fold (lambda (ff n) (put ff n n)) empty perm-1))

		 ;; delete keys in insertion, reverse, rtl and ltr order
		 (blank-fifo (fold (lambda (ff n) (del ff n)) ff perm-1))
		 (blank-lifo (fold (lambda (ff n) (del ff n)) ff (reverse perm-1)))
		 (blank-random (fold (lambda (ff n) (del ff n)) ff perm-2))
		 (blank-inorder (fold (lambda (ff n) (del ff n)) ff keys))
		 (blank-reverse (fold (lambda (ff n) (del ff n)) ff (reverse keys))))
		(cond
			((not (equal? (map car (ff->list ff)) keys))
				(error "bad ff for keys " perm-1))
			((not (empty? blank-fifo)) (error "delete fifo tail " perm-1))
			((not (empty? blank-lifo)) (error "delete lifo tail " perm-1))
			((not (empty? blank-random)) (error "random delete fail: " (list 'keys perm-1 'delete 'order perm-2)))
			((not (empty? blank-inorder)) (error "left to right deletion fails: " perm-1))
			((not (empty? blank-reverse)) (error "right to left deletion fails: " perm-1))
			(else rst))))

(let loop ((rst (seed->rands (time-ms))) (n 0))
	(if (= n 100)
		(print (list 'ok n))
		(loop (try-perm rst) (+ n 1))))
		
