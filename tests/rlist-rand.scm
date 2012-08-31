;;;
;;; a bit of testing
;;;

; lset could be on toplevel

(define (lset l p v)
	(if (= p 0)
		(cons v (cdr l))
		(cons (car l) (lset (cdr l) (- p 1) v))))

(define max-list-size 10000)

; run a battery of operations to a list and an equal rlist
; and stop on inconsistencies 

(define (list-test rst rl l len steps)
	(if (= steps 0)
		(print "ok")
		(lets ((rst n (rand rst 25))
				 (steps (- steps 1)))
			;; draw a graph of the list length 
			;(mail stdout (fold (lambda (out n) (cons 45 out)) '(124 10) (iota 0 1 len)))
			(case n
				((0 1 2) ; cons a new head
					(if (< len max-list-size)
						(lets ((rst x (rand rst 1000)))
							(list-test rst (rcons x rl) (cons x l) (+ len 1) steps))
						(list-test rst rl l len steps)))
				((3 4 5) ; drop a head, slightly less frequent 
					(if (= len 0)
						(list-test rst rl l len steps)
						(list-test rst (rcdr rl) (cdr l) (- len 1) steps)))
				((7 8) ; check car
					(cond
						((= len 0) (list-test rst rl l len steps))
						((eq? (rcar rl) (car l)) (list-test rst rl l len steps))
						(else (error "heads differ: " (list (rcar rl) (car l) steps)))))
				((9 10) ; set a random element
					(if (= len 0)
						(list-test rst rl l len steps)
						(lets 
							((rst p (rand rst len))
							 (rst v (rand rst 10000)))
							;(print* (list "L[" p "] = " v))
							(list-test rst (rset rl p v) (lset l p v) len steps))))
				((11) ; map increment
					(list-test rst
						(rmap (lambda (x) (+ x 1)) rl)
						(map (lambda (x) (+ x 1)) l)
						len steps))
				((12) ; fold
					(if (= (fold - 0 l) (rfold - 0 rl))
						(list-test rst rl l len steps)
						(error "folds fail at len " len)))
				((13) ; foldr
					(if (= (foldr - 0 l) (rfoldr - 0 rl))
						(list-test rst rl l len steps)
						(error "foldrs fail at len " len)))
				((14) ; cross convert
					(list-test rst
						(list->rlist l)
						(rlist->list rl) len steps))
				((15) ; iter test
					(if (equal? (fold - 0 l) (lfold - 0 (riter rl)))
						(list-test rst rl l len steps)
						(error "riter fails at len " len)))
				((16) ; iterr test
					(if (equal? (foldr - 0 l) (lfoldr - 0 (riterr rl)))
						(list-test rst rl l len steps)
						(error "riterr fails at len " len)))
				((17)
					(if (= (rlen rl) len)
						(list-test rst rl l len steps)
						(error "length fail: " (list 'got (rlen rl) 'for len))))
				(else ; check a random element
					(if (> len 0)
						(lets ((rst p (rand rst len)))
							(if (eq? (rget rl p 'nan) (lref l p))
								(list-test rst rl l len steps)
								(error "elems differ: " p)))
						(list-test rst rl l len steps)))))))

(define (test-ops)
	(list-test (seed->rands (time-ms)) null null 0 10000))

(test-ops)

