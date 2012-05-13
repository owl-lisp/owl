;;;
;;; A simple algorithm benchmarking/correctness library
;;;

; todo
;	- compare should show 2.21x instead of percentage
;	- and a logarithmically scaled  chart with ansi colors
;	-  [                    ****|                          ]  2.21x (avg 1.91x) - rand
;	-  [                   *****|                          ]  2.31x (avg 1.92x) - dec
;	-  [                        |*                         ] -0.19x (avg 1.92x) - ord
;	-  [                      **|                          ]  1.02  (avg 1.92x) - rand

(define-module lib-test
	(export
		test				; vals x a x b -> simple comparison (equality and speed), show stats and return on differences
		compare  		; ((msg . val) ...) fn1 fn2 -> speed (and result equivalence) test
		time				; timing macro for single-valued computations
		elapsed-ms		; thunk -> ms + val
		)

	;(import-old lib-lazy)

	(define (elapsed-ms thunk) ; -> ms + value
		(lets
			((start (time-ms))
			 (result (thunk))
			 (elapsed (- (time-ms) start)))
			(values elapsed result)))

	(define-syntax time
		(syntax-rules ()
			((time op)
				(time op "elapsed time: "))
			((time op comment ...)
				(lets ((ms val (elapsed-ms (lambda () op))))
					(for-each display (list comment ...))
					(print ms "ms")
					val))))

	(define (percentage ams bms)
		(cond
			((= bms 0) 0)
			((> ams bms) (- 0 (percentage bms ams)))
			(else (floor (* 100 (/ (- bms ams) bms))))))
		
	(define (score-of a b)
		(cond
			((<= (+ a b) 2) 0) ; too little to be of interest
			((< a b)
				(if (= a 0)
					100
					(- (floor (* 100 (/ b a))) 100)))
			((= a b) 0)
			(else
				(- 0 (score-of b a)))))

	(define (try a b txt val total) ; -> (n-tests+1 . n-avg')
		(lets ((n-tests a-total b-total total))
			(if (list? txt)
				(for-each display txt)
				(display txt))
			(flush-port 1)
			(lets
				((ams av (elapsed-ms (lambda () (a val))))
				 (bms bv (elapsed-ms (lambda () (b val))))
				 (n-tests (+ 1 n-tests))
				 (a-total (+ a-total ams))
				 (b-total (+ b-total bms))
				 (score (score-of ams bms)))
				(print* (list " at " ams "ms vs " bms "ms, " 
					" first is " score "% better (average " (score-of a-total b-total) "%)"))
				(if (equal? av bv)
					(tuple n-tests a-total b-total)
					(error "results differ" (list val 'gives av 'and bv))))))
	
	(define (compare cases a b)
		(lfold
			(lambda (total val)
				(try a b (car val) (cdr val) total))
			(tuple 0 0 0) cases))

	(define (test cases a b)
		(print "Testing ops:")
		(call/cc
			(λ (ret)
				(lfold
					(λ (stats case)
						(lets
							((ams bms stats)
							 (elapsed-a this-a (elapsed-ms (λ () (a case))))
							 (elapsed-b this-b (elapsed-ms (λ () (b case)))))
							(print* 
								(list "  " (div (* (max ams bms) 100) (max 1 (min ams bms))) "% diff in total " ams "ms/" bms "ms, here "
									elapsed-a "ms/" elapsed-b "ms"))
							(if (equal? this-a this-b)
								(tuple (+ ams elapsed-a) (+ bms elapsed-b))
								(begin
									(print "results differ for " case)
									(ret (list 'a this-a 'b this-b))))))
					(tuple 0 0) cases))))

	;; examples for quick checks speed/correctness tests (with test)
	;
	;(import-old lib-random)
	;(test 
	;	(lmap (λ (i) (lets ((rst n (rand i 10000000))) n)) (lnums 1))
	;	(λ (n) (prime? n))
	;	(λ (n) (let ((f (factor n))) (and (null? (cdr f)) (= 1 (cdar f))))))

   ;(define nums (iota 0 1 100000))
   ;(define ff (fold (λ (ff n) (put ff n n)) #false nums))
   ;(test
	;	(lmap (λ (i) (lets ((rst n (rnd (seed->rands i) #x10000))) n)) (lnums 1))
   ;   (λ (n) (fold (λ (ff i) (fupd ff i i)) ff nums))
   ;   (λ (n) (fold (λ (ff i) (put ff i i)) ff nums)))
      
	;(import-old lib-random)
	;(test 
	;	(lmap (λ (i) (lets ((rst n (rand i 10000))) n)) (lnums 1))
	;	(λ (n) (<< 1 n))
	;	(λ (n) (expt 2 n)))
)


