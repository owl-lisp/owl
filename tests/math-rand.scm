;;;
;;; Math library unit tests
;;;

;; todo: factor this to smaller tests

(import (owl math))
(import (owl math-extra))

; fixme, put abs to math

(define (abs x)
	(if (< x 0) (- 0 x) x))

;; take a random seed

(define seed (expt (time-ms) 3))

;; the rest is deterministic

;;;
;;; Test subject creation
;;;

; note, karatsuba kicks in when numbers are around 100 digits, 
; and the same will probably hold for faster versions of other 
; algorithms later

;(define max-bits 2048)
(define max-bits 128)

(define (rand-bits rst bits)
	(rand rst (<< 1 bits)))

(define (nat rst)
	(lets ((rst type (rand rst 50)))
		(cond
			((eq? type 0) (rand-bits rst max-bits))
			((eq? type 1) (rand-bits rst 128))
			((eq? type 2) (rand-bits rst 64))
			((eq? type 3) (rand-bits rst 16))
			; make the usual suspect a small bignum (n > 65536)
			(else (rand-bits rst 32)))))

(define (nat-nz rst) ; nonzero
	(receive (nat rst)
		(lambda (rst n)
			(if (= n 0)
				(nat-nz rst)
				(values rst n)))))

(define (int rst)
	(receive (rand rst 2)
		(lambda (rst s)
			(receive (nat rst)
				(lambda (rst n)
					(values rst
						(if (eq? s 0) n (- 0 n))))))))

(define (int-nz rst) ; nonzero
	(receive (int rst)
		(lambda (rst n)
			(if (= n 0)
				(int-nz rst)
				(values rst n)))))

(define (rat rst)
	(receive (int rst)
		(lambda (rst a)
			(receive (int-nz rst)
				(lambda (rst b)
					(values rst (/ a b)))))))

(define (rat-nz rst) ; nonzero
	(receive (int rst)
		(lambda (rst n)
			(if (= n 0)
				(rat-nz rst)
				(values rst n)))))

(define (comp rst)
   (lets
      ((rs r (rat rst))
       (rs i (rat-nz rst)))
      (values rs (complex r i))))

(define (comp-nz rst)
   (lets ((rs c (comp rst)))
      (if (= c 0)
         (comp-nz rs)
         (values rs c))))

(define (fixnum rs)
   (lets ((d rs (uncons rs #false)))
      (values rs d)))

(define (fixnum-nz rs)
   (lets ((d rs (uncons rs #false)))
      (if (eq? d 0)
         (fixnum-nz rs)
         (values rs d))))

(define (any rs)
   (lets ((rs type (rand rs 5)))
      (case type
         ((0) (fixnum rs)) 
         ((1) (nat rs))
         ((2) (int rs))
         ((3) (rat rs))
         ((4) (comp rs)))))

(define (any-nz rs)
   (lets ((rs n (any rs)))
      (if (= n 0)
         (any rs)
         (values rs n))))

;; all numbers are funny and some more than others

(define funny-numbers
	(let*
		((ns 
			(map (lambda (x) (<< 1 x))
				(map (lambda (x) (expt 2 x)) (iota 0 1 6))))
		 (ns (append ns (map (lambda (x) (- x 1)) ns)))
		 (ns (append ns (map (lambda (x) (- 0 x)) ns))))
		; positive first
		(sort > (cons 0 ns))))

(define simple-numbers
	(map (lambda (x) (expt 2 x)) (iota 0 1 17)))

;;; 
;;;  Unit tests 
;;; 

; approximate a bunch of Forall x,y,... P(x,y,...)
; by checking several Exists x,y,... P(x,y,...) in the domain


(define math-tests
	(list
		(tuple 'unary any 'add-double
			(lambda (x) (= (+ x x) (* x 2))))
		(tuple 'unary any 'add-double-int
			(lambda (x) (= (+ x x) (* x 2))))
		(tuple 'unary any-nz 'div-self-one
			(lambda (x) (= 1 (/ x x))))
		(tuple 'unary rat 'succ-greater
			(lambda (x) (> (+ x 1) x)))	;; FIXME, make rat
		(tuple 'unary rat 'pred-lesser
			(lambda (x) (< (- x 1) x)))	;; FIXME, ditto
		(tuple 'binary any any 'mul-add-1
			(lambda (a b) (= (* a (+ b 1)) (+ (* a b) a))))
		(tuple 'binary any any 'add-comm
			(lambda (a b) (= (+ a b) (+ b a))))
		(tuple 'binary any any 'mul-comm
			(lambda (a b) (= (* a b) (* b a))))
		(tuple 'binary any any 'add-cancel
			(lambda (a b) (= a (- (+ a b) b))))
		(tuple 'binary any any-nz 'mul-div-cancel	
			(lambda (a b) (= a (/ (* a b) b))))
		(tuple 'binary any any-nz 'div-mul-cancel 
			(lambda (a b) (= a (* (/ a b) b))))
		(tuple 'binary any any 'add-trans-one	
			(lambda (a b) (= (+ a (+ b 1)) (+ (+ a b) 1))))
		(tuple 'binary any any 'mul-trans-two 
			(lambda (a b) (= (* a (* b 2)) (* (* a b) 2))))
		(tuple 'binary any any-nz 'div-twice 
			(lambda (a b)	(= (/ (/ a b) b) (/ a (* b b)))))
		(tuple 'binary int int-nz 'rem-abs-less 
			(lambda (a b) (< (abs (rem a b)) (abs b))))
		(tuple 'binary int int-nz 'a=qb+r 	
			(lambda (a b) (= a (+ (* (div a b) b) (rem a b)))))
		(tuple 'binary int nat 'shift-cancel 
			(lambda (a b)
				(let ((b (band b #x1ff))) (= a (>> (<< a b) b)))))
		(tuple 'binary int nat 'shift-is-expt
			(lambda (a b)
				(let ((b (band b #xff)))
					(= (<< a b) (* a (expt 2 b))))))
		(tuple 'binary int int-nz 'quotrem=quot-rem	
			(lambda (a b) 
				(receive (quotrem a b)
					(lambda (q r)
						(and (= q (div a b)) (= r (rem a b)))))))
		(tuple 'binary nat nat 'xor-trans
			(lambda (a b) (= (bxor (bxor a b)  b) (bxor a (bxor b b)))))
		(tuple 'binary nat-nz nat-nz 'logarithm
			(lambda (n a) (lets ((n (max 2 n)) (m (log n a))) (and (>= (expt n m) a) (<= (expt n (max 0 (- m 1))) a)))))
		(tuple 'binary int int 'gcd-swap 
			(lambda (a b) (= (gcd a b) (gcd b a))))
		(tuple 'binary int int 'gcd-sign 
			(lambda (a b) (= (gcd (- 0 a) b) (gcd a b))))
		(tuple 'unary int 'gcd-zero 
			(lambda (a) (= (gcd a 0) (abs a))))
		(tuple 'binary int int 'gcd-sign 
			(lambda (a b) (= (gcd a (* a b)) (abs a))))
		(tuple 'binary nat nat 'bitwise-misc
			(lambda (a b) (= a (band a (bor a (bxor b b))))))
		(tuple 'binary int-nz int-nz 'gcd-divides
			(lambda (a b)
				(let ((gab (gcd a b)))
					(and (ediv a gab) (ediv b gab)))))
		(tuple 'binary nat nat-nz 'mul-ediv-cancel
			(lambda (a b) (= a (ediv (* a b) b))))
		;(tuple 'binary nat nat-nz 'invmod-ok ; fixme, ints
		;	(lambda (a b)
		;		(let ((ai (inv-mod a b)))
		;			(if ai (= (rem (* a ai) b) 1) #true))))
		(tuple 'unary nat-nz 'isqrt-ok
			(lambda (a)
				(if (eq? a 1) #true
					(= a (isqrt (* a a))))))
		(tuple 'trinary any any any 'add-assoc
			(lambda (a b c)
				(= (+ (+ a b) c) (+ a (+ b c)))))
		(tuple 'trinary any any any 'mul-assoc
			(lambda (a b c)
				(= (* (* a b) c) (* a (* b c)))))
		(tuple 'trinary any any any 'mul-add
			(lambda (a b c)
				(= (* a (+ b c)) (+ (* a b) (* a c)))))
		;(tuple 'trinary nat nat nat-nz 'discrete-log ; fixme, ints
		;	(lambda (a b c)
		;		(let*
		;			((n (+ 2 (rem c 1000)))
		;			 (y (rem a n))
		;			 (a (rem b n))
		;			 (z (print (list 'dlog y a n)))
		;			 (x (dlog y a n)))
		;			(if x
		;				(if (= y (expt-mod a x n))
		;					(begin
		;						(print "dlp ok " (tuple y a x n))
		;						#true)
		;					#false)
		;				#true))))
))

(define (run-test rst test)
	;(mail stdout 42) (flush-port 1)
	(tuple-case test
		((unary gen-a name test)
			(receive (gen-a rst)
				(lambda (rst a)
					;(print (list name a))
					(if (test a) 
						#true 
						(error "Math unreliable: " (list 'test name 'a a 'rst rst))))))
		((binary gen-a gen-b name test)
			(let* 
				(((rst a) (gen-a rst))
				 ((rst b) (gen-b rst)))
				;(print (list name a b))
				(if (test a b) 
					#true 
					(error "Math unreliable: " 
						(list 'test name 'a a 'b b 'rst rst)))))
		((trinary gen-a gen-b gen-c name test)
			(let* 
				(((rst a) (gen-a rst))
				 ((rst b) (gen-b rst))
				 ((rst c) (gen-c rst)))
				;(print (list name a b c))
				(if (test a b c) 
					#true 
					(error "Math unreliable: " 
						(list 'test name 'a a 'b b 'c c 'rst rst)))))
		(else
			(error "Bad test: " test))))

(define (run-tests rst step)
   (if (= step 0)
      (print "Tests complete.")
      (begin
         (for-each (lambda (x) (run-test rst x)) math-tests)
         (lets ((rst n (rand rst 10000000)))
            (run-tests rst (- step 1))))))

(define (type-ok? gen n) ; n is an integer from funny numbers
	(cond
		((eq? gen rat) #true)	
		((eq? gen rat-nz) (not (= n 0)))
		((eq? gen nat) (>= n 0))
		((eq? gen nat-nz) (> n 0))
		((eq? gen int) #true)
		((eq? gen int-nz) (not (= n 0)))
		((eq? gen comp) #true)
		((eq? gen comp-nz) (not (= n 0)))
		((eq? gen any) #true)
		((eq? gen any-nz) (not (= n 0)))
		(else (error "type-ok: unknown generator: " gen))))

(define (run-cartesian-test nums msg)
	(print msg)
	(for 1 nums
		(lambda (x a)
			(print " * " a)
			(for 1 math-tests
				(lambda (x test-node)
					(tuple-case test-node
						((unary ta name test)
							(if (type-ok? ta a)
								(if (not (test a))
									(error "Funny test failed: " 
										(list 'test name 'using a)))))
						((binary ta tb name test)
							(for 1 nums
								(lambda (x b)
									(if (and (type-ok? ta a) (type-ok? tb b))
										(if (not (test a b))
											(error "Funny test failed: " 
												(list 'test name 'using a b)))))))
						((trinary ta tb tc name test)
							(for 1 nums
								(lambda (x b)
									(for 1 nums
										(lambda (x c)
											(if (and (type-ok? ta a) (type-ok? tb b)
														(type-ok? tc c))
												(if (not (test a b c))
													(error "Funny test failed: " 
														(list 'test name 
															'using a b c)))))))))
						(else
							(print "run-funny-tests: too funny test: " 
								test-node))))))))


;; run tests until an error occurs

(begin
	; these often find the stupidest issus when hacking bignums
	(print "Running simple number tests:")
	(run-cartesian-test simple-numbers "Testing simple cases")
	; these often find less trivial issues 
	(print "Running funny number tests:")
	(run-cartesian-test funny-numbers "Testing cartesian products of funny numbers")
	; these occasionally dig out issues
	(print "Running random tests:")
	(run-tests (seed->rands seed) 50)
	)

