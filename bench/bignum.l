; benchmark basic math operations with bignums (integers only, rationals separately)

; addition and multiplication only atm 

; tags: bignum

(define n (expt 3 10000))

;; bignum addition

; note, owl has amortized O(log n) addition. it would be easy to make it look really 
; good by simply adding a smaller number to a *really* big one many times. to avoid 
; this effect, the benchmark uses roughly equally large numbers, making owl look 
; really bad (for now).

(define x (expt 2 128))

; (f1 n) = (<< 2 (+ 127 n))
(define (f1 i)
	(if (eq? i 0)
		x
		(+ (f1 (- i 1))
			(f1 (- i 1)))))

(define (c1 i)
   (expt 2 (+ 128 i)))
   
;; bignum multiplication 

(define (f3 n)
   (if (= n 1)
      x
      (* x (f3 (- n 1)))))

(define (c3 n)
   (expt x n))

;; bignum division

;(define m (expt 3 100))

;(define (f4 i)
;	(if (eq? i 0)
;		n
;		(let ((x (f4 (- i 1))))
;			(/ x m)))) ; ediv is somewhat ok, but plain division is still awfully slow

(define (test args)
   (let 
      ((a  (f1 17))
       (ap (c1 17))
       (b  (f3 200))
       (bp (c3 200)))
      (list (+ 42 (- (+ a b) (+ ap bp))))))

test
