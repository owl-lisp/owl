;;;
;;; MISC CRYPTOGRAPHY
;;;

;; DO NOT USE THESE FOR ANYTHING 
;; this file is only for testing algorithms.
;; DO NOT USE

; ,require "lib/random.scm"

;;;
;;; RSA prototype (based on introduction to algorithms)
;;;

; 1. select two distinct big primes p and q
; 2. n = pq
; 3. select a small e for which gcd(e,phi(n)) = 1
;      - phi(n) = (p-1)*(q-1)
; 4. find e' for which e*e' = 1 (mod (phi n))
;		- is uniquely solvable as a corollary of gcd(e,phi(n)) = 1
;		- can be computed efficiently with extended-euclid 
; 5. publish (e  . n) as the public key
;       keep (e' . n) as the private key
;    private is secret because phi(n) is needed to 
;	  efficiently compute e', and it involves factoring n
;
; messages (being 0 < M < n) can now processed with:
; 	P(M) = expt(M,e) (mod n) = C to encrypt or sign
;  P(C) = expt(C,d) (mod n) = M to decrypt or verify signature
;
; however, you MUST HAVE padding to protect against partial 
; decryption and care in selection of p and q.
; RSASSA-PSS should be a good padding these days

(define-module lib-rsa

	;; create a toy key-pair and try it out

	(export sign public-key private-key)

	(define seed 11111111111111111)

   (define exponent 65537) ; 3 is also common, but this one is at least slightly more secure

	(print "lib-rsa: warning, small primes and seed fixed to " seed)

	(define (biggish-prime rst) 
		(let loop ((rst rst)) 
			(receive (rand rst 10000000000000) 
				(lambda (rst n) 
					(if (prime? n) n (loop (rand-succ rst)))))))

	(define (extended-euclid a b)
		(if (= b 0)
			(values a 1 0)
			(receive (extended-euclid b (rem a b))
				(lambda (dp xp yp)
					(values dp yp (- xp (* yp (div a b))))))))

	; fixme, unforced requirements
	;	- n should really be >= 1024 bits (ie primes >= 512 bits)
	;	- (p-1) and (q-1) should have at least one large prime factor
	;		+ could be enforced by first generating randomly a 200-250-bit
	;		  prime, then a random number, multiplying together and chacking 
	;       primality of successor?
	; 	- p and q should be more than 2n^(1/4) apart to avoid easy factorization  (wikipedia)


	(define p (biggish-prime (rand-succ seed)))
	(define q (biggish-prime (rand-succ (rand-succ seed))))

	(define n (* p q))

	(define phin (* (- p 1) (- q 1))) ; trivial when p and q are known
	
	(define e 
		(let loop ((e exponent))
			(if (= (gcd e phin) 1) e (loop (+ e 2)))))

	(define ep 
		(receive (extended-euclid e phin) 
			(lambda (a x y) (if (< x 0) (+ x phin) x))))

	(if (not (= 1 (rem (* e ep) phin)))
		(error "ras key generation fail: " (list 'p p 'q q 'e e 'ep ep)))

	(define public-key (cons e n))
	(define private-key (cons ep n))

	(print "toy  public key is " public-key)
	(print "toy private key is " private-key)

	(define (crypt-num key num) (expt-mod num (car key) (cdr key)))

	(define decrypt-num crypt-num) ; identical, only key changes

	(define (sign num) (crypt-num private-key num))

	;;; run some tests

	(print "Crypting and decrypting 10 numbers in the range of key " public-key)

	(for-each
		(lambda (a)
			(lets 
				((ac (sign a))
				 (ad (decrypt-num public-key ac)))
				(cond
					((not (= a ad))
						(error "RSA FAIL: " (list a '-> ac '-> ad)))
					((= a ac)
						(print " o " (list a '-> ac '-> ad)))
					(else
						(print " * " (list a '-> ac '-> ad))))))
		(random-numbers 121241415125124514 (cdr public-key) 10))

)






;;;
;;; Blum-Blum-Shub (proto, may have use later)
;;;

; take two distinct (big) primes p and q for which 
;	1. p (mod 4) = 3 
;  2. q (mod 4) = 3
;  3. gcd(phi(p-1), phi(q-1)) is small
; let M = p*q
;     s = some starting value
;	x_(i+1) = (x_i)^2 (mod M)
;  this produces a stream, and taking only low O(log log M) bits 
;  from each x_i produces a stream which should be as hard to
;  decipher as factoring M.


(define-module lib-bbs

	(export encrypt decrypt)

	; fixme, should use $HOME/.my1337key etc and generate it if not there 
	; taking seed from /dev/random (and maybe use a bit bigger primes =)

	(define seed 11111111111111111)

	(print "lib-bbs: warning, seed fixed to " seed)

	(define (candidate-prime rst)
		(let loop ((rst rst))
         (lets ((rst n (rand rst 10000000000)))
            (cond
               ((not (= 3 (band n 3)))
                  (print " bad mod   " n)
                  (loop rst))
               ((not (prime? n))
                  (print " composite " n)
                  (loop rst))
               (else
                  (print "        ok " n)
                  n)))))

	; testing only, make a very small key-pair

	(define key
		(let ((p (candidate-prime seed)))
			(let loop ((rst (rand-succ seed)))
				(let ((q (candidate-prime rst)))
					(let ((score (gcd (totient (- p 1)) (totient (- q 1)))))
						(cond
							((= p q)
								(print " collisition " p)
								(loop (rand-succ rst)))
							((> score 50)
								(print " predecessor phi gcd too high for " (cons p q))
								(print "  - it is " score)
								(loop (rand-succ rst)))
							(else
								(receive (rand rst (* p q))
									(lambda (rst x0)
										(cons x0 (cons p q)))))))))))

	(define m (* (cadr key) (cddr key)))

	(print " the key is " key)
	(print " m is " m)

	(define (step n)
		(rem (* n n) m))

	; can use log log n bits
	;(define (byte a)
	;	(let loop ((a a) (o 0) (n 0))
	;		(if (eq? n 8)
	;			(values a o)
	;			(loop (step a) (bor (<< o 1) (band a 1)) (+ n 1)))))

	(define (byte a)
		(let ((ap (step a)))
			(values (step ap)
				(bor (<< (band a #b1111) 4)
					(band ap #b1111)))))

	(define (encrypt st bytes) ; -> st' + bytes'
		(let loop ((st st) (bytes bytes) (out null))
			(if (null? bytes)
				(values st (reverse out))
				(receive (byte st)
					(lambda (st byte)
						(loop st (cdr bytes) 
							(cons (bxor (car bytes) byte) out)))))))

	(define decrypt encrypt)

	(lets 
		((data (map (lambda (x) (band x #xff)) (iota 0 1 (* 16 1024))))
		 (start (time)))
		(print "bbs encrypting...")
		(receive (encrypt (car key) data)
			(lambda (st l) 
				(let ((end (time)))
					(print "encrypted in " (- end start))
					(if (> (- end start) 0)
						(print "bytes/s " (div (length data) (- end start))))
					(receive (decrypt (car key) l)
						(lambda (st d)
							(if (equal? data d)
								(print "decryption ok")
								(begin
									(print "decrypt FAILS")
									(print "      orig " data)
									(print "   crypted " l)
									(print " decrypted " d)))))))))

)


