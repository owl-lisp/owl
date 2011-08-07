;;; 
;;; A simple checksummer
;;; 

; rle + crosspollination
;;

(define-module lib-checksum
   (export 
      checksum            ; ll -> nat, default checksum
      adler-32
      fletcher-64
      make-checksummer
   )

   ;(import lib-lazy)

   ;;; Adler32 checksum - simple and fast to calculate

   (define adler-mod 65521) ; largest prime in 16 bits

   ;; fixme: fixnum add-mod with substract would be much better
   (define (adler-32 ll)
      (let loop ((ll ll) (a 1) (b 0))
         (cond
            ((pair? ll)
               (let ((a (rem (+ a (car ll)) adler-mod)))
                  (loop (cdr ll)
                     a (rem (+ a b) adler-mod))))
            ((null? ll) (+ (<< b 16) a))
            (else (loop (ll) a b)))))

   ;;; AdlerX checksum - get an Adler of any size 

   ;; todo: add pseudoprime? to lib-math, ad optionally use largest-primeish here
   (define (largest-prime-below n)
      (cond
         ((prime? n) n)
         ((< n 2) 2) ; not exactly below..
         (else (largest-prime-below (- n 1)))))

   ;; bignum remainder is expensive, and rarely actually needed when numbers (usually 0-256) are below modulus
   (define (add-mod a b m)
      (let ((a (+ a b)))
         (if (teq? a fix+)
            (rem a m)
            (let loop ((a (+ a b)))
               (if (< a m) a (loop (- a m)))))))

   ;; fixme: no bit amount guard in make-adler

   ;; recall that finding largest prime below n is not exactly O(1)

   (define (make-adler n)
      (let ((modulus (largest-prime-below (- (<< 1 (>> n 1)) 1))))
         (define (walk lst a b)
            (cond
               ((null? lst)
                  (lets 
                     ((abits (>> n 1))
                      (bbits (- n abits)))
                     (bor (band a (- (<< 1 abits) 1))
                        (<< (band b (- (<< 1 bbits) 1)) abits))))

               ((pair? lst)
                  (let ((a (add-mod a (car lst) modulus)))
                     (walk (cdr lst) a
                        (add-mod b a modulus))))
               (else (walk (lst) a b))))
         (λ (lst) (walk lst 1 0))))

   ;;; a quick check

   (let 
      ((data (string->list "Wikipedia")) ;; guess where the example checksum is from
       (expected 300286872))
      (if (not (= expected (adler-32 data)))
         (error "adler-32 is broken: " 'bad))
      (if (not (= expected ((make-adler 32) data)))
         (error "make-adler is broken: " 'bad)))

   ;;; Fletcher64 (composite modulus, faster to compute but less accurate results)

   (define fletcher-bits #xffffffff) ; checksum will have twice this many bits

   (define (fletcher-64 ll)
      (let loop ((ll ll) (a 716742388357) (b 4946315))
         (cond
            ((pair? ll)
               (lets
                  ((a (band (+ a (car ll)) fletcher-bits))
                   (b (band (+ a b) fletcher-bits)))
                  (loop (cdr ll) a b)))
            ((null? ll) (* a b)) ; shift would actually be better (larger target space)
            (else (loop (ll) a b)))))
                  


   ;;; Fletcher 

   ; a simpler version of Adler, which could be faster in owl

   ; a = rolling sum of  n (bitwise and 2^n-1)
   ; b = rolling sum of as (ditto)a
   ; finish = a (<< n) | b

   (define (make-fletcher total-bits) ; work in 2^(16*nd)

      (define bits (ceil (/ total-bits 2)))

      (define mask (- (expt 2 bits) 1))

      (define (walk lst a b s)
         (cond
            ((null? lst)
               (bor (band b mask)
                  (<< (band a mask) bits)))
            ((eq? s bits)
               (lets 
                  ((a (band mask (+ a (car lst))))
                   (b (band mask (+ a b))))
                  (walk (cdr lst) a b 0)))
            (else
               (lets 
                  ((a (+ a (car lst)))
                   (b (+ a b))
                   (s _ (fx+ s 1)))
                  (walk (cdr lst) a b s)))))

      (λ (lst) (walk lst 0 0 True)))

   ;;;
   ;;; Default operations
   ;;;

   ;; default generic checksum

   (define default-checksummer adler-32)

   (define (checksum x)
      (cond
         ((pair? x)   (default-checksummer x))
         ((null? x)   (default-checksummer x))
         ((string? x) (default-checksummer (str-iter x)))
         ((vector? x) (default-checksummer (vec-iter x)))
         (else
           (error "checksum: what is " x)))) 


   ;; default checksum generator

   ; nbits -> ((byte ...) -> checksum), where 0 <= checksum <= 2^nbits

   (define default-checksummer
      ;make-fletcher ; O(1) construction, pretty much duplicates
      make-adler     ; slow construction for large n, prettu accurate
      )
  
   ;; n → (list|vector|string → checksum)
   (define (make-checksummer n)
      (let ((csum (default-checksummer n)))
         (λ (data)
            (csum
               (cond
                  ((pair? data) data)
                  ((null? data) data)
                  ((string? data) (str-iter data))
                  ((vector? data) (vec-iter data))
                  (else (error "how do i compute a checksum for " data)))))))




   (define (now-ms) (lets ((ss ms (clock))) (+ ms (* ss 1000))))

   (define (test-speed name data checksum)
      (lets 
         ((start (now-ms))
          (chksum (checksum data))
          (chksum (checksum data))
          (chksum (checksum data))
          (chksum (checksum data))
          (chksum (checksum data))
          (end (now-ms)))
         (print* (list name " - speed " (div (div (* 1000 (* 100000 5)) (max 1 (- end start))) 1024) " Kb/s - " chksum))))

   (define (inc data)
      (cond   
         ((null? data) '(0))
         ((eq? (car data) 255)
            (cons 0 (inc (cdr data))))
         (else
            (cons (+ (car data) 1) (cdr data)))))

;   (import lib-iff)
;   (define (test-duplicates txt data checksum)
;      (let loop ((n 0) (data null) (dups 0) (csums False))
;         (cond
;            ((= n 100000)
;               (print* (list txt " - " (floor (* 100 (/ dups n))) "% duplicates (" dups ")")))
;            (else
;               (lets
;                  ((data (inc data))
;                   (csum (checksum data))
;                   (seen (iget csums csum False)))
;                  (if seen
;                     (loop (+ n 1) data (+ dups 1) csums)
;                     (loop (+ n 1) data dups (iput csums csum True))))))))
;               
;
;   (let ((data (random-numbers (now-ms) 256 100000)))
;      ;(test-duplicates "adler32 fix  " data adler-32)
;      ;(test-duplicates "fletcher64   " data fletcher-64)
;      (test-duplicates "fletcher8    " data (make-fletcher 8))
;      (test-duplicates "fletcher16   " data (make-fletcher 16))
;      (test-duplicates "fletcher32   " data (make-fletcher 32))
;      (test-duplicates "fletcher64   " data (make-fletcher 64))
;      (test-duplicates "fletcher128  " data (make-fletcher 128))
;      (test-duplicates "adler8       " data (make-adler 8))
;      (test-duplicates "adler16      " data (make-adler 16))
;      (test-duplicates "adler32      " data (make-adler 32))
;      (test-duplicates "adler64      " data (make-adler 64))
;      (test-duplicates "adler128     " data (make-adler 128))
;      (test-speed "adler32 fixd " data adler-32)
;      (test-speed "adler32      " data (make-adler 32))
;      (test-speed "adler8       " data (make-adler 8))
;      (test-speed "fletcher8    " data (make-fletcher 8))
;      (test-speed "adler16      " data (make-adler 16))
;      (test-speed "fletcher16   " data (make-fletcher 16))
;      (test-speed "adler32      " data (make-adler 32))
;      (test-speed "fletcher32   " data (make-fletcher 32))
;      (test-speed "adler64      " data (make-adler 64))
;      (test-speed "fletcher64   " data (make-fletcher 64))
;      (test-speed "fletcher96   " data (make-fletcher 96))
;      (test-speed "fletcher128  " data (make-fletcher 128))
;      (test-speed "fletcher256  " data (make-fletcher 256))
;      (test-speed "fletcher512  " data (make-fletcher 512))
;      (test-speed "fletcher1024 " data (make-fletcher 1024))
;   )

   ;(show " => " (checksum '()))   ; check
)

;(import lib-iff)
;(import lib-checksum)
;(import lib-random)
;
;(define (inc l)
;   (cond
;      ((null? l) '(0))
;      ((eq? (car l) 1) (cons 0 (inc (cdr l))))
;      (else (cons (+ (car l) 1) (cdr l)))))
;
;(let loop ((ff False) (l '()) (n 0) (errs 0) (new 0))
;   (print* (list errs "/" n " collisions, so " (floor (/ (* errs 100) (max n 1))) "% failure, uniqs " new))
;   (let ((c (checksum l)))
;      (if (iget ff c False)
;         (begin
;            ;(print (list l 'collides 'at c))
;            (loop (iput ff c True) (inc l) (+ n 1) (+ errs 1) new))
;         (begin
;            ;(print (list l 'ok c))
;            ;(print "OK")
;            (loop (iput ff c True) (inc l) (+ n 1) errs (+ new 1))))))
;
;(let loop ((ff False) (n 0) (errs 0) (new 0) (rst 124124124124))
;   (print* (list errs "/" n " collisions, so " (floor (/ (* errs 100) (max n 1))) "% failure, uniqs " new))
;   (lets
;      ((rst len (rand rst 1024))
;       (nums (random-numbers rst 256 len))
;       (c (checksum nums)))
;      (if (iget ff c False)
;         (begin
;            ;(print (list nums 'collides 'at c))
;            (loop (iput ff c True) (+ n 1) (+ errs 1) new rst))
;         (begin
;            ;(print (list nums 'ok c))
;            ;(print "OK")
;            (loop (iput ff c True) (+ n 1) errs (+ new 1) rst)))))
;   
;;; todo: add lib-checksum-probabilistic (n simple m-bit hashes, check bit collision from all)
;
