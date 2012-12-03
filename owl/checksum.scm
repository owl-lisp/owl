;;; 
;;; A simple checksummer
;;; 

; rle + crosspollination
;;

(define-library (owl checksum)
   (export 
      checksum            ; ll -> nat, default checksum
      adler-32
      fletcher-64
      make-checksummer)

   (import
      (owl defmac)
      (owl math)
      (owl math-extra)
      (owl list)
      (owl string)
      (owl vector)
      (only (owl syscall) error)
      (owl lazy))

   (begin

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
            (if (eq? (type a) type-fix+)
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

         (λ (lst) (walk lst 0 0 #true)))

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
                     (else (error "how do i compute a checksum for " data)))))))))


