
(define-library (owl digest)
   
   (export
      sha1)

   (import
      (owl defmac)
      (owl math)
      (owl list)
      (owl io)
      (owl string)
      (scheme base)
      (owl lazy))

   (begin

      (define (n->bytes n)
         (let ((a (band n 255))
               (b (band (>> n 8) 255))
               (c (band (>> n 16) 255))
               (d (band (>> n 24) 255)))
            (values a b c d)))
                 
      (define (sha1-finish-pad bits)
         (cons #x80
            (let loop ((pos (band (+ bits 8) 511)))
               (if (= pos 448)
                  (lets ((bits (+ bits 0)) ;; +1 for #x80
                         (a b c d (n->bytes bits)) 
                         (e f g h (n->bytes (>> bits 32))))
                     (list h g f e d c b a))
                  (cons 0 (loop (band (+ pos 8)  511)))))))

      (define (word x) 
         (band x #xffffffff))

      (define (rol x n)
         (word
            (bor
               (<< x n)
               (>> x (- 32 n)))))

      (define (sha1-pad ll)
         (let loop ((ll ll) (bits 0))
            (cond
               ((pair? ll)
                  (cons (car ll)
                     (loop (cdr ll) (+ bits 8))))
               ((null? ll)
                  (sha1-finish-pad bits))
               (else
                  (λ () (loop (ll) bits))))))

      (define (grab-word ll)
         (lets
            ((a ll (uncons ll #false))
             (b ll (uncons ll #false))
             (c ll (uncons ll #false))
             (d ll (uncons ll #false)))
            (values (bor (bor d (<< c 8))
                         (bor (<< b 16) (<< a 24)))
                    ll)))
               
      (define (grab-initial-words ll)
         (lets ((a ll (uncons ll #false)))
            (if a ;;something in stream
               (let loop ((ll (cons a ll)) (ws null) (n 0))
                  (cond
                     ((= n 16)
                        (values ws ll))
                     (else
                        (lets ((x ll (grab-word ll)))
                           (loop ll (cons x ws) (+ n 1))))))
               (values #false null))))

      (define (xor-poss x n ps lst)
         (if (eq? n 1) 
            (if (null? ps)
               (bxor x (car lst))
               (xor-poss (bxor x (car lst)) (car ps) (cdr ps) (cdr lst)))
            (xor-poss x (- n 1) ps (cdr lst))))

      ;; i-3 i-8 i-14 i-16
      (define (extend-initial-words lst)
         (let loop ((lst lst) (n 16))
            (if (= n 80)
               lst
               (loop
                  (cons (rol (xor-poss 0 3 '(5 6 2) lst) 1) lst)
                  (+ n 1)))))

      (define (sha1-step a b c d e f k w)
         (values (word (+ (rol a 5) f e k w)) a (word (rol b 30)) c d))

      (define (bnot w)
         (bxor w #xffffffff))

      (define (sha1-chunk h0 h1 h2 h3 h4 ws)
         (let loop ((i 0) (a h0) (b h1) (c h2) (d h3) (e h4) (ws (reverse ws)))
            ;(print (list 'sha1-loop i a b c d e ws))
            (cond
               ((< i 20)
                  (lets ((f (bor (band b c) (band (bnot b) d)))
                         (k #x5a827999)
                         (a b c d e 
                           (sha1-step a b c d e f k (car ws))))
                     (loop (+ i 1) a b c d e (cdr ws))))
               ((< i 40)
                  (lets ((f (bxor b (bxor c d)))
                         (k #x6ed9eba1)
                         (a b c d e 
                           (sha1-step a b c d e f k (car ws))))
                     (loop (+ i 1) a b c d e (cdr ws))))
               ((< i 60)
                  (lets ((f (bor (bor (band b c) (band b d)) (band c d)))
                         (k #x8f1bbcdc)
                         (a b c d e 
                           (sha1-step a b c d e f k (car ws))))
                     (loop (+ i 1) a b c d e (cdr ws))))
               ((< i 80)
                  (lets ((f (bxor b (bxor c d)))
                         (k #xca62c1d6)
                         (a b c d e 
                           (sha1-step a b c d e f k (car ws))))
                     (loop (+ i 1) a b c d e (cdr ws))))
               (else
                  (values 
                     (word (+ h0 a))
                     (word (+ h1 b))
                     (word (+ h2 c))
                     (word (+ h3 d))
                     (word (+ h4 e)))))))

   (define (sha1-format-result ws)
      (list->string
         (foldr append null
            (map (λ (x) (cdr (string->list (number->string (+ #x100000000 x) 16)))) ws))))

   (define (sha1-chunks ll)
      (let loop 
         ((ll (sha1-pad ll)) 
          (h0 #x67452301) 
          (h1 #xefcdab89) 
          (h2 #x98badcfe) 
          (h3 #x10325476) 
          (h4 #xc3d2e1f0))
         (lets ((ws ll (grab-initial-words ll)))
            (if ws
               (lets 
                  ((h0 h1 h2 h3 h4 
                     (sha1-chunk h0 h1 h2 h3 h4 
                        (extend-initial-words ws))))
                  (loop ll h0 h1 h2 h3 h4))
               (sha1-format-result (list h0 h1 h2 h3 h4))))))

   (define (sha1 thing)
      (cond
         ((string? thing)
            (sha1-chunks (str-iter thing)))
         (else
            (sha1-chunks thing))))

))


