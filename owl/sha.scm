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
            (lets ((bits (+ bits 1)) ;; +1 for #x80
                   (a b c d (n->bytes bits)) 
                   (e f g h (n->bytes (>> bits 32))))
               (list a b c d e f g h))
            (cons 0 (loop (band (+ pos 8)  511)))))))

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
      (values (bor (bor a (<< b 8))
                   (bor (<< c 16) (<< d 24)))
              ll)))
         
(define (grab-initial-words ll)
   (lets ((a ll (uncons ll #false)))
      (if a ;;something in stream
         (let loop ((ll (cons a ll)) (ws null) (n 0))
            (cond
               ((= n 16)
                  ws)
               (else
                  (lets ((x ll (grab-word ll)))
                     (loop ll (cons x ws) (+ n 1))))))
         null)))

(define (xor-poss x n ps lst)
   (if (eq? n 0) 
      (if (null? ps)
         (bxor x (car lst))
         (xor-poss (bxor x (car lst)) (car ps) (cdr ps) (cdr lst)))
      (xor-poss x (- n 1) ps (cdr lst))))

(define h0 #x67452301)
(define h1 #xefcdab90)
(define h2 #x98badcfe)
(define h3 #x10325476)
(define h4 #xc3d2e1f0)

;; i-3 i-8 i-14 i-16
(define (extend-initial-words lst)
   (let loop ((lst lst) (n 16))
      (if (= n 80)
         lst
         (loop
            (cons
               (band #xffffffff
                  (<< (xor-poss 0 3 '(5 6 2) lst)) 1)
               lst)
            (+ n 1)))))

(define (word x) 
   (band x #xffffffff))

(define (sha1-step a b c d e f k w)
   (values
      (word (+ (<< a 5) f e k w))
      a
      (word (<< b 30))
      c
      d))

(define (bnot w)
   (bxor w #xffffffff))

;; eww.. this can be deuglified easily, but first get it running.
(define (sha1-chunk a b c d e ws)
   (let loop ((i 0) (a a) (b b) (c c) (d d) (e e) (ws (reverse ws)))
      (cond
         ((< i 20)
            (lets ((f (bxor (band b c) (band (bnot b) d)))
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
            (lets ((f (bxor (bxor (band b c) (band b d)) (band c d)))
                   (k #x8f1bbcdc)
                   (a b c d e 
                     (sha1-step a b c d e f k (car ws))))
               (loop (+ i 1) a b c d e (cdr ws))))
         ((< i 80)
            (lets ((f (bxor b (bxor c d)))
                   (k #xca82c1d6)
                   (a b c d e 
                     (sha1-step a b c d e f k (car ws))))
               (loop (+ i 1) a b c d e (cdr ws))))
         (else
            (list 'block-sha1 a b c d e)))))

(print 
   (sha1-chunk h0 h1 h2 h3 h4
      (grab-initial-words 
         (sha1-finish-pad 0))))




