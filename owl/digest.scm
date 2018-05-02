;;; The digest library provides functions for computing cryptographic signatures.  
;;; Currently SHA1 and SHA256 digests and corresponding message authentication codes
;;; are supported. 
;;; 
;;; The hash functions also have `hasher-raw` and `hasher-bytes` -variants, which 
;;; return the state words and raw signature bytes correspondingly.
;;;
;;; ```
;;;   (sha1 data)   → hash-string
;;;   (sha256 data) → hash-string
;;;   (hmac-sha1   key message) → hash-string
;;;   (hmac-sha256 key message) → hash-string
;;; ```

(define-library (owl digest)

   (export
      sha1            ;; str | vec | list | ll → str
      sha1-raw        ;; ditto → integer list
      sha1-bytes      ;; ditto → byte list
      sha256
      sha256-raw
      sha256-bytes
      hmac-sha1       ;; key, data → sha1 
      hmac-sha1-bytes ;; key, data → sha1 bytes
      hmac-sha256
      hmac-sha256-bytes
      )

   (import
      (owl defmac)
      (owl math)
      (owl list)
      (owl list-extra)
      (owl vector)
      (owl io)
      (owl string)
      (owl proof)
      (scheme base)
      (owl lazy))

   (begin

      (define sha1-blocksize 64)
      (define sha256-blocksize 64)
      (define sha512-blocksize 64)

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

      (define word
         (C band #xffffffff))

      (define (rol x n)
         (word
            (bor
               (<< x n)
               (>> x (- 32 n)))))

      (define (ror x n)
         (word
            (bor
               (>> x n)
               (<< x (- 32 n)))))

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

      (define (sha1-step a b c d e f k w)
         (values (word (+ (rol a 5) f e k w)) a (word (rol b 30)) c d))

      (define bnot
         (C bxor #xffffffff))

      (define (sha1-chunk h0 h1 h2 h3 h4 ws)
         (let loop ((i 0) (a h0) (b h1) (c h2) (d h3) (e h4) (ws (reverse ws)))
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

   (define (uint32->bytes n tail)
      (lets
         ((a (band n #xff)) (n (>> n 8))
          (b (band n #xff)) (n (>> n 8))
          (c (band n #xff)) (n (>> n 8))
          (d (band n #xff)))
         (ilist d c b a tail)))

   (define (ws->bytes ws)
      (foldr uint32->bytes null ws))

   ;; silly version for now
   (define (hash-bytes->string bs)
      (list->string
         (foldr
            (λ (b tl)
               (append (cdr (string->list (number->string (+ #x100 b) 16))) tl))
            null bs)))

   (define sha1-format-result
      (B hash-bytes->string ws->bytes))

      ;; i-3 i-8 i-14 i-16
      (define (sha1-extend-initial-words lst)
         (let loop ((lst lst) (n 16))
            (if (= n 80)
               lst
               (loop
                  (cons (rol (xor-poss 0 3 '(5 6 2) lst) 1) lst)
                  (+ n 1)))))


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
                        (sha1-extend-initial-words ws))))
                  (loop ll h0 h1 h2 h3 h4))
               (list h0 h1 h2 h3 h4)))))

   (define (sha1-raw thing)
      (sha1-chunks
         (cond
            ((string? thing) (str-iter-bytes thing))
            ((vector? thing) (vec-iter thing))
            (else thing))))

   (define sha1-bytes
      (B ws->bytes sha1-raw))

   (define sha1
      (B sha1-format-result sha1-raw))

   (define (list-xor a b)
      (cond
         ((null? a) b)
         ((null? b) a)
         (else
            (lets ((a as a)
                   (b bs b))
               (cons (bxor a b)
                  (list-xor as bs))))))

   (define (any->bytes x)
      (cond
         ((string? x)
            (string->bytes x))
         ((vector? x)
            (vector->list x))
         (else
            ;; may be a lazy list or list
            x)))

   (define (make-hmac hasher blocksize)
      (lambda (key msg)
         (lets
            ((key (any->bytes key)) ;; we want to UTF-8 encode it
             (msg (any->bytes msg)) ;; ditto
             (key (if (> (length key) blocksize) (hasher key) key))
             (key
               (append key
                  (map (λ (x) 0)
                     (iota 0 1 (- blocksize (length key))))))
             (o-pad (map (λ (x) #x5c) (iota 0 1 blocksize)))
             (i-pad (map (λ (x) #x36) (iota 0 1 blocksize))))
            (hasher
               (append (list-xor o-pad key)
                  (hasher (append (list-xor i-pad key) msg)))))))

   (define hmac-sha1-bytes
      (make-hmac sha1-bytes sha1-blocksize))

   ;; (hmac-sha1 key message) → "result", compute SHA1-based message authentication code
   (define (hmac-sha1 k m)
      (hash-bytes->string
         (hmac-sha1-bytes k m)))


   ;;;
   ;;; SHA-2
   ;;;

   ;; 0 .. 63
   (define ks
      (list
         #x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
         #xd807aa98 #x12835b01 #x243185be #x550c7dc3 #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
         #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
         #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
         #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13 #x650a7354 #x766a0abb #x81c2c92e #x92722c85
         #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
         #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
         #x748f82ee #x78a5636f #x84c87814 #x8cc70208 #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))

   (define sha256-pad
      sha1-pad)

   (define (sha256-chunk h0 h1 h2 h3 h4 h5 h6 h7 ws)
      (let loop ((a h0) (b h1) (c h2) (d h3) (e h4) (f h5) (g h6) (h h7) (ws (reverse ws)) (ks ks))
         (if (null? ws)
            (values 
               (word (+ h0 a))
               (word (+ h1 b))
               (word (+ h2 c))
               (word (+ h3 d))
               (word (+ h4 e))
               (word (+ h5 f))
               (word (+ h6 g))
               (word (+ h7 h)))
            (lets
               ((S1 (bxor (bxor (ror e 6) (ror e 11)) (ror e 25)))
                (ch (bxor (band e f) (band g (bnot e))))
                (temp1 (word (+ h S1 ch (car ws) (car ks))))
                (S0 (bxor (bxor (ror a 2) (ror a 13)) (ror a 22)))
                (maj (bxor (bxor (band a b) (band a c)) (band b c)))
                (temp2 (+ S0 maj)))
               (loop (word (+ temp1 temp2)) a b c (word (+ d temp1)) e f g (cdr ws) (cdr ks))))))

   (define (pick lst n)
      (if (eq? n 1)
         (lets ((a as lst))
            (values a as))
         (pick (cdr lst) (- n 1))))

   ;; i-3 i-8 i-14 i-16
   (define (sha256-extend-initial-words lst)
      (let loop ((lst lst) (n 16))
         (if (eq? n 64)
            lst
            (lets
               ((p2 l (pick lst 2)) ;; i-2
                (p7 l (pick l 5))   ;; i-7 
                (p15 l (pick l 8))  ;; i-15
                (p16 l (pick l 1))  ;; i-16
                (s0 (bxor (bxor (ror p15 7) (ror p15 18)) (>> p15 3)))
                (s1 (bxor (bxor (ror p2 17) (ror p2 19)) (>> p2 10)))
                (new (word (+ (+ p16 s0) (+ p7 s1)))))
               (loop (cons new lst) (+ n 1))))))


   (define (sha256-chunks ll)
      (let loop
         ((ll (sha256-pad ll))
          (h0 #x6a09e667)
          (h1 #xbb67ae85)
          (h2 #x3c6ef372)
          (h3 #xa54ff53a)
          (h4 #x510e527f)
          (h5 #x9b05688c)
          (h6 #x1f83d9ab)
          (h7 #x5be0cd19))
         (lets ((ws ll (grab-initial-words ll)))
            (if ws
               (lets 
                  ((h0 h1 h2 h3 h4 h5 h6 h7
                     (sha256-chunk h0 h1 h2 h3 h4 h5 h6 h7 
                        (sha256-extend-initial-words ws))))
                  (loop ll h0 h1 h2 h3 h4 h5 h6 h7))
               (list h0 h1 h2 h3 h4 h5 h6 h7)))))


   (define (sha256-raw thing)
      (sha256-chunks
         (cond
            ((string? thing) (str-iter-bytes thing))
            ((vector? thing) (vec-iter thing))
            (else thing))))

   (define sha256-bytes
      (B ws->bytes sha256-raw))

   (define sha256
      (B sha1-format-result sha256-raw))

   (define hmac-sha256-bytes
      (make-hmac sha256-bytes sha256-blocksize))

   (define (hmac-sha256 k m)
      (hash-bytes->string
         (hmac-sha256-bytes k m)))

   (define sha2 sha256)


   (example
      (sha1 "")     = "da39a3ee5e6b4b0d3255bfef95601890afd80709"
      (sha1 "owl")  = "2c730e3a6d2aad6d914872e45f868d20a543570a"
      (sha1 "λä.ä") = "726f4307d91f7433472917b27c486c9004b94179"
      (sha256 "")     = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
      (sha256 "owl")  = "10f7127b980b0b06ef157749f8a02cca1c606d22642729dcb2d24dbbf2bf30df"
      (sha256 "λä.ä") = "e5b21ee5e8532c8f768aa9090ba71ae799016a0904b13ee855cbd58853464234")

))
