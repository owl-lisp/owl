(define-library (owl crypt)

   (export 
      crypt
      encrypt
      decrypt)

   (import
      (owl base)
      (owl random)
      (owl digest))

   (begin

      (define (grow n)
         (if (< n 100000000000000)
            (grow (* (+ n 1) n))
            n))

      (define (string->seed str)
         ;; take 7 bits per char, maybe use also others
         (str-fold
            (lambda (n c) (bxor (<< n 7) c))
            0 str))

      (define (rand-stream str)
         (let loop ((rs (seed->rands (string->seed str))) (bits 0) (out 0))
            (if (eq? bits 8)
               (pair out (loop rs 0 0))
               (lets ((d rs (uncons rs #f)))
                  (loop rs (+ bits 1)
                     (+ (<< out 1) (band d 1)))))))
    
      (define (hex->val c)
         (if (lesser? c #\:) (- c #\0) (- c #\W)))
         
      (define (sha1-stream str)
         (let loop ((str (sha1 str)))
            (lets ((l (string->list str)))
               (pair 
                  (bor (hex->val (car l))
                       (<< (hex->val (cadr l)) 4))
                  (loop (sha1 str))))))

      (define (xormux a b)
         (cond
            ((pair? a)
               (cond
                  ((pair? b)
                     (pair 
                        (bxor (car a) (car b))
                        (xormux (cdr a) (cdr b))))
                  ((null? b) null)
                  (else (xormux a (b)))))
            ((null? a) null)
            (else (xormux (a) b))))

      (define (key-stream str)
         (xormux
            (sha1-stream str)
            (rand-stream str)))

      (define magic 
         (list 67 114 121 49))

      ;; note: ll gets computed twice
      (define (crypt ll key)
         ;; using just 4*8 bits for now
         (let ((csum (map (lambda (x) (band x 255)) (sha1-raw ll))))
            (list->vector
               (append magic
                  (force-ll
                     (xormux 
                        (append csum ll)
                        (key-stream key)))))))

      (define (encrypt data key)
         (crypt
            (cond
               ((string? data) (str-iter-bytes data))
               ((vector? data) (vec-iter data))
               (else data))
            key))
         
      (define (decrypt lst key)
         (cond
            ((vector? lst)
               (decrypt (vector->list lst) key))
            (else
               (if (equal? magic (take lst 4))
                  (lets
                     ((cipher  (drop lst 4))
                      (c-plain (force-ll (xormux cipher (key-stream key))))
                      (csum    (take c-plain 5))
                      (plain   (drop c-plain 5))
                      (psum    (map (lambda (x) (band x 255)) (sha1-raw plain))))
                     (if (equal? csum psum)
                        (list->vector plain)
                        #false))
                  #false))))))
