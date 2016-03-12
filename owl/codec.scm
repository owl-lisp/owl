(define-library (owl codec)

  (import (owl base))

  (export 
    hex-encode     ;; str → str
    hex-decode)    ;; str → str | #false

  (begin
  
    (define hex-chars
      (vector #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
              #\a #\b #\c #\d #\e #\f))

    (define (hex-encode-bytes lst)
      (foldr
        (λ (x tl)
          (ilist 
            (vector-ref hex-chars (>> x 4))
            (vector-ref hex-chars (band x 15))
            tl))
        null lst))

    (define (hex-char->bits x)
      (cond
        ((lesser? x #\0) #false)
        ((lesser? x #\:) (- x 48))
        ((lesser? x #\A) #false)
        ((lesser? x #\G) (- x 55))
        ((lesser? x #\a) #false)
        ((lesser? x #\g) (- x 87))
        (else #false)))

    (define (hex-decode-bytes bs)
       (let loop ((out null) (bs bs))
         (if (null? bs)
            (reverse out)
            (lets ((b bs bs))
               (cond
                  ((null? bs)
                     #false)
                  ((hex-char->bits b) =>
                     (lambda (b)
                        (lets 
                           ((a bs bs)
                            (a (hex-char->bits a)))
                           (if a
                              (loop (cons (bor (<< b 4) a) out) bs)
                              #false))))
                  (else #false))))))

   (define (hex-encode str)
      (list->string
         (hex-encode-bytes
            (string->bytes str))))

   (define (hex-decode str)
      (let ((bs (hex-decode-bytes (string->bytes str))))
         (if bs
            (bytes->string bs)
            #false)))))
