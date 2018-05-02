;;; The codec library contains some simple content encoding transformations.
;;; 
;;; ```
;;;   (hex-encode "slartibartfast") → "736c617274696261727466617374"
;;;   (hex-decode "736c617274696261727466617374") → "slartibartfast"
;;;   (hex-decode "7") → #false
;;;   (hex-encode "λx.x") → "cebb782e78"
;;;   (hex-decode "cebb782e78") → "λx.x"
;;; ```

(define-library (owl codec)

  (import 
     (owl base)
     (owl proof))

  (export 
    hex-encode-list  ;; (byte ...) → str
    hex-encode       ;; str → str
    hex-decode       ;; str → str | #false
    hex-decode-list) ;; str → (byte ...) | #false

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

   (define hex-encode-list
      (B list->string hex-encode-bytes))

   (define (hex-encode str)
      (cond
         ((string? str)
            (hex-encode-list 
               (string->bytes str)))
         ((pair? str)
            (hex-encode-list str))
         ((null? str)
            "")
         (else
            (error "hex-encode: " str))))

   (define hex-decode-list
      (B hex-decode-bytes string->bytes))

   (define (hex-decode str)
      (maybe bytes->string
         (hex-decode-list str)))

   (example
      (hex-decode (hex-encode "")) = ""
      (hex-decode (hex-encode "foo")) = "foo"
      (hex-decode (hex-encode "λä.ä")) = "λä.ä")))


