(define-library (owl boolean)

   (export boolean?)

   (import
      (owl defmac))

   (begin
      (define (boolean? x)
         (cond
            ((eq? x #true) #true)
            ((eq? x #false) #true)
            (else #false)))))

