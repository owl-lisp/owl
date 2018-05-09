(define-library (owl boolean)

   (export
      boolean?)

   (import
      (owl defmac))

   (begin

      (define (boolean? x)
         (or (eq? x #true) (eq? x #false)))

))
