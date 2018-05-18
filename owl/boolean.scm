(define-library (owl boolean)

   (export
      boolean?
      boolean=?)

   (import
      (owl defmac)
      (only (owl list) every))

   (begin

      (define (boolean? x)
         (or (eq? x #true) (eq? x #false)))

      (define (boolean=? x . lst)
         (and (boolean? x) (every (C eq? x) lst)))
))
