(define-library (owl equal)

   (export
      simple-equal?
      equal?
      eqv?)

   (import 
      (owl defmac)
      (owl equal-prim)
      (owl symbol)
      (owl string)
      (owl list)
      (owl math))

   (begin
      
      (define (equal? a b)
         (cond
            ((eq? a b) #true)
            ((string? a)
               (if (string? b)
                  (string=? a b)
                  #false))
            ((pair? a)
               (if (pair? b)
                  (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))
                  #false))
            ((symbol? a) #false)
            (else 
               (equal-prim? equal? a b))))

      (define ≡ equal?)

      (define eqv? equal?)))
