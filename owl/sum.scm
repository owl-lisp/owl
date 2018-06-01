(define-library (owl sum)

   (import
      (owl defmac)
      (only (owl math) +)
      (only (owl io) print))

   (export
      eval)

   (begin

      (define (val x)
         (λ (vc sc) (vc x)))

      (define (sum a b)
         (λ (vc sc) (sc a b)))

      (define-syntax sum-case
         (syntax-rules (val sum)
            ((sum-case x ((val a) val-exp) ((sum a b) sum-exp))
               (x (λ (a) val-exp) (λ (a b) sum-exp)))
            ((sum-case x ((sum a b) sum-exp) ((val a) val-exp))
               (x (λ (a) val-exp) (λ (a b) sum-exp)))))

      (define (eval exp)
         (sum-case exp
            ((val a) a)
            ((sum a b) (+ (eval a) (eval b)))))

      (print
         (eval
            (lets ((a (sum (val 1) (val 1)))
                   (a (sum a a))
                   (a (sum a a))
                   (a (sum a a))
                   (a (sum a a))
                   (a (sum a a))) ; 64
                  a)))))
