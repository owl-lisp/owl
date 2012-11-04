
(define-library (owl function)

   (export function? procedure? bytecode?)

   (import
      (owl defmac)
      (only (owl ff) ff?)
      (only (owl syscall) interact))

   (begin

      (define (bytecode? x) 
         (eq? type-bytecode (type x)))

      ;; raw bytecode vector, 1-level (proc) or 2-level (clos) function
      (define (function? x)
         (or
            (bytecode? x)
            (eq? (type x) type-proc)
            (eq? (type x) type-clos)
            ;(eq? #b010 (fxband (type-old x) #b11111010))
            ))

      ;; something executable? being a function or a finite function
      (define (procedure? obj)
         (or (function? obj)
             (ff? obj)))))


