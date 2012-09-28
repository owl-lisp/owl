
(define-library (owl function)

   (export function? procedure? bytecode?)

   (import
      (owl defmac)
      (only (owl ff) ff?)
      (only (owl syscall) interact))

   (begin

      ;; raw bytecode vector, 1-level (proc) or 2-level (clos) function
      (define (function? x)
         (or
            (eq? (fxband 31 (type x)) type-bytecode) ;; FIXME, drop 31 after changing types
            (eq? #b010 (fxband (type-old x) #b11111010))))

      ;; something executable? being a function or a finite function
      (define (procedure? obj)
         (or (function? obj)
             (ff? obj)
             (eq? obj #false))) ;; fixme, to be replaced with #empty soon

      (define (bytecode? x) 
         (or 
            ; (eq? type-bytecode-2 (type x))  ;; new good
            (eq? #b100000000010 (fxband (type-old x) #b100011111010)))))) ;; old bad


