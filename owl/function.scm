
(define-library (owl function)

   (export function? procedure? bytecode?)

   (import
      (owl defmac)
      (only (owl ff) ff?)
      (only (owl syscall) interact))

   (begin
      ;; raw bytecode vector, 1-level (proc) or 2-level (clos) function
      (define (function? x) (eq? #b110 (fxband (type x)  #b11111110)))

      ;; something executable? being a function or a finite function
      (define (procedure? obj)
         (or (function? obj)
             (ff? obj)
             (eq? obj False)))

      ;                                                               .-> ignore padding byte count
      ;                            .-> raw data object              .-+
      (define (bytecode? x) (eq? #b100000000110 (fxband (type x) #b100011111110))) ))
      ;                             '------+
      ;                                    '-> 8-bit type/padding info


