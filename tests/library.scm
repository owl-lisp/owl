
;; tests for (beginnings of) R7RS-style library support

(define-library (foo test) 
   (export bar) 
   (begin 
      (define mine "ALPHA")
      (define bar "BAR"))
   (import)
)

(define-library (foo bar)
   (import (foo test))
   (export baz)
   (begin
      (define mine "BRAVO")
      (define (baz x) (cons mine bar))
      (print (baz 42))))

