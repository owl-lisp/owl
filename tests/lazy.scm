(import (owl lazy))

(define (editor x ll)
   (cond
      ((even? x) (cons x ll)) ;; keep evens
      ((< x 10) ll)    ;; drop small ones
      (else (ilist x x ll))))  ;; double big odds

(print (force-ll (ltake (ledit editor (lnums 1)) 20)))


;; start checking laziness/eagerness preservation

(define (boom . tail)
   (lambda ()
      (print "BOOM! tail got computed")
      tail))

(define three-elems (ilist 0 1 2 (boom 3 4 5)))

(define (computed x)
   (cond
      ((pair? x) (cons (car x) (computed (cdr x))))
      ((null? x) x)
      ((function? x) 'TBC)
      (else 'ERROR)))

(define print-computed
   (B print computed))

(print-computed three-elems)
