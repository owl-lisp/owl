(import (owl lazy))

(define (editor x)
   (cond
      ((even? x) #false) ;; keep evens
      ((< x 10) null)    ;; drop small ones
      (else (list x x))))  ;; double big odds

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
   (o print computed))

;; 

(print-computed three-elems)
