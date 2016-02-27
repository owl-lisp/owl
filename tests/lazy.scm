(import (owl lazy))

(define (editor x)
   (cond
      ((even? x) #false) ;; keep evens
      ((< x 10) null)    ;; drop small ones
      (else (list x x))))  ;; double big odds

(print (force-ll (ltake (ledit editor (lnums 1)) 20)))
