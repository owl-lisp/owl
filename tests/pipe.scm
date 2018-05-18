(import (owl sys))

(define p (pipe)) ;; (read . write)

(define (listen)
   (let ((res (try-get-block (car p) 1000 #false)))
      (print
         (if (eq? res #true)
            'nothing
            res))))

(listen)
(write-bytes (cdr p) (list 1 2 3 4))
(listen)
(listen)
(write-bytes (cdr p) (list 11 22 33 44))
(listen)
(listen)
(close-port (cdr p))
(listen)
(close-port (car p))
(listen)
