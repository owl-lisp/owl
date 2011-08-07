;;;
;;; Basic io
;;;

(define string "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")

(define (test n)
   (let loop ((n 50000))
      (if (eq? n 0)
         (list (+ n 42))
         (begin
            (print string)
            (loop (- n 1))))))

test
