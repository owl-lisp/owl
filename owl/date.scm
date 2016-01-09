
(define-library (owl date)
   (import
      (owl base))

   (export
      date)

   (begin

      ;; 0 = epoch @ Thu Jan 1 1970

      (define minute 60)

      (define hour (* 60 minute))

      (define day (* 24 hour))
      
      (define week (* day 7))

      (define (day-name n)
         (vector-ref #(Thursday Friday Saturday Sunday Monday Tuesday Wednesday) n))

      (define (day-of x)
         (let loop ((x (modulo x week)) (daynum 0))
            (if (< x day)
               (day-name daynum)
               (loop (- x day) (+ daynum 1)))))

      (define (date-of x)
         (print x " = " (day-of x)))

      (define date
         (case-lambda
            (()  (date-of (time)))
            ((x) (date-of x))))
))


; (import (owl date)) (date 0) (date)


