;;; check that the default sort is stable

(define rst (seed->rands (time-ms)))

;; (i ...) → ((i . nth-i) ...)
(define (index lst)
   (let loop ((lst lst) (ff empty))
      (if (null? lst)
         null
         (lets 
            ((x lst lst)
             (n (get ff x 0)))
            (cons (cons x n)
               (loop lst (put ff x (+ n 1))))))))

(define (car< a b) (< (car a) (car b)))

(define (carcdr< a b) 
   (cond
      ((< (car a) (car b)) #true)
      ((= (car a) (car b)) (< (cdr a) (cdr b)))
      (else #false)))

(lets
   ((rst (seed->rands (time-ms)))
    (rst base (rand-range rst 2 100))
    (rst nums (random-numbers rst base 1000))
    (pairs (index nums))
    (pairs (sort car< pairs))
    (pairsp (sort carcdr< pairs)))
   (if (equal? pairs pairsp)
      (print "ceci est une stable")
      (print "ceci n'est pas une stable: " (list pairs pairsp))))

