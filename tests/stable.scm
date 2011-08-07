;;; check that the default sort is stable

(define rst (seed->rands (time-ms)))

;; (i ...) → ((i . nth-i) ...)
(define (index lst)
   (let loop ((lst lst) (ff False))
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
      ((< (car a) (car b)) True)
      ((= (car a) (car b)) (< (cdr a) (cdr b)))
      (else False)))

(lets
   ((rst (seed->rands (time-ms)))
    (rst base (rnd-range rst 2 100))
    (rst nums (random-numbers rst base 1000))
    (pairs (index nums))
    (pairs (sort car< pairs))
    (pairsp (sort carcdr< pairs)))
   (if (equal? pairs pairsp)
      (print "ceci est une stable")
      (show "ceci n'est pas une stable: " (list pairs pairsp))))

