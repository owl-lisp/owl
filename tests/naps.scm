
(define max-ms-oversleep 10)

(define (close-enough? want got)
   (and (>= got want)
        (< got (+ want max-ms-oversleep))))

(define (random-napper seed n)
   (let loop ((rs (seed->rands seed)) (n n))
      (set-ticker 0)
      (if (> n 0)
         (lets 
            ((rs x (rand rs 30))
             (start (time-ms))
             (_ (sleep x))
             (elapsed (- (time-ms) start)))
            (if (close-enough? x elapsed)
               (loop rs (- n 1))
               (print "Tried to sleep " x " but slept " elapsed "!")))
         (print "ok"))))

(begin
   (fork (lambda () (random-napper 1 10)))
   (fork (lambda () (random-napper 12 10)))
   (fork (lambda () (random-napper 123 10)))
   (fork (lambda () (random-napper 1234 10)))
   (fork (lambda () (random-napper 12345 10)))
   (fork (lambda () (random-napper 123456 10)))
   )
