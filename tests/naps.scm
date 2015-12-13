
(define max-ms-oversleep 10)

(define (close-enough? want got)
   (and (>= got want)
        (< got (+ want max-ms-oversleep))))

(define (random-napper seed n)
   (let loop ((rs (seed->rands seed)) (n n) (failed? #false))
      (set-ticker 0)
      (if (> n 0)
         (lets 
            ((rs x (rand rs 30))
             (start (time-ms))
             (_ (sleep x))
             (elapsed (- (time-ms) start)))
            (cond
               ((close-enough? x elapsed)
                  (loop rs (- n 1) #false))
               (failed?
                  (print "Tried to sleep " x " but slept " elapsed "!"))
               ((< elapsed x)
                  (print "Tried to sleep " x " but slept " elapsed "!"))
               (else
                  ;; possible to oversleep on occasion, if a big gc happens
                  ;; in the meantime
                  (loop rs (- n 1) #true))))
         (print "ok"))))

(begin
   (fork (lambda () (random-napper 1 10)))
   (fork (lambda () (random-napper 12 10)))
   (fork (lambda () (random-napper 123 10)))
   (fork (lambda () (random-napper 1234 10)))
   (fork (lambda () (random-napper 12345 10)))
   (fork (lambda () (random-napper 123456 10)))
   )
