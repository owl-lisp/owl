
(define n (band (time-ms) 16383))

(print (= (fib (+ n 2)) (+ (fib n) (fib (+ n 1)))))
