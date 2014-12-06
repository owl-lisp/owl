(define num (+ (band (time-ms) #b1111111111) 2))

(define por-opts 
   (map 
      (λ (try) (λ () (ediv num try)))
      (iota 2 1 (+ 1 (isqrt num)))))

(define (xor a b)
   (if a b (if b #false (not a))))

(print
	(if (equal? (prime? num) (not (por* por-opts)))
		"OK"
	   num))
