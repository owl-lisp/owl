;; compute some factorials

(define (fakt x) 
   (fold * 1 (iota 1 1 (+ x 1))))

(define (fakti x)
   (let loop ((n 1) (x x))
      (if (= x 1)
         n
         (loop (* n x) (- x 1)))))

(define (faktr x)
   (if (= x 1)
      1
      (* x (faktr (- x 1)))))

(define faktl
   (λ (x)
      ((λ (self) (self self x))
       (λ (self x)
         (if (= x 1)
            x
            (* (self self (- x 1)) x))))))

(for-each print
   (map 
      (λ (op) (op 100))
      (list ! fakt fakti faktr faktl)))

