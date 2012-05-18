;; funny call/cc tests

;; defining y with self application via call/cc
(define Y 
   (λ (e) ((call/cc call/cc) (λ (f) (e (λ (x) (((call/cc (call/cc call/cc)) f) x)))))))

(define fakt
   (Y (λ (self) (λ (x) (if (= x 0) 1 (* x (self (- x 1))))))))

(print (fakt 10))


