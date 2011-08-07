;; This is ctak.scm taken from Larceny, with trivial changes to make it work in this benchmark

(define (ctak-aux k x y z)
  (if (not (< y x))
      (k z)
      (call/cc
       (lambda (k)
         (ctak-aux
          k
          (call/cc
           (lambda (k) (ctak-aux k (- x 1) y z)))
          (call/cc
           (lambda (k) (ctak-aux k (- y 1) z x)))
          (call/cc
           (lambda (k) (ctak-aux k (- z 1) x y))))))))

(define (ctak x y z)
  (call/cc
   (lambda (k) (ctak-aux k x y z))))

(define (test args)
   (let loop ((n 10) (last null))
      (if (eq? n 0)
         (if (equal? last 7)
            (print (cons 42 null))
            (print last))
         (loop (- n 1) (ctak 18 12 6)))))

test
