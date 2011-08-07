;; This is the Gabriel benchmark takl with minor changes

(define (listn n)
  (if (= n 0)
    '()
    (cons n (listn (- n 1)))))
 
(define l18 (listn 18))
(define l12 (listn 12))
(define  l6 (listn 6))
 
(define (shorterp x y)
  (and (not (null? y))
       (or (null? x)
           (shorterp (cdr x)
                     (cdr y)))))
 
(define (mas x y z)
  (if (not (shorterp y x))
      z
      (mas (mas (cdr x) y z)
           (mas (cdr y) z x)
           (mas (cdr z) x y))))

(define expected '(7 6 5 4 3 2 1))

(define (test args)
   (let loop ((n 5) (last null))
      (if (eq? n 0)
         (if (equal? last expected)
            (print (cons 42 null))
            (print last))
         (loop (- n 1) (mas l18 l12 l6)))))

test

