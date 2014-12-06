;;;
;;; Functional backtracking with a closure-based amb
;;;

; tags: values

;; todo: make the einstein puzzle or something more interesting with this

(define (nums n) 
   (if (= n 0)
      '()
      (cons n (nums (- n 1)))))

(define data (nums 2000))

;; amb value = (fail-cont . opts)
(define (amb opts cont)
   (if (null? opts)
      #f
      (let ((outcome (cont (car opts))))
         (if outcome
            outcome
            (amb (cdr opts) cont)))))

(define (last ok)
   (amb data
      (lambda (x)
         (amb data
            (lambda (y)
               (cond
                  ((not (eq? x y)) #f)
                  ((not (eq? x 1)) #f)
                  (else (ok (list x y)))))))))

(define (test args)
   (list
      (fold 
         (lambda (a b) (if (eq? b 1) (+ a 1) 0))
         40
         (last (lambda (ok) ok)))))

