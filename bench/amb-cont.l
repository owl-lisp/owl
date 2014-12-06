;;;
;;; Functional backtracking with a continuation-based amb
;;;

; tags: cont values

;; todo: make the einstein puzzle or something more interesting with this

(define (nums n) 
   (if (= n 0)
      '()
      (cons n (nums (- n 1)))))

(define data (nums 100))

;; amb value = (fail-cont . opts)
(define (amb back opts)
   (if (null? opts)
      (back)
      (call/cc
         (lambda (ret)
            (let 
               ((retry 
                  (lambda () 
                     (receive (amb back (cdr opts))
                        (lambda (retry val)
                           (ret retry val))))))
               (values retry (car opts)))))))

(define (require b pred)
   (if (not pred) (b) 42))

(define (last b)
   ;; no lets in r5rs.defs, so use the silly receive
   (receive (amb b data)
      (lambda (b x)
         (receive (amb b data)
            (lambda (b y)
               (require b (eq? x y))
               (require b (eq? x 1))
               (list x y))))))

(define (test-one args)
   (list 
      (fold + 2 
         (map (lambda (x) (* x 20)) 
            (call/cc
               (lambda (ret)
                  (last (lambda () (ret 'fail)))))))))

; 10x test-one
(define (test args)
   (let loop ((n 0) (last 0))
      (if (= n 10)
         last
         (loop (+ n 1) (test-one args)))))

