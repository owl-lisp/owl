;; Things that should work the same as in R5RS Scheme

(define (print thing)
   ;; silly R5RS & Owl version
   (display thing)
   (display "
"))

;; print something if bad result
(define (check name thing wanted)
   (display name)
   (display " ")
   (if (equal? thing wanted)
      (print "ok")
      (begin 
         (print " *** WRONG *** ")
         (print (list 'got thing))
         (print (list 'not wanted)))))
      

(check "1.3.4" (* 5 8) 40)

;; mismatch - factorial definition

(define x 28)

(check "4.1.1" x 28)

(check "4.1.2" (quote a) 'a)
(check "4.1.2" (quote #(a b c)) (vector 'a 'b 'c))
(check "4.1.2" #(a b c) #(a b c))
(check "4.1.2" ''a '(quote a))
(check "4.1.2" '"abc" "abc")
(check "4.1.2" "abc" "abc")
(check "4.1.2" '145932 145932)
(check "4.1.2" 145932 145932)
(check "4.1.2" '#t #t)
(check "4.1.2" #t #t)

(check "4.1.3" (+ 3 4) 7)
(check "4.1.3" ((if #f + *) 3 4)   12)
(check "4.1.4" ((lambda (x) (+ x x)) 4) 8)

(define reverse-subtract
  (lambda (x y) (- y x)))

(check "4.1.4" (reverse-subtract 7 10) 3)

(define add4
  (let ((x 4))
      (lambda (y) (+ x y))))

(check "4.1.4" (add4 6) 10)

;; mismatch - no variable arity lambdas 

(check "4.1.5" (if (> 3 2) 'yes 'no)  'yes)
(check "4.1.5" (if (> 2 3) 'yes 'no) 'no)
(check "4.1.5" (if (> 3 2) (- 3 2) (+ 3 2)) 1)

;; mismatch - assignment 
(check "4.2.1" (cond ((> 3 2) 'greater) ((< 3 2) 'less)) 'greater)
(check "4.2.1" (cond ((> 3 3) 'greater) ((< 3 3) 'less) (else 'equal)) 'equal)
(check "4.2.1" (cond ((assv 'b '((a 1) (b 2))) => cadr) (else #f)) 2)

(check "4.2.1" (case (* 2 3) ((2 3 5 7) 'prime) ((1 4 6 8 9) 'composite))  'composite)

(check "4.2.1" (case (car '(c d))
  ((a e i o u) 'vowel)
    ((w y) 'semivowel)
      (else 'consonant)) 'consonant)

(check "4.2.1" (and (= 2 2) (> 2 1)) #t)
(check "4.2.1" (and (= 2 2) (< 2 1)) #f)
(check "4.2.1" (and 1 2 'c '(f g)) '(f g))
(check "4.2.1" (and) #t)
(check "4.2.1" (or (= 2 2) (> 2 1)) #t)
(check "4.2.1" (or (= 2 2) (< 2 1)) #t)
(check "4.2.1" (or #f #f #f) #f)
(check "4.2.1" (or (memq 'b '(a b c)) (/ 3 0)) '(b c))

(check "4.2.2" (let ((x 2) (y 3)) (* x y)) 6)
(check "4.2.2" (let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x))) 35)

(check "4.2.2" (let ((x 2) (y 3)) (let* ((x 7) (z (+ x y))) (* z x))) 70)

(check "4.2.2"
   (letrec ((even?
          (lambda (n)
            (if (zero? n)
                #t
                (odd? (- n 1)))))
         (odd?
          (lambda (n)
            (if (zero? n)
                #f
                (even? (- n 1))))))
     (even? 88))
   #t)


(check "4.2.4" 
   (let ((x '(1 3 5 7 9)))
     (do ((x x (cdr x))
          (sum 0 (+ sum (car x))))
         ((null? x) sum)))
   25)

(check "4.2.4" 
   (let loop ((numbers '(3 -2 1 6 -5))
           (nonneg '())
           (neg '()))
  (cond ((null? numbers) (list nonneg neg))
        ((>= (car numbers) 0)
         (loop (cdr numbers)
               (cons (car numbers) nonneg)
               neg))
        ((< (car numbers) 0)
         (loop (cdr numbers)
               nonneg
               (cons (car numbers) neg)))))
   '((6 1 3) (-5 -2)))


(check "4.2.6" `(list ,(+ 1 2) 4) '(list 3 4))
(check "4.2.6" (let ((name 'a)) `(list ,name ',name))   '(list a (quote a)))
(check "4.2.6" `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b) '(a 3 4 5 6 b))
(check "4.2.6" `(( foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))) '((foo 7) . cons))

;; FIXME
;(check "4.2.6" 
;   `#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)  
;   #(10 5 2 4 3 8))

;; FIXME: let-syntax and letrec-syntax are missing

;; FIXME - expands to the wrong one
; (check "4.3.2" (let ((=> #f)) (cond (#t => 'ok))) 'ok)

(check "5.2.2" (let ((x 5))
   (define foo (lambda (y) (bar x y)))
   (define bar (lambda (a b) (+ (* a b) a)))
   (foo (+ x 3)))
   45)

(check "6.1" (eqv? 'a 'a) #t)
(check "6.1" (eqv? 'a 'b) #f)
(check "6.1" (eqv? 2 2) #t)
(check "6.1" (eqv? '() '()) #t)
(check "6.1" (eqv? 100000000 100000000)  #t)
;; (check "6.1" (eqv? (cons 1 2) (cons 1 2)) #f) <- check defn of eqv
(check "6.1" (eqv? (lambda () 1) (lambda () 2)) #f)
(check "6.1" (eqv? #f 'nil)  #f)
(check "6.1" (let ((p (lambda (x) x))) (eqv? p p)) #t)

(check "6.1" 
   (letrec ((f (lambda () (if (eqv? f g) 'f 'both)))
         (g (lambda () (if (eqv? f g) 'g 'both))))
           (eqv? f g))
   #f)

(check "6.1" (eq? 'a 'a) #t)
(check "6.1" (eq? (list 'a) (list 'a)) #f)
(check "6.1" (eq? '() '()) #t)
(check "6.1" (eq? car car)  #t)
(check "6.1" (let ((x '(a))) (eq? x x)) #t)
(check "6.1" (let ((x '#())) (eq? x x)) #t)
(check "6.1" (let ((p (lambda (x) x))) (eq? p p))#t)

(check "6.1" (equal? 'a 'a) #t)
(check "6.1" (equal? '(a) '(a)) #t)
(check "6.1" (equal? '(a (b) c) '(a (b) c)) #t)
(check "6.1" (equal? "abc" "abc") #t)
(check "6.1" (equal? 2 2)  #t)
; (check "6.1" (equal? (make-vector 5 'a) (make-vector 5 'a)) #t) ;; FIXME make-vector not there, but could be
(check "6.1" (equal? (lambda (x) x) (lambda (y) y)) #t)
