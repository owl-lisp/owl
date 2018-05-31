(define-syntax test
  (syntax-rules ()
    ((testi a b) (lambda (a) (print a b)))))

;; ((lambda (a) (print a 22)) 11) → 1122
((test a 22) 11)

;;            .--> shadows the macro
;; ((lambda (test) (print test 22)) 11) → 1122
((test test 22) 11)

;; ((lambda (test) (print test test)) 11) → 1111
((test test test) 11)
;;           '--> multiple references to the same gensym

(define foo 22)
;; ((lambda (foo) (print foo 22)) 11), foo is fresh
((test foo 22) 11) ;; -> 1122

((test a foo) 11) ;; -> 1122

(define foo "no")
(let ((foo 22))
   ((test a foo) 11)) ;; 1122

(define-library (macro test)
   (import (owl defmac))
   (export foo bar)
   (begin
      (define bar 42)
      (define hidden 100)
      (define-syntax foo
         (syntax-rules (x)
            ((_) bar)
            ((_ x) hidden)
            ((_ a b) (list a b hidden bar (quote x)))))))

(import
   (prefix (macro test) my-))

(print my-bar) ;; 42
(print (my-foo)) ;; 42, should work even after rename
(print (my-foo x)) ;; 100, should work via macro even though it's not exported
(print (my-foo 1 2)) ;; (1 2 100 42 x)
