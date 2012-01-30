(define (print arg)
   (display arg)
   (display "
"))

#|

Testing block comments. They could also be nested on second thought...

|#

;; test |symbol| syntax

(let
   ((a '|foo bar|)
    (b (string->symbol "foo bar")))
   (if (not (eq? a b))  
      (print "symbolic failure 1")))

(if (not (= 42 ((lambda (|foo|) foo) 42)))
   (print "symbolic failure 2"))

;; returning both "" and "||" make sense. using the latter for now, but 
;; might be that that behavior should only be done for write.
(if (not (string=? "||" (symbol->string (string->symbol ""))))
   (print "symbolic failure 3"))

;; test _ wildcard in macros
(define-syntax foo
   (syntax-rules (x)
      ((_ x x) 2)
      ((_ _ x) 1)
      ((_ x _) 1)
      ((_ . _) 0)))

(if (not (equal? (list 2 1 1 0) (list (foo x x) (foo o x) (foo x o) (foo o o))))
   (print (list (foo x x) (foo o x) (foo x o) (foo o o))))

;; case also supports =>

(case (+ 1 2)
   ((1 2) => (lambda (x) (print (list 'bad x))))
   ((3) => (lambda (x) 'ok)) ;; uses eqv
   (else => (lambda (x) (print (list 'bad x)))))

(case (+ 1 2)
   ((1) => (lambda (x) (print (list 'bad x))))
   ((2 3) => (lambda (x) 'ok)) ;; uses memv
   (else => (lambda (x) (print (list 'bad x)))))

(case (+ 1 3)
   ((1) => (lambda (x) (print (list 'bad x))))
   (else => (lambda (x) 42)))

;; let*-values
(let*-values
   (((a b) (values 11 22))
    ((b a) (values a b))
    ((x)   (values (- a b))))
   (if (not (= x 11))
      (print (list 'let*-values 'got x))))

;; letrec*
(letrec*
   ((foo (λ (x) 0))
    (bar (λ (x) (if (= x 1) x (bar (- x 1)))))
    (foo (λ (x) (if (= x 1) x (foo (- x (bar x)))))))
   (if (not (= (foo 10) 1))
      (print (list 'letrec* 'got (foo 10)))))

;; shouldn't have printed anything before this
(print "all done")
