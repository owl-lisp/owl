
;; tests for (beginnings of) R7RS-style library support

(define-library (foo test) 
   (export bar) 
   (begin 
      (define mine "ALPHA")
      (define bar "BAR"))
   (import)
)

(define-library (foo bar)
   (import (foo test))
   (export baz)
   (begin
      (define mine "BRAVO")
      (define (baz x) (cons mine bar))
      (print (baz 42))))

;; test export + renaming

(define-library (rename)
   (export 
      (rename foo bar)
      (rename bar foo))
   (begin
      (define foo "bar")
      (define bar "foo")))

(define-library (test)
   (import (rename))
   (export out)
   (begin
      (define out 42)
      (print (list foo bar))))

;; test only import

(define-library (foobar)
   (export foo bar)
   (begin
      (define foo "foo")
      (define bar "bad")))

(define-library (barfoo)
   (export foo bar)
   (begin
      (define foo "BAD")
      (define bar "bar")))

(define-library (test)
   (export foobar)
   (import 
      (only (foobar) foo)
      (only (barfoo) bar))
   (begin
      (define (foobar)
         (print (cons foo bar)))
      (foobar)))

(define-library (test)
   (export foobar)
   (import
      (except (foobar) bar)
      (except (barfoo) foo))
   (begin
      (define (foobar)
         (print (cons foo bar)))
      (foobar)))


(print "END OF LINE")
