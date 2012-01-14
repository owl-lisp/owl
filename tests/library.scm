
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

;; nested and prefixing 

(define-library (aa)
   (export foo bar)
   (begin
      (define foo "aa-foo")
      (define bar "aa-bar")))

(define-library (bb)
   (export foo bar)
   (begin
      (define foo "bb-foo")
      (define bar "bb-bar")))

(define-library (test)
   (export test)
   (import
      (prefix (except (aa) bar) aa-)
      (prefix (only (bb) bar) bb-))
   (begin
      (define (test)
         (print (list aa-foo bb-bar)))
      (test)))

;; cond-expand

#|(define-library (cond)
   (export test)
   (cond-expand 
      (slartibartfast
         (import (only (norway) coasts))
         (begin   
            (define (test x) coasts)))
      (else
         (begin
            (define (test x) x))))
   (begin
      (print (test "ok"))))
 |#        

(define-library (cond)
   (export test)
   (cond-expand 
      (pyramid-scheme
         (import (only (norway) coasts))
         (begin
            (unbound-thingy)
            (define (test x) coasts)))
      (owl-lisp
         (begin
            (define (test x) "ok")))
      (else
         (begin
            (define (test x) "fail"))))
   (begin
      (print (test "ok"))))

(print "END OF LINE")
