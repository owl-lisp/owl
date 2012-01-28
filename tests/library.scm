
;; tests for (beginnings of) R7RS-style library support

(define-library (foo test) 
   (export bar) 
   (import (owl base))
   (begin 
      (define mine "ALPHA")
      (define bar "BAR"))
   (import)
)

(define-library (foo bar)
   (import 
      (owl base)
      (foo test))
   (export baz)
   (begin
      (define mine "BRAVO")
      (define (baz x) (cons mine bar))
      (print (baz 42))))

;; test export + renaming

(define-library (rename)
   (import (owl base))
   (export 
      (rename foo bar)
      (rename bar foo))
   (begin
      (define foo "bar")
      (define bar "foo")))

(define-library (test)
   (import (owl base) (rename))
   (export out)
   (begin
      (define out 42)
      (print (list foo bar))))

;; test only import

(define-library (foobar)
   (import (owl base))
   (export foo bar)
   (begin
      (define foo "foo")
      (define bar "bad")))

(define-library (barfoo)
   (export foo bar)
   (import (owl base))
   (begin
      (define foo "BAD")
      (define bar "bar")))

(define-library (test)
   (export foobar)
   (import 
      (owl base)
      (only (foobar) foo)
      (only (barfoo) bar))
   (begin
      (define (foobar)
         (print (cons foo bar)))
      (foobar)))

(define-library (test)
   (export foobar)
   (import
      (owl base)
      (except (foobar) bar)
      (except (barfoo) foo))
   (begin
      (define (foobar)
         (print (cons foo bar)))
      (foobar)))

;; nested and prefixing 

(define-library (aa)
   (import (owl base))
   (export foo bar)
   (begin
      (define foo "aa-foo")
      (define bar "aa-bar")))

(define-library (bb)
   (import (owl base))
   (export foo bar)
   (begin
      (define foo "bb-foo")
      (define bar "bb-bar")))

(define-library (test)
   (export test)
   (import
      (owl base)
      (prefix (except (aa) bar) aa-)
      (prefix (only (bb) bar) bb-))
   (begin
      (define (test)
         (print (list aa-foo bb-bar)))
      (test)))

;; cond-expand

(define-library (cond)
   (export test)
   (import (owl base))
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
            (define (test x) "feature fail"))))
   (begin
      (print (test "ok"))))

(define-library (cond logic)
   (export test)
   (import (owl base))
   (cond-expand
      ((and owl-lisp (not owl-lisp))
         (begin (define foo "wrong")))
      ((or quilty (not quilty))
         (begin (define foo "correct")))
      (else
         (begin (define foo "wronger"))))
   (begin
      (define (test) (print foo))
      (test)))

;; file include

(define *include-dirs*  ;; try to include from the tests directory only
   (list "tests"))

(define-library (include test)
   (export test)
   (import (owl base))
   (include "included.txt") ;; load tests/included.txt or fail
   (begin (test)))          ;; call it to get output

(print "END OF LINE")

;; toplevel import

(define a "o")
(define b "o")

(define-library (foo bar)
   (export a b)
   (import (owl base))
   (begin
      (define a "O")
      (define b "O")))

(print (list a '_ b))

(import (only (foo bar) b))

(print (list a '_ b)) ; 

(import (except (foo bar) b))

(print (list a '_ b))


