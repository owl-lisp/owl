#|(define-library (my test)
   (export life)
   (import 
      (except (scheme base) set!)
      (scheme write)
      (example grid))
   (begin
      (define foo 42)
      (define (life x) 
         (+ x foo))))
|#

;; changes order to (name import ... begin ... export)
(define-syntax define-library
   (syntax-rules (export import begin _define-library)
      ((_ name (import . i) ... (begin . b) ... (export . e))
       (_define-library 'name '(import . i) ... '(begin . b) ... '(export . e)))
      ((_ x ... (export . e) term . tl)
       (_ x ... term (export . e) . tl))
      ((_ x ... (begin . b) (import . i) . tl)
       (_ x ... (import . i) (begin . b) . tl))
      ((_ . wtf)
         (syntax-error "Weird library contents: " (quote . wtf)))))

'(print
   (define-library (my lib)
      (import lol)
      (begin   
         (define foo 42))
      (export foo)))
