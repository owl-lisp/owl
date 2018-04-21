
'(foo (bar baz))

42

2+3i

(import 
   (owl sexp)
   (owl parse))

(define foo
   (open-input-file "tests/sexp.scm"))

(print
   (read foo)) ;; → (quote (foo (bar (baz))))

(close-port foo)

(define fd (open-input-file "tests/sexp.scm"))

(define es (fd->exp-stream fd #false sexp-parser #f #f))

(print (force-ll (ltake es 3)))

