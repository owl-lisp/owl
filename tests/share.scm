(define (foo a b c d) c)
(define (bar x y z w) z)

(if (eq? foo bar)
   (print "shared"))
