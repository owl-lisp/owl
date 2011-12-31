(define (print arg)
   (display arg)
   (display "
"))

#|

Testing block comments. They could also be nested on second thought...

|#

(let
   ((a '|foo bar|)
    (b (string->symbol "foo bar")))
   (if (not (eq? a b))  
      (print "symbolic failure 1")))

(if (not (= 42 ((lambda (|foo|) foo) 42)))
   (print "symbolic failure 2"))

(print "all done")
