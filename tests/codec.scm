
(import (owl codec))

(define (try-hex str)
  (lets
    ((encd (hex-encode str))
     (decd (hex-decode encd)))
    (if (equal? str decd)
      (print "hex: ok '" str "'")
      (print "hex fail: '" str "' -> '" encd "' -> '" decd "'"))))

(try-hex "")
(try-hex "a")
(try-hex "ab")
(try-hex "foobar")
(try-hex "Kärsämäki")

