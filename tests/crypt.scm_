(import (owl crypt))

(define (test-str str key)
   (lets ((encd (encrypt str key))
          (back-v (decrypt encd key))
          (back (bytes->string (vector->list back-v))))
      (if (equal? str back)
         (print "ok: " str)
         (print "ERROR: " str))))

(define key "Strand3dP0ssum2016/weakery")

(test-str "" key)
(test-str "a" key)
(test-str "aa" key)
(test-str "Kärsämäki" key)
