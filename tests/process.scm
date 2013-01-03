
(import (prefix (owl sys) sys-))

(define exit-values '(42 43 44))

(print "forking children")

(define pids
   (map
      (λ (exit-val)
         (let ((x (sys-fork)))
            (cond
               ((not x)
                  (print "FORK FAILED")
                  #false)
               ((eq? x #true)
                  ;; exit with given value from child processes
                  (halt exit-val))
               (else
                  ;; return pid to parent
                  x))))
      exit-values))

(print "forked child processes")

(for-each
   (λ (pid)
      (print "child exited with " (sys-wait pid)))
   pids)

(print "done")

