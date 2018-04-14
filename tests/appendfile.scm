
(import (owl sys))

(print "init -> " (vector->file #(42 42 42) "tmp/app"))

(print "start -> " (file->vector "tmp/app"))

(print "1st size -> " (cdr (assq 'size (stat "tmp/app" #f))))

(let ((port (open-append-file "tmp/app")))
   (print-to port "aaa")
   (print "2nd size -> " (cdr (assq 'size (stat port #f))))
   (close-port port))

(print "*** + append aaa\\n -> " (file->vector "tmp/app"))

(let ((port (open-output-file "tmp/app")))
   (print-to port "bbb")
   (close-port port))

(print "truncate + bbb -> " (file->vector "tmp/app"))

(print "remove -> " (unlink "tmp/app"))

