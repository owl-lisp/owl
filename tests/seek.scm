
(import (owl sys))

(define fd (open-output-file "tmp/seek"))

(print "port -> " (port? fd))

(define foo
   (vector 0 1 2 3 4 5 6 7 8 9))

(write-vector foo fd)

(print "start -> " (seek-set fd 0))

(write-vector #(100 101 102) fd)

(print "end -> " (seek-end fd))

(write-vector #(10 11 12) fd)

(print "take five -> " (seek-set fd 5))

(write-vector #(55 66 77) fd)

(close-port fd)

(print "result: " (file->vector "tmp/seek"))



