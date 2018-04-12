(import (owl io))

(thread
   (lfold
      (λ (nth packet)
         (print packet)
         (if (= nth 3)
            (halt 0)
            (+ nth 1)))
      1
      (udp-packets 31337)))

(sleep 100)

(define sock (udp-client-socket))

(for-each
   (λ (n)
      (send-udp-packet sock (vector 127 0 0 1) 31337 (vector 42 42 42 n)))
   (iota 240 5 255))
