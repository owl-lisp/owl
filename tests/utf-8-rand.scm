
(define seed (time-ms))

;; decode 1000 encoded (valid) strings

(import (only (owl unicode) last-code-point))

(let loop ((rst (seed->rands seed)) (n 100))
	(if (= n 0)
		(print "ok")
		(lets
			((rst len (rand rst 100))
			 (rst cps (random-numbers rst (+ last-code-point 1) len)))
			;; could also use ((o utf8-decode utf8-encode) data), but using strings
			;; to also touch that code
			;(print " => " (list->string cps))
			(if (equal? cps (string->list (bytes->string (string->bytes (list->string cps)))))
				(loop rst (- n 1))
				(print "failed for " cps)))))

