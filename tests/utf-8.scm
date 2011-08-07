
(define seed (time-ms))

;; decode 1000 encoded (valid) strings

(import lib-unicode last-code-point)

(let loop ((rst (seed->rands seed)) (n 100))
	(if (= n 0)
		(print "ok")
		(lets
			((rst len (rnd rst 100))
			 (rst cps (random-numbers rst (+ last-code-point 1) len)))
			;; could also use ((o utf8-decode utf8-encode) data), but using strings
			;; to also touch that code
			;(show " => " (list->string cps))
			(if (equal? cps (string->list (bytes->string (string->bytes (list->string cps)))))
				(loop rst (- n 1))
				(show "failed for " cps)))))

