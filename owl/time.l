;; todo: date handling

(define (elapsed-real-time thunk)
	(display "timing: ")
	(flush-port 1)
	(lets
		((ss sms (clock))
		 (res (thunk))
		 (es ems (clock))
		 (elapsed
			(- (+ ems (* es 1000))
				(+ sms (* ss 1000)))))
		(show elapsed "ms")
		res))

(define timed elapsed-real-time)

(define (time)
   (lets ((ss ms (clock))) ss))

(define (time-ms) 
   (lets ((ss ms (clock))) 
      (+ (* ss 1000) ms)))

