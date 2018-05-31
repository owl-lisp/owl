;; Start 10K threads and pass a message through them

(define n 10000)

(define (forwarder to)
		(let loop ()
			(mail to (ref (wait-mail) 2))
			(loop)))

(print* (list "Starting " n " threads."))

;; first one is special
(thread 1
		(let ((msg (ref (wait-mail) 2)))
			(mail 2 "pass this around")
			(let ((result (wait-mail)))
				(print* (list "Thread 1: " (ref result 1) " sent me \"" (ref result 2)"\"")))))

;; 2-(n-1) just forward to the next
(let loop ((id (- n 1)) (next n))
	(when (> id 1)
		(thread id (forwarder next))
		(loop (- id 1) id)))

;; last one sends to first
(thread n (forwarder 1))

(print "Sending message.")

(mail 1 'start)
