(import (owl queue))

; test by mirroring random operations with lists and (append lst (list a)) for snoc

(define (uncons l d) (if (null? l) (values d l) (values (car l) (cdr l))))
(define (snoc a l) (append l (list a)))
(define (unsnoc l d) (let ((r (reverse l))) (if (null? r) (values d r) (values (car r) (reverse (cdr r))))))

; cl and l should have equal content
; using balanced consing and unconsing, so this will start with short lists 
; hitting null a few times and then will eventually wander to longer lists.
(define (test rst cl l i) 
   (if (eq? i 0)
      (if (equal? l (queue->list cl))
         (print "done")
         (print "lists differ..."))
      (lets ((rst op (rand rst 5)))
         (cond
            ((eq? op 0) ; cons a random number
               (lets ((rst n (rand rst #xffff)))
                  (test rst (qcons n cl) (cons n l) (- i 1))))
            ((eq? op 1)
               (lets ((rst n (rand rst 256)))
                  (test rst (qsnoc n cl) (snoc n l) (- i 1))))
            ((eq? op 2)
               (lets 
                  ((a cl (quncons cl -1))
                   (b  l (uncons   l -1)))
                  (if (= a b)
                     (test rst cl l (- i 1))
                     (error "uncons gave different results for " (list 'cl cl 'l l)))))
            ((eq? op 3)
               (lets 
                  ((a cl (qunsnoc cl -1))
                   (b  l (unsnoc   l -1)))
                  (if (= a b)
                     (test rst cl l (- i 1))
                     (error "unsnoc gave different results for " (list 'cl cl 'l l)))))
            (else
               (if (not (equal? l (queue->list cl)))
                  (error "queues are unreliable: " (list 'l l 'cl cl))
                  (test rst cl l (- i 1))))))))

(test (seed->rands (time-ms)) qnull null 1000)

