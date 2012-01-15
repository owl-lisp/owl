;;;
;;; Queues (double-ended lists)
;;;

; todo:
;   - halve the list instead of full reversal to keep more 
;     efficient access to both ends of the list.

; note, the name snoc (reverse of cons) comes from Okasaki's book

(define-module lib-queue-halving

   (export 
      qnull 
      qnull?         ; Q → bool
      qlen            ; Q → nat
      qcons          ; a Q → Q' -- cons to head
      qsnoc          ; a Q → Q' -- cons to tail
      quncons         ; Q d → a Q' 
      qunsnoc         ; Q d → a Q'
      qcar           ; Q → a Q' -- return first and Q' with O(1) qcar
      qrac           ; Q → a Q' -- return last  and Q' with O(1) qrac
      list->queue
      queue->list)

   ; cl = (hd . rtl)
   ;
   ;     ((a b c d) . ())
   ;   = ((a b) . (d c))
   ;   = (() . (d c b a))

   (define qnull '(() . ()))

   (define (qnull? cl)
      (and (null? (car cl)) (null? (cdr cl))))

   (define (half-rev l)
      (let loop ((l l) (h l) (s False) (o null)) ;; todo: check that at least one is always moved, like () (a)
         (cond
            ((null? h) (values (reverse l) (reverse o)))
            (s (loop l (cdr h) False o))
            (else (loop (cdr l) (cdr h) True (cons (car l) o))))))

   (define (qlen cl)
      (+ (length (car cl)) (length (cdr cl))))

   ; cons to head
   (define (qcons a cl)
      (lets ((hd rtl cl))
         (cons (cons a hd) rtl)))
   
   ; cons to tail
   (define (qsnoc a cl)
      (lets ((hd rtl cl))
         (cons hd (cons a rtl))))

   ; (quncons (qcons a b)) = (values a b)
   (define (quncons cl d)
      (lets ((hd rtl cl))
         (if (null? hd)
            (cond
               ((null? rtl)
                  (values d qnull))
               ((null? (cdr rtl))
                  (values (car rtl) qnull))
               (else
                  (lets ((hd rtl (half-rev rtl)))
                     (values (car hd) (cons (cdr hd) rtl)))))
            (values (car hd) (cons (cdr hd) rtl)))))

   ; Q → a Q'
   (define (qcar cl)
      (lets ((hd rtl cl))
         (if (null? hd)
            (cond
               ((null? rtl)
                  (error "qcar: empty queue: " cl))
               (else
                  (lets ((hd rtl (half-rev rtl)))
                     (values (car hd) (cons hd rtl)))))
            (values (car hd) cl))))

   ; Q → a Q'
   (define (qrac cl)
      (lets ((hd rtl cl))
         (if (null? rtl)
            (cond
               ((null? hd)
                  (error "qrac: empty queue: " cl))
               (else
                  (lets ((rtl hd (half-rev hd)))
                     (values (car rtl) (cons hd rtl)))))
            (values (car rtl) cl))))

   ; (qunsnoc (qsnoc a b)) = (values a b)
   (define (qunsnoc cl d)
      (lets ((hd rtl cl))
         (if (null? rtl)
            (cond
               ((null? hd)
                  (values d qnull))
               ((null? (cdr hd))
                  (values (car hd) qnull))
               (else
                  (lets ((rtl hd (half-rev hd)))
                     (values (car rtl) (cons hd (cdr rtl))))))
            (values (car rtl) (cons hd (cdr rtl))))))

   (define (list->queue lst) (cons lst null))

   (define (queue->list cl)
      (lets ((hd rtl cl))
         (if (null? cl)
            hd
            (append hd (reverse rtl)))))

   ; could add append, map etc here if needed.

)

; see Okasaki's book for other options. This one seems to be fairly good for 
; most use cases.

(define lib-queue lib-queue-halving)


