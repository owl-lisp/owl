;;;
;;; Minimal thread-based pseudo-mutable values 
;;;

(define-library (owl variable)

   (export
      make-variable)

   (import
      (owl defmac)
      (only (owl syscall) mail interact wait-mail thread thunk->thread)
      (owl ff))

   (begin

      ;; unique value
      (define read-msg (list 1))

      (define (handler id)
         (case-lambda
            (() (interact id read-msg))          ;; read (sync)
            ((val) (mail id (cons 'set val)))    ;; write (async)
            ((op val) (mail id (cons op val))))) ;; write (async)

      (define (store val)
         (lets ((envelope (wait-mail))
                (from msg envelope))
            (if (eq? msg read-msg)
               (begin
                  (mail from val)
                  (store val))
               (let ((op (car msg)))
                  (cond
                     ((eq? op 'set)
                        (store (cdr msg)))
                     ((eq? op 'call)
                        (store ((cdr msg) val)))
                     (else
                        (store val)))))))

      (define (make-variable)
         (let ((id (list 'var)))
            (thread id (store #false))
            (handler id)))))
