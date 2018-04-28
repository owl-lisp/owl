;;;
;;; Minimal thread-based pseudo-mutable values 
;;;

(define-library (owl variable)

   (export
      make-variable
      link-variable)

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
      
      (define (start-variable id val)
         (thread id (store val))
         (handler id))
      
      (define make-variable
         (case-lambda
            ((id val) (start-variable id val))
            ((id) (start-variable id #false))
            (() (start-variable (list 'var) #false))))
      
      (define (link-variable id)
         (handler id))))
