;;;
;;; Minimal thread-based pseudo-mutable values
;;;

(define-library (owl variable)

   (export
      make-variable
      link-variable)

   (import
      (owl defmac)
      (owl list)
      (owl io)
      (only (owl syscall) mail interact wait-mail thread thunk->thread link)
      (owl ff))

   (begin

      (define (handler id)
         (case-lambda
            (() (interact id '(get)))                ;; read (sync)
            ((val) (mail id (cons 'set val)))        ;; write (async, which is efficient but may cause odd bugs if confused with sync 'set)
            ((op val) (interact id (cons op val))))) ;; any (sync)

      (define (store val env)
         (lets ((envelope (wait-mail))
                (from msg envelope))
            (if (pair? msg)
               (let ((op (car msg)))
                  (cond
                     ((eq? op 'get)
                        (mail from val)
                        (store val env))
                     ((eq? op 'set)
                        (store (cdr msg) env))
                     ((eq? op 'call)
                        (let ((res ((cdr msg) val)))
                           (mail from res)
                           (store res env)))
                     ((eq? op 'get-local)
                        (mail from (get env from (cdr msg)))
                        (store val env))
                     ((eq? op 'set-local)
                        (mail from (cdr msg))
                        (let ((lenv (get env from #f)))
                           (if lenv
                              (store val
                                 (fupd env from (cdr msg)))
                              (begin
                                 (link from) ;; we want to know when it exists
                                 (store val (put env from (cdr msg)))))))
                     (else
                        ;; spam
                        (store val env))))
               ;; a linked thread has finished - drop local env
               (store val (del env from)))))

      (define (start-variable id val)
         (thread id (store val #empty))
         (handler id))

      (define make-variable
         (case-lambda
            ((id val) (start-variable id val))
            ((id) (start-variable id #false))
            (() (start-variable (list 'var) #false))))

      (define link-variable handler)
))
