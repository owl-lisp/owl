(define-library (owl syscall)

   (export
      syscall error interact fork accept-mail wait-mail check-mail
      exit-owl release-thread catch-thread set-signal-action
      single-thread? kill mail fork-linked-server fork-server
      return-mails fork-server fork-linked fork-named exit-thread exit-owl
      poll-mail-from start-profiling stop-profiling running-threads par*
      par por* por)

   (import 
      (owl defmac)
      (owl primop))

   (begin

      (define (syscall op a b)
         (call/cc (λ (resume) (sys resume op a b))))

      (define (exit-thread value)
         (syscall 2 value value))

      ;; 3 = vm thrown error
      (define (fork-named name thunk)
         (syscall 4 (list name) thunk))

      (define (fork-linked name thunk)
         (syscall 4 (list name 'link) thunk))

      (define (fork-server name handler)
         (syscall 4 (list name 'mailbox) handler))

      (define (error reason info)
         (syscall 5 reason info))

      (define (return-mails rmails)
         (syscall 6 rmails rmails))

      (define (fork-linked-server name handler)
         (syscall 4 (list name 'mailbox 'link) handler))

      (define (running-threads)
         (syscall 8 #false #false))

      (define (mail id msg)
         (syscall 9 id msg))

      (define (kill id) 
         (syscall 15 id #false))

      (define (single-thread?)
         (syscall 7 #true #true))
         
      (define (set-signal-action choice)
         (syscall 12 choice #false))

      (define (catch-thread id)
         (syscall 17 #true id))

      (define (release-thread thread)
         (syscall 17 #false thread))

      (define (exit-owl value)
         (syscall 19 value value) ;; set exit value proposal in thread scheduler
         (exit-thread value))     ;; stop self and leave the rest (io etc) running to completion

      ;; (executable ...) → (first-value . rest-ll) | (), or crash if something crashes in them
      (define (par* ts)
         (syscall 22 ts '()))

      ;; macro for calling from code directly
      (define-syntax par
         (syntax-rules ()
            ((par exp ...)
               (par* (list (λ () exp) ...)))))

      (define (por* ts)
         (let loop ((rss (par* ts)))
            (cond
               ((null? rss) #false)
               ((car rss) => (λ (result) result))
               (else (loop ((cdr rss)))))))
               
      (define-syntax por
         (syntax-rules ()
            ((por exp ...)
               (por* (list (λ () exp) ...)))))
    
      (define (wait-mail)           (syscall 13 #false #false))
      (define (check-mail)          (syscall 13 #false #true))

      (define (accept-mail pred)
         (let loop ((this (wait-mail)) (rev-spam '()))
            (cond
               ((pred this)
                  (return-mails rev-spam) ; return the other mails to mailbox as such
                  this)
               (else
                  (loop (wait-mail) (cons this rev-spam))))))

      ;; wait mail from given thread for a while, giving other threads time (or sleeping) while waiting
      ;; todo: could interact with the sleeper thread to allow vm go to sleep between rounds

      (define (return-from-wait value spam)
         (if (null? spam)
            value
            (begin
               (return-mails spam)
               value)))

      (define (poll-mail-from id rounds default)
         (let loop ((envp (check-mail)) (spam '()) (rounds rounds))
            (cond
               ((not envp)
                  (if (eq? rounds 0)
                     (return-from-wait default spam)
                     ;; no mail, request a thread switch and recurse, at which point all other threads have moved
                     (begin   
                        ;(set-ticker 0) ;; FIXME restore this when librarized
                        ;; no bignum math yet at this point
                        (lets ((rounds _ (fx- rounds 1)))
                           (loop (check-mail) spam rounds)))))
               ((eq? (ref envp 1) id)
                  ;; got it
                  (return-from-wait (ref envp 2) spam))
               (else
                  ;; got spam, keep waiting
                  (loop (check-mail) (cons envp spam) rounds)))))
         

      (define (fork thunk)
         ; the tuple is fresh and therefore a proper although rather 
         ; nondescriptive thread name
         (fork-named (tuple 'anonimas) thunk))


      ; Message passing (aka mailing) is asynchronous, and at least 
      ; in a one-core environment order-preserving. interact is like 
      ; mail, but it blocks the thread until the desired response 
      ; arrives. Messages are of the form #(<sender-id> <message>).

      (define (interact whom message)
         (mail whom message)
         (ref (accept-mail (λ (env) (eq? (ref env 1) whom))) 2))

      (define (start-profiling)
         (syscall 20 #true #true))

      (define (stop-profiling)
         (syscall 21 #true #true))
))
