(define-module lib-syscall

   (export syscall error interact fork accept-mail wait-mail check-mail exi-owl release-thread catch-thread set-signal-action single-thread? kill mail fork-linked-server fork-server return-mails fork-server fork-linked fork-named exit-thread exit-owl poll-mail-from)

   (define (syscall op a b)
      (call/cc (λ (resume) (sys resume op a b))))

   (define (exit-thread value)
      (syscall 2 value value))

   (define (fork-named name thunk)
      (syscall 4 (list name) thunk))

   (define (fork-linked name thunk)
      (syscall 4 (list name 'link) thunk))

   (define (fork-server name handler)
      (syscall 4 (list name 'mailbox) handler))

   (define (return-mails rmails)
      (syscall 6 rmails rmails))

   (define (fork-linked-server name handler)
      (syscall 4 (list name 'mailbox 'link) handler))

   (define (mail id msg)
      (syscall 9 id msg))

   (define (kill id) 
      (syscall 15 id False))

   (define (single-thread?)
      (syscall 7 True True))
      
   (define (set-signal-action choice)
      (syscall 12 choice False))

   (define (catch-thread id)
      (syscall 17 True id))

   (define (release-thread thread)
      (syscall 17 False thread))

   (define (exit-owl value)
      (syscall 19 value value) ;; set exit value proposal in thread scheduler
      (exit-thread value))     ;; stop self and leave the rest (io etc) running to completion

   (define (wait-mail)           (syscall 13 False False))
   (define (check-mail)          (syscall 13 False True))

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
                     (set-ticker 0)
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

   (define (error reason info)
      (syscall 5 reason info))

)
