;;;
;;; Thread controller
;;;

;; thread controller is like the kernel of owl lisp. it handles 
;; activation and suspension of threads, and has a tuple of 
;; functions are like the system calls via which threads can 
;; send requests via the thread scheduler to other threads or 
;; the underlying system.

;; todo: make it a bug to send mail to a thread having no inbox.
;; todo: switch thread inboxen to be lib-queue things instead of ad-hoc ones

,r "owl/mcp-tags.l"
,r "owl/queue.l"


(define-module lib-threads

   (export thread-controller wait)

   (import lib-queue) ;; for mail queueing

	;; fixme: wait should use fixnum math to be as light as possible
	(define (wait n)
		(if (eq? n 0)
			n
			(begin 
				(set-ticker 0)
				(wait (- n 1)))))

   (define (bad-syscall id a b c todo done state)
      (system-println "mcp: got bad syscall")
      (values todo done state))

   ; -> state x False|waked-thread
   (define (deliver-mail state to envelope)
      (let ((st (get state to False)))
         (cond
            ((pair? st) ;; currently working, leave a mail to inbox queue
               (values (fupd state to (qsnoc envelope st)) False))
            ((not st) ;; no such thread, or just no inbox
               (system-stderr (bytes->string (foldr renderer '(10) (list "ol: dropping envelope " envelope " to nonexistent local thread " to))))
               (values state False))
            (else ;; activate the state function
               (values 
                  (fupd state to qnull) ;; leave an inbox
                  (tuple to (λ () (st envelope)))))))) ;; activate it

   (define (deliver-messages todo done state subs msg tc)
      (if (null? subs)
         (tc tc todo done state)
         (lets ((state waked (deliver-mail state (car subs) msg)))
            (if waked
               (deliver-messages (cons waked todo) done state (cdr subs) msg tc)
               (deliver-messages todo done state (cdr subs) msg tc)))))

   ; remove the thread and report to any interested parties about the event 
   (define (drop-delivering todo done state id msg tc)
      (lets
         ((links (get state link-tag False))
          (subscribers (get links id null)))
         (if (null? subscribers)
            (if (eq? (ref (ref msg 2) 1) 'finished)
               ; a thread finishes silently
               (tc tc todo done (del state id))
               ; a thread poofs silently. should make some noise to stderr.
               (tc tc todo done (del state id)))
            (deliver-messages todo done 
               (del (fupd state link-tag (del links id)) id)
               subscribers msg tc))))

   ;; thread dropping, O(n)
   (define (drop-from-list lst tid) ; -> lst'
      (cond
         ((null? lst) lst)
         ((eq? (ref (car lst) 1) tid) (cdr lst))
         (else 
            (cons (car lst) 
               (drop-from-list (cdr lst) tid)))))

   ; drop a possibly running thread and notify linked 
   (define (drop-thread id todo done state msg tc) ; -> todo' x done' x state'
      (drop-delivering 
         (drop-from-list todo id)
         (drop-from-list done id)
         state id msg tc))

   ; l id → False|thread l', O(n) running threads
   (define (catch-thread l id)
      (if (null? l) 
         (values False l)
         (let ((this (car l)))
            (if (eq? id (ref this 1))
               (values this (cdr l))
               (lets ((caught l (catch-thread (cdr l) id)))
                  (values caught (cons this l)))))))

   (define return-value-tag "rval") ; a unique key if thread state ff

   ; mcp syscalls grab the functions from here to transform the state 

   ;; syscalls used when profiler is running
   (define mcp-syscalls-during-profiling
      (tuple

         ; 1, runnig and time slice exhausted (the usual suspect, could be on a shorter branch)
         (λ (id a b c todo done state tc)
            ; (system-println "syscall 1 - switch thread")
            (tc tc todo (cons (tuple id a) done) state))

         ; 2, thread finished, drop
         (λ (id a b c todo done state tc)
            ; (system-println "mcp: syscall 2 -- thread finished")
            (drop-delivering todo done state id 
               (tuple id (tuple 'finished a b c)) tc))

         ; 3, vm thrown error
         (λ (id a b c todo done state tc)
            ; (system-println "mcp: syscall 3 -- vm error")
            (drop-delivering todo done state id 
               (tuple id (tuple 'crashed a b c)) tc))
         
         ; 4, fork
         (λ (id cont opts thunk todo done state tc)
            (lets 
               ((new-id (car opts))
                (todo (ilist (tuple new-id thunk) (tuple id (λ () (cont new-id))) todo))
                (state
                   (for state (cdr opts)
                     (λ (state req)
                        (cond
                           ((eq? req 'link)
                              ;; forker wants to receive any issues the thread runs into
                              (let ((links (get state link-tag False)))
                                 (put state link-tag
                                    (put links new-id (list id)))))
                           ((eq? req 'mailbox)
                              ;; the thread should have a mailbox for communication in state
                              (put state new-id qnull))
                           (else
                              (system-println "fork: bad parameter")
                              state))))))
               (tc tc todo done state)))

         ; 5, user thrown error
         (λ (id a b c todo done state tc)
            ; (system-println "mcp: syscall 5 -- user poof")
            (drop-delivering todo done state id 
               (tuple id (tuple 'error a b c)) tc))

         ;; return mails to my own inbox (in reverse order, newest on top)

         ; 6, (return-mails rl)
         (λ (id cont rmails foo todo done state tc)
            (let ((queue (get state id qnull)))
               (tc tc (cons (tuple id (λ () (cont 'done))) todo)
                  done (put state id (foldr qsnoc queue rmails)))))

         ; 7, am i the only thread?
         (λ (id cont b c todo done state tc)
            (tc tc 
               (cons (tuple id (λ () (cont (and (null? todo) (null? done))))) todo)
               done state))
            
         ; 8, fork a server (fork + add a queue to state)
         bad-syscall

         ; 9, send mail
         (λ (id cont to msg todo done state tc)
            ;(system-println "syscall 9 - mail")
            (let ((todo (cons (tuple id (λ () (cont 'delivered))) todo)))
               ; send a normal mail
               (lets ((state waked (deliver-mail state to (tuple id msg))))
                  (if waked
                     (tc tc (ilist (car todo) waked (cdr todo)) done state)
                     (tc tc todo done state)))))

         ;; not in use atm, was old method of starting mcp (and possibly will also be the new one)
         ; 10, have a break, have a quick-chat-with-mcp-repl
         (λ (id a b c todo done state thread-controller)
            ; break during eval (often also via poll)
            ;(system-println "syscall 10 - break")
            (let ((all-threads (cons (tuple id a) (append todo done))))
               (thread-controller 
                  (list 
                     (tuple 'mcp-repl 
                        (λ () 
                           ;; mcp forks the io threads it needs
                           ((get state mcp-tag mcp-halt) ; default to standard mcp 
                              all-threads state thread-controller))))
                  null False)))

         
         ; 11, reset mcp state (usually means exit from mcp repl)
         (λ (id cont threads state xtodo xdone xstate tc)
            ; (system-println "syscall 11 - swapping mcp state")
            (tc tc threads null state))

         ;; todo: allow setting the break function itself here
         ; 12, set break action
         (λ (id cont choice x todo done state tc)
            (tc tc  
               (cons (tuple id (λ () (cont True))) todo)
               done (put state mcp-tag choice)))

         ; 13, look for mail in my inbox at state
         (λ (id cont foo nonblock? todo done state tc)
            (lets ((valp queue (quncons (get state id qnull) F)))
               (cond
                  (valp      ;; envelope popped from inbox
                     (tc tc (cons (tuple id (λ () (cont valp))) todo) done
                        (fupd state id queue)))
                  (nonblock? ;; just tell there is no mail with False
                     (tc tc (cons (tuple id (λ () (cont False))) todo) done state))
                  (else      ;; leave thread continuation waiting
                     (tc tc todo done (put state id cont))))))

         ;; todo: switch memory limit to a hard one in ovm.c
         ; 14, memory limit was exceeded 
         (λ (id a b c todo done state tc)
            (system-println "syscall 14 - memlimit exceeded, dropping a thread")
            ; for now, kill the currently active thread (a bit dangerous) 
            (drop-delivering todo done state id 
               (tuple id (tuple 'crashed 'memory-limit b c)) tc))

         ; 15, drop local thread
         (λ (id cont target c todo done state tc)
            (drop-thread target
               (cons (tuple id (λ () (cont (tuple 'killing target)))) todo)
               done state (tuple 'killed-by id) tc))

         ; 16, wrap the whole world to a thunk
         (λ (id cont path c todo done state tc)
            (let
               ((resume
                  (λ (args)
                     (tc tc (cons (tuple id (λ () (cont 'resumed))) todo)
                        done state))))
               (tc tc (cons (tuple id (λ () (cont resume))) todo) done state)))

         ; 17, catch or release a running thread (not touching mailbox etc)
         (λ (id cont catch? info todo done state tc)
            (if catch?
               (lets
                  ((all (append todo done))
                   (val all (catch-thread all info)))
                  (tc
                     (cons (tuple id (λ () (cont val))) all)
                     null state))
               (tc
                  (ilist (tuple id (λ () (cont 'released))) info todo)
                  done state)))

         ; 18, get a list of currently running thread ids
         (λ (id cont b c todo done state tc)
            (lets
               ((grab (λ (l n) (cons (ref n 1) l)))
                (ids (fold grab (fold grab null todo) done)))
               (tc tc (cons (tuple id (λ () (cont (cons id ids)))) todo) done state)))

         ; 19, set return value proposal
         (λ (id cont b c todo done state tc)
            (tc tc (cons (tuple id (λ () (cont b))) todo) done (put state return-value-tag b)))
        
         ;;; 20 & 21 change during profiling 

         ; 20, start profiling, no-op during profiling returning 'already-profiling
         (λ (id cont b c todo done state tc) 
            (tc tc (cons (tuple id (λ () (cont 'already-profiling))) todo) done state))
         
         ; 21, end profiling, resume old ones, pass profiling info 
         (λ (id cont b c todo done state tc) 
            (lets
               ((prof (get state 'prof False)) ;; ff storing profiling info
                (tc (get prof 'tc False))      ;; normal thread scheduler
                (prof (del prof 'tc)))         ;; give just the collected data to thread
               (tc tc (cons (tuple id (λ () (id (cont prof)))) todo) done 
                  (del state 'prof))))
   ))

   ;; todo: add deadlock detection here (and other bad terminal waits)
   (define (halt-thread-controller state)
      (get state return-value-tag 0))

   ;; store profiling info about this call
   (define (update-state state exec)
      (let ((prof (get state 'prof F)))
         (fupd state 'prof
            (put prof 'counter (+ 1 (get prof 'counter 0))))))

   (define mcp-syscalls
      (lets
         ((syscalls mcp-syscalls-during-profiling)
          (syscalls
            (set syscalls 20
               (λ (id cont b c todo done state tc)
                  ;; make a new thread scheduler using the other syscall set
                  (define (scheduler self todo done state)
                     (if (eq? todo null)
                        (if (null? done)
                           (halt-thread-controller state)
                           (self self done null state))
                        (lets 
                           ((this todo todo)
                            (id st this)
                            (state (update-state state st))
                            (op a b c (run st 10))) ;; capture every 10 samples (nondeterministic later)
                           (if (eq? op 1)
                              ; out of time, usual suspect, short path here
                              (self self todo (cons (tuple id a) done) state)
                              ((ref mcp-syscalls-during-profiling op) id a b c todo done state self))))) ; <- difference here

                  (scheduler scheduler (cons (tuple id (λ () (cont 'started-profiling))) todo) done 
                     (put state 'prof           ;; profiling data is stored under key 'prof
                        (put state 'tc tc)))))) ;; store normal scheduler there for resuming on syscall 21
          (syscalls
            (set syscalls 21 ;; end-profiling syscall doesn't do anything when not profiling
               (λ (id cont b c todo done state tc) 
                  (tc tc (cons (tuple id (λ () (cont 'not-profiling-you-fool))) todo) done state)))))
         syscalls))

   (define (enter-mcp controller threads state)
      ; could break here also when threads is just repl-input
      (controller
         (list 
            (tuple 'mcp 
               (λ ()
                  ((get state mcp-tag mcp-halt) ; exit by default
                     threads state controller))))
         null False))


   ; (enter-mcp thread-controller done state) -- no way to go here without the poll, rethink that

   (define (thread-controller self todo done state)
      (if (eq? todo null)
         (if (null? done)
            (halt-thread-controller state)
            (self self done null state))
         (lets 
            ((this todo todo)
             (id st this)
             (op a b c (run st thread-quantum)))
            (if (eq? op 1)
               ; out of time, usual suspect, short path here
               (self self todo (cons (tuple id a) done) state)
               ((ref mcp-syscalls op) id a b c todo done state self)))))


)
