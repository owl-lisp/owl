;;;
;;; Thread controller
;;;

;; thread controller is like the kernel of owl lisp. it handles 
;; activation and suspension of threads, and has a tuple of 
;; functions which are like the system calls, via which a thread
;; send requests via the thread scheduler to other threads or 
;; the underlying system.

;; todo: make it a bug to send mail to a thread having no inbox.

(define-library (owl thread)

   (export 
      start-thread-controller
      thread-controller 
      repl-signal-handler)

   (import
      (owl defmac)
      (owl queue)
      (owl syscall)
      (owl ff)
      (owl function)
      (owl primop)
      (owl list)
      (owl math)
      (owl tuple)
      (owl string)
      (owl render)
      (owl env)
      (owl io))

   (begin

      (define (bad-syscall id a b c todo done state)
         (system-println "mcp: got bad syscall")
         (values todo done state))

      ; -> state x #false|waked-thread
      (define (deliver-mail state to envelope)
         (let ((st (get state to #false)))
            (cond
               ((pair? st) ;; currently working, leave a mail to inbox queue
                  (values (fupd state to (qsnoc envelope st)) #false))
               ((not st) ;; no such thread, or just no inbox
                  (system-stderr "ol: dropping envelope to missing thread\n")
                  (system-stderr "ol: envelope ")
                  (system-stderr (bytes->string (bytes->string (render envelope '(10)))))
                  (values state #false))
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

      (define eval-break-message (tuple 'repl-eval (tuple 'breaked)))

      (define (subscribers-of state id)
         (get (get state link-tag empty) id null))

      ; remove the thread and report to any interested parties about the event 
      (define (drop-delivering todo done state id msg tc)
         (let ((subs (subscribers-of state id)))
            (if (null? subs)
               (begin
                  ;; no threads were waiting for something that is being removed, so tell stderr about it
                  ;(print*-to stderr "VM: thread " id " exited due to " msg)
                  (tc tc todo done (del state id)))
               (deliver-messages todo done 
                  (del (fupd state link-tag (del (get state link-tag empty) id)) id)
                  subs msg tc))))

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

      ; l id → #false|thread l', O(n) running threads
      (define (catch-thread l id)
         (if (null? l) 
            (values #false l)
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

            ; 1, runnig and time slice exhausted (the usual suspect, handled directly in scheduler)
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
               ;(system-println "mcp: syscall 3 -- vm error")
               ;; set crashed exit value proposal 
               (let ((state (put state return-value-tag 127)))
                  (drop-delivering todo done state id 
                     (tuple id (tuple 'crashed a b c)) tc)))
            
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
                                 (let ((links (get state link-tag empty)))
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
               
            ; 8, get running thread ids (sans self)
            (λ (id cont b c todo done state tc)
               (let 
                  ((ids
                     (append
                        (map (λ (x) (ref x 1)) todo)
                        (map (λ (x) (ref x 1)) done))))
                  (tc tc 
                     (cons 
                        (tuple id (λ () (cont ids)))
                        todo)
                     done state)))

            ; 9, send mail
            (λ (id cont to msg todo done state tc)
               ;(system-println "syscall 9 - mail")
               (let ((todo (cons (tuple id (λ () (cont 'delivered))) todo)))
                  ; send a normal mail
                  (lets ((state waked (deliver-mail state to (tuple id msg))))
                     (if waked
                        (tc tc (ilist (car todo) waked (cdr todo)) done state)
                        (tc tc todo done state)))))

            ; 10, breaked - call signal handler
            (λ (id a b c todo done state thread-controller)
               ; (system-println "syscall 10 - break")
               (let ((all-threads (cons (tuple id a) (append todo done))))
                  ;; tailcall signal handler and pass controller to allow resuming operation
                  ((get state signal-tag signal-halt) ; default to standard mcp 
                     all-threads state thread-controller)))
            
            ; 11, reset mcp state (usually means exit from mcp repl)
            (λ (id cont threads state xtodo xdone xstate tc)
               ; (system-println "syscall 11 - swapping mcp state")
               (tc tc threads null state))

            ; 12, set break action
            (λ (id cont choice x todo done state tc)
               (tc tc  
                  (cons (tuple id (λ () (cont #true))) todo)
                  done (put state signal-tag choice)))

            ; 13, look for mail in my inbox at state
            (λ (id cont foo nonblock? todo done state tc)
               (lets ((valp queue (quncons (get state id qnull) #false)))
                  (cond
                     (valp      ;; envelope popped from inbox
                        (tc tc (cons (tuple id (λ () (cont valp))) todo) done
                           (fupd state id queue)))
                     (nonblock? ;; just tell there is no mail with #false
                        (tc tc (cons (tuple id (λ () (cont #false))) todo) done state))
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
                  done state (tuple target (tuple 'killed-by id)) tc))

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
                  ((prof (get state 'prof #false)) ;; ff storing profiling info
                   (tc (get prof 'tc #false))      ;; normal thread scheduler
                   (prof (del prof 'tc)))         ;; give just the collected data to thread
                  (tc tc (cons (tuple id (λ () (cont prof))) todo) done 
                     (del state 'prof))))

            ; 22, nestable parallel computation
            (λ (id cont opts c todo done state tc)
               (lets
                  ((por-state (tuple cont opts c)))
                  (tc tc (cons (tuple id por-state) todo) done state)))
      ))

      ;; todo: add deadlock detection here (and other bad terminal waits)
      (define (halt-thread-controller state)
         (get state return-value-tag 0))

      (define (bytecode-of thing default)
         (cond
            ((bytecode? thing) thing)
            ((function? thing) (bytecode-of (ref thing 1) default))
            (else default)))

      ;; store profiling info about this call
      ;; the exec is either a thunk to be run in a thread as a result of 
      ;; forking or a syscall being answered, or a vm-generated tuple which 
      ;; has arguments for the next function call and the function at the 
      ;; *last* slot of the tuple.

      (define (update-state state exec)
         (if (tuple? exec) ;; vm thread suspensions are tuples
            (lets
               ((bcode (bytecode-of (ref exec (size exec)) 'not-a-function)) ;; identify place based in bytecode which is inert
                (prof (get state 'prof #false))
                (count (get prof bcode 0))
                (prof (put prof bcode (+ count 1)))
                (state (fupd state 'prof prof)))
               state)
            ;; don't record anything for now for the rare thread starts and resumes with syscall results
            state))

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
                               (op a b c (run st 0)))
                              (if (eq? op 1)
                                 ; out of time, usual suspect, short path here
                                 (self self todo (cons (tuple id a) done) state)
                                 ((ref mcp-syscalls-during-profiling op) id a b c todo done state self))))) ; <- difference here

                     (scheduler scheduler (cons (tuple id (λ () (cont 'started-profiling))) todo) done 
                        (put state 'prof           ;; profiling data is stored under key 'prof
                           (put empty 'tc tc)))))) ;; store normal scheduler there for resuming on syscall 21
             (syscalls
               (set syscalls 21 ;; end-profiling syscall doesn't do anything when not profiling
                  (λ (id cont b c todo done state tc) 
                     (tc tc (cons (tuple id (λ () (cont 'not-profiling-you-fool))) todo) done state)))))
            syscalls))

      (define (enter-mcp controller threads state)
         ; could break here also when threads is just repl-input
         (controller controller
            (list 
               (tuple 'mcp 
                  (λ ()
                     ((get state signal-tag signal-halt) ; exit by default
                        threads state controller))))
            null empty))


      ; (enter-mcp thread-controller done state) -- no way to go here without the poll, rethink that

      ;; nested thread steps cause
      ;;    - exit and false -> forget todo & done
      ;;    - crash -> forget 

      ;; st=#(cont todo done) → finished? st'
      ;; finished? = #f -> new state but no result yet, #t = result found and state is thunk, else error
      ;; TODO: downgrade to single state and prune useless such nodes from tree whenever # of options is reduced down to 1
      (define (step-parallel st)
         (lets ((cont todo done st))
            (if (null? todo)
               (if (null? done)
                  ;; no options left
                  (values #true (λ () (cont null)))
                  ;; rewind the track, spin the record and take it back
                  (step-parallel (tuple cont done null)))
               (lets ((state todo todo))
                  (if (eq? (type state) type-tuple)
                     (lets ((fini state (step-parallel state)))
                        (if (eq? fini null)
                           ;; crashed, propagate
                           (values null state)
                           ;; either par->single or single->value state change, 
                           ;; but consumed a quantum already so handle it in next round
                           (values #false (tuple cont todo (cons state done)))))
                     (lets ((op a b c (run state thread-quantum)))
                        (cond
                           ((eq? op 1) ;; out of time, a is new state
                              (values #false 
                                 (tuple cont todo (cons a done))))
                           ((eq? op 2) ;; finished, return value and thunk to continue computation
                              (values #true
                                 (λ () (cont (cons a (λ () (syscall 22 todo done)))))))
                           ((eq? op 22) ;; start nested parallel computation
                              (lets ((contp a) (todop b) (donep c) 
                                     (por-state (tuple contp todop donep)))
                                 (values #false
                                    (tuple cont todo (cons por-state done)))))
                           (else
                              ;; treat all other reasons and syscalls as errors
                              (print "bad syscall op within par: " op)
                              (values null (tuple op a b c))))))))))

      (define (thread-controller self todo done state)
         (if (eq? todo null)
            (if (null? done)
               (halt-thread-controller state)  ;; nothing left to run
               (self self done null state))    ;; new scheduler round
            (lets 
               ((this todo todo)
                (id st this))
               (if (eq? (type st) type-tuple)
                  ;; parallel or node, hunt next slice to run and proceed
                  (lets ((fini stp (step-parallel st)))
                     (cond
                        ((not fini)
                           ;; no result found yet but options remaining - keep on truckin
                           (self self todo (cons (tuple id stp) done) state))
                        ((eq? fini #true)
                           ;; some result was found and stp is a thunk to proceed computation
                           (self self todo (cons (tuple id stp) done) state))
                        (else
                           ;; TODO: something failed and stp is an error code. time to crash.
                           (print "GONDOR!")
                           "GONDOR!")))
                  (lets ((op a b c (run st thread-quantum)))
                     (if (eq? op 1)
                        (self self todo (cons (tuple id a) done) state)
                        ((ref mcp-syscalls op) id a b c todo done state self)))))))

      (define (start-thread-controller threads state-alist)
         (thread-controller thread-controller threads
            null (list->ff state-alist)))

      ;; signal handler which kills the 'repl-eval thread if there, or repl 
      ;; if not, meaning we are just at toplevel minding our own business.
      (define (repl-signal-handler threads state controller)
         (if (first (λ (x) (eq? (ref x 1) 'repl-eval)) threads #false)
            ;; there is a thread evaling user input, linkely gone awry somehow
            (drop-thread 'repl-eval threads null state eval-break-message controller)
            ;; nothing evaling atm, exit owl
            (signal-halt threads state controller)))
))
