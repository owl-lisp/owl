;;; 
;;; MCP - the special control program which can be invoked with ^c
;;;

; note, heavily connected to the thread controller's internals

,r "owl/mcp-tags.l"
,r "owl/dump.l"
,r "owl/io.l"
,r "owl/queue.l"

(define-module lib-mcp

   (export 
      mcp-repl mcp-halt                      ; signal handlers
      halt-on-break mcp-on-break               ; signal handler setters
      poll-tag buffer-tag link-tag mcp-tag   ; keys
      )

   (import lib-dump dump-fasl) ; for state saving
   (import lib-io start-base-threads)
   (import lib-queue)

   ;;; NOTE: the mcp will execute as the only live thread, so no symbol 
   ;;; interning or function naming (used by printing) will be available.
   ;;; use of them will result in number of active threads dropping to 0
   ;;; since there is no-one to respond.

   ; thread-node = (pos . #(active id state mail-queue|False))
   ;             = (pos . #(passive id cont-fn))
   ;             = (pos . #(drop _))

   (define stoppeth-mcp
      "'--------------------------------------8<--------------------------------------")

   (define begineth-mcp "
.--------------------------------------8<--------------------------------------
| MASTER CONTROL PROGRAM")

   (define (show-polls polls)
      (print "| o polls: ")
      (for-each
         (λ node
            (lets ((fd dir data timeout node))
               (cond
                  ((not fd)
                     (print* (list "|   - " data " is having a nap")))
                  (else
                     (print* (list "|   - data going to fd " fd))))))
         polls))

   ; default signal handler, allow some control over threads (later)

   (define (mcp-halt threads state)
      (print "mcp: halting on break")
      1)

   (define (verbose-mbox mbox)
      (if mbox
         (let ((n (qlen mbox)))
            (cond
               ((= n 0) (list "no mail"))
               ((= n 1) (list "1 mail waiting"))
               (else (list n " mails waiting"))))
         ""))

   (define (mcp-show-threads threads)
      (show "| VM THREADS: " (length threads))
      (for-each
         (λ p 
            (lets ((idx node p))
               (tuple-case node
                  ((active id state mailq)
                     (print* (list "|  [" idx "] " id " " (verbose-mbox mailq))))
                  ((passive id cont) ;; waiting for mail
                     (print* (list "|  [" idx "] " id " (waiting)")))
                  ((drop type)
                     (print* (list "|  [" idx "] will be dropped")))
                  (else
                     (print* (list "|  [" idx "] FUNNY THREAD NODE " node))))))
         threads))

   ;   thread = (pos . #(active  #(id thread-state) state-value))
   ;         = (pos . #(passive id state-value))
   ;         = (pos . #(drop <active|passive>))

   ; return threads to proper format and restore state with syscall 11

   (define (bye msg)
      (lambda () 
         (show "You were killed by a MCP. " msg)
         ; make sure the thread goes boom loudly instead of looking 
         ; like it just successfully retired.
         (car 'so-you-whizzed-on-the-electric-fence)))

   (define (exit-mcp threads state maybe-cont)
      (let loop ((threads (map cdr threads)) (vm-threads null) (state state))
         (if (null? threads)
            (begin
               (if (not maybe-cont) ;; do not show MCP exit when resuming the session
                  (begin
                     (print stoppeth-mcp)
                     (! 300))) ;; fixme: hack to wait for io flushing
               (if maybe-cont
                  (maybe-cont vm-threads null state) ; start running the given controller 
                  (syscall 11 vm-threads state))) ; resume existing mcp with these threads
            (tuple-case (car threads)
               ((active id thread-state value)
                  (loop (cdr threads) 
                     (cons (tuple id thread-state) vm-threads)
                     (if value (put state (ref thread-state 1) value) state)))
               ((passive id value)
                  (loop (cdr threads) vm-threads (put state id value)))
               ((drop thing)
                  ;(system-println "sys: will drop a thread")
                  (tuple-case thing
                     ((active id th-state value)   ; state is #(id func|dump)
                        ;(system-println "sys: dropping active")
                        (show "| DROP ACTIVE ID " id)
                        (loop (cdr threads) (cons (tuple id (bye "You were running.")) vm-threads) state))
                     ((passive id value)
                        ;(system-println "sys: dropping passive")
                        (show "| DROP PASSIVE ID " id)
                        (loop (cdr threads)
                           (cons (tuple id (bye "You were not running.")) vm-threads)
                           state))
                     (else
                        (show "mcp: i do not know this thread state: " thing)
                        (loop (cdr threads) vm-threads state))))
               (else
                  (show "Unknown thread node in exit-mcp: " (car threads))
                  (loop (cdr threads) vm-threads state))))))

   (define (mcp-cook bs)
      (let ((str (bytes->string bs)))
         (or (string->integer str) str)))

   (define whitespaces '(9 10 13 32)) ; could be in raw mode, so include 13

   (define (mcp-split bvec)
      (str-foldr
         (lambda (rune done)
            (cond 
               ((has? whitespaces rune)
                  (if (null? (car done))
                     done
                     (cons null done)))
               (else
                  (cons (cons rune (car done)) (cdr done)))))
         '(())
         (bytes->string (vec->list bvec))))
   
   (define (input)
      (mail stdout '(124 32 62 32))
      (flush-port stdout)
      (let ((in (interact stdin 'input)))
         (cond
            ((eof? in) (print "") (list "halt"))
            ((not in)  (list "halt"))
            (else (map mcp-cook (mcp-split in))))))

   (define (print-links links)
      (ff-fold
         (λ (foo id subs)
            (print* (list "|   " id " -> " subs)))
         42 links))

   (define mcp-help
"| COMMANDS: 
|   [c]ontinue    - back to lisp
|   [t]hreads     - show currently running threads
|   [m]isc        - show passive threads and other stuff in state
|   [d]rop <n>    - kill a thread
|   [h]elp        - show this thing
|   [s]ave <path> - save a resumable program state to <path>
|   halt <n>      - exit program with return value n (if given)")

   (define (mcp-show-state state)
      (print "| STATE:")
      (ff-fold
         (λ (st id val)
            (cond
               ((eq? id poll-tag)
                  (show-polls val))
               ((eq? id buffer-tag)
                  (if val
                     (begin
                        (print "| o buffers")
                        (ff-fold
                           (λ (foo fd buff)
                              (lets ((n lst buff))
                                 (print* (list "|    - " n " bytes to " fd))))
                           42 val))
                     (print "| o no output buffers")))
               ((eq? id link-tag) 
                  (print "| o links")
                  (print-links val))
               ((eq? id mcp-tag) (print "| o signal handler"))
               (else
                  (tuple-case val ;; fixme: deprecated
                     ((ready rst)
                        (print* (list "| + " id " is ready")))
                     ((working in out)
                        (print* (append (list "| + " id " is working ") (verbose-mbox qnull))))
                     (else
                        (print* (list "| + " id " is in RATHER BAD SHAPE")))))))
         42 state))

   (define (maybe-drop l n)
      (cond
         ((null? l)
            (print "| NO SUCH THREAD")
            null)
         ((= n (caar l))
            (tuple-case (car l)
               ((drop state)
                  (print "| ALREADY DROPPED")
                  l)
               (else
                  (print "| OK")
                  (cons (cons n (tuple 'drop (cdar l))) (cdr l)))))
         (else
            (cons (car l)
               (maybe-drop (cdr l) n)))))

   (define (mcp threads state cont)
      (let ((in (input)))
         (cond
            ((equal? (car in) "halt")
               (print "| BYE ")
               (if (and (pair? (cdr in)) (number? (cadr in)))
                  ; fixme, no exit value passing yet
                  (halt (cadr in))
                  (halt 0)))
            ((mem equal? '("c" "continue" "resume" "proceed") (car in))
               (print "| END OF LINE")
               (exit-mcp threads state False))
            ((mem equal? '("t" "threads") (car in))
               (mcp-show-threads threads)
               (mcp threads state cont))
            ((mem equal? '("m" "misc") (car in))
               (mcp-show-state state)
               (mcp threads state cont))
            ((and (mem equal? '("s" "save") (car in)) (= (length in) 2))
               (print
                  (if (dump-fasl (λ (args) (exit-mcp threads state cont)) (cadr in))
                     "| STATE SAVED"
                     "| COMPLETE FAILURE"))
               (mcp threads state cont))
            ((mem equal? '("d" "drop") (car in))
               (if (and (pair? (cdr in)) (number? (cadr in)))
                  (mcp
                     (maybe-drop threads (cadr in))
                     state cont)
                  (begin
                     (print "| DROP WHAT?")
                     (mcp threads state cont))))
            ((mem equal? '("help" "h" "?" "wtf" ":q") (car in))
               (print mcp-help)
               (mcp threads state cont))
            (else
               (print* (cons "| WHY DO YOU THINK " (append in '("?"))))
               (mcp threads state cont)))))



   (define (grab-running-threads state threads) ; -> state' x (#(active state maybe-mailbox) ...)
      (let loop ((state state) (threads threads) (out null))
         (if (null? threads)
            (values state out)
            (loop 
               (del state (ref (car threads) 1))
               (cdr threads)
               (lets ((thread (car threads)) (id thread-state thread))
                  (cons (tuple 'active id thread-state (get state id False)) out))))))

   (define (grab-passive-threads state)
      (lets
         ; special non-thread things in the state
         ((specials (list poll-tag buffer-tag link-tag mcp-tag))
          (new-state (fold (lambda (new key) (put new key (get state key False))) False specials))
          (state (fold (lambda (state key) (del state key)) state specials)))
         (values
            new-state
            (ff-fold 
               (lambda (out id state)
                  (cons (tuple 'passive id state) out))
               null state))))

   (define (index l n)
      (if (null? l)
         null
         (cons (cons n (car l)) (index (cdr l) (+ n 1)))))

   ; temporary solution until the meta thread is removed
   (define (toy-meta)
      (fork-server 'meta
         (lambda ()
            (let loop ()
               (lets ((env (wait-mail)) (from msg env))
                  (mail from 'func)
                  (loop))))))

   ; get threads to a list and go to mcp repl
   (define (mcp-repl threads state cont)
      (start-base-threads) ;; start basic stdio for the mcp
      (print begineth-mcp)
      (toy-meta)           ;; start a toy db, which is (for now) needed by render to get names for functions
      (lets
         ((state active (grab-running-threads state threads))
          (state passive (grab-passive-threads state))
          (threads (index (append active passive) 1)))
         (mcp threads state cont)))

   (define (halt-on-break) (set-signal-action mcp-halt))
   (define (mcp-on-break)  (set-signal-action mcp-repl))

)

