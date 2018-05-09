
(import (prefix (owl sys) sys-))

(define exit-values '(42 43 44))
(define (wait-pid pid) (print "child exited with " (sys-wait pid)))

(print "forking children")

(define pids
   (map
      (λ (exit-val)
         (let ((x (sys-fork)))
            (cond
               ((not x)
                  (print "FORK FAILED")
                  #false)
               ((eq? x #true)
                  ;; exit with given value from child processes
                  (halt exit-val))
               (else
                  ;; return pid to parent
                  x))))
      exit-values))

(print "forked child processes")

(for-each wait-pid pids)

(print "starting sub-process")

(define pipefd (sys-pipe))

(if pipefd
   (case (sys-fork)
      ((#false)
         (print "fork FAILED"))
      ((#true)
         ;; child: close read end
         (close-port (car pipefd))
         (sys-dupfd (cdr pipefd) stdout #true)
         (close-port (cdr pipefd))
         (for-each
            (λ (path)
               (if (m/^\// path)
                  (sys-exec (string-append path "/echo") '("echo" "hello"))))
            (c/:/ (sys-getenv "PATH")))
         (halt 45))
      (else => (λ (pid)
         ;; parent: close write end
         (close-port (cdr pipefd))
         (print (car ((lines (car pipefd)))) " from sub-process")
         (wait-pid pid))))
   (print "pipe creation failed"))

(print "done")
