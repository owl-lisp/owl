;;;
;;; Extra IO etc exposed via the sys-prim
;;;

;; Adding some extra system primops to see how much could be added while 
;; still keeping the generated .c code portable, win32 being the main 
;; reason for worry.

(define-library (owl sys)
   (export
      dir-fold
      dir->list
      dir->list-all
      exec
      fork
      pipe
      wait
      kill
      getenv
      setenv
      unsetenv
      umask
      getcwd
      chdir
      readlink
      symlink
      link
      rename
      unlink
      rmdir
      mknod
      mkdir
      mkfifo
      directory?
      file?
      lseek
      seek-current
      seek-set
      seek-end
      sighup
      signint
      sigquit
      sigill
      sigabrt
      sigfpe
      sigkill
      sigsegv
      sigpipe
      sigalrm
      sigterm
      stdin
      stdout
      stderr
      fopen
      fclose
      dupfd
      set-terminal-rawness)

   (import
      (owl defmac)
      (owl string)
      (owl math)
      (owl equal)
      (owl syscall)
      (owl port)
      (owl list)
      (owl vector))

   (begin

      (define stdin  (fd->port 0))
      (define stdout (fd->port 1))
      (define stderr (fd->port 2))

      ;; owl value → value processable in vm (mainly string conversion)
      (define (sys-arg x)
         (cond
            ((string? x)
               (c-string x))
            (else 
               x)))
     
      ;; call fixed arity prim-sys instruction with converted arguments
      (define sys
         (case-lambda 
            ((op)
               (sys-prim op #f #f #f))
            ((op a)
               (sys-prim op (sys-arg a) #f #f))
            ((op a b)
               (sys-prim op (sys-arg a) (sys-arg b) #f))
            ((op a b c)
               (sys-prim op (sys-arg a) (sys-arg b) (sys-arg c)))))
      
      (define (fclose fd)
         (sys 2 fd))

      (define (fopen path mode)
         (sys 1 path mode))

      ;; → (fixed ? fd == new-fd : fd >= new-fd) | #false
      (define (dupfd old-fd new-fd fixed)
         (sys 30 old-fd new-fd fixed))

      ;;;
      ;;; Unsafe operations not to be exported
      ;;;

      ;; string → #false | unsafe-dirptr
      (define (open-dir path)
         (sys 11 path))

      ;; unsafe-dirfd → #false | eof | bvec
      (define (read-dir obj)
         (sys 12 obj))

      ;; _ → #true
      (define (close-dir obj)
         (sys 13 obj))

      ;;;
      ;;; Safe derived operations
      ;;;

      ;; dir elements are #false or fake strings, which have the type of small raw ASCII 
      ;; strings, but may in fact contain anything the OS happens to allow in a file name.

      (define (dir-fold op st path)
         (let ((dfd (open-dir path)))
            (if dfd
               (let loop ((st st))
                  (let ((val (read-dir dfd)))
                     (if (eof? val)
                        (begin
                           (close-dir dfd)
                           st)
                        (loop (op st val)))))
               #false)))

      ;; no dotfiles
      (define (dir->list path)
         (dir-fold
            (λ (seen this)
               (if (eq? #\. (refb this 0))
                  seen
                  (cons this seen)))
            null path))

      ;; everything reported by OS
      (define (dir->list-all path)
         (dir-fold
            (λ (seen this) (cons this seen))
            null path))

      ;;;
      ;;; Processes
      ;;;

      ;; path (arg0 ...), arg0 customarily being path
      ;; returns only if exec fails

      ;; list conversion might also be worth doing in sys-arg instead
      (define (exec path args)
         (lets ((args (map c-string args)))
            (if (all (λ (x) x) args)
               (sys 17 path args)
               #false)))

      ;; → #false on failure, else '(read-fd . write-fd)
      (define (pipe)
         (sys 31))

      ;; → #false = fork failed, #true = ok, we're in child, n = ok, child pid is n
      (define (fork)
         (sys 18))

      (define (wait pid)
         (let ((res (sys 19 pid (cons #false #false))))
            (cond
               ((not res) res)
               ((eq? res #true)
                  (interact 'iomux (tuple 'alarm 100))
                  (wait pid))
               (else
                  ;; pair of (<exittype> . <result>)
                  res))))

      (define sighup   1)      ; hangup from controlling terminal or proces
      (define signint  2)      ; interrupt (keyboard)
      (define sigquit  3)      ; quit (keyboard)
      (define sigill   4)      ; illegal instruction
      (define sigabrt  6)      ; abort(3)
      (define sigfpe   8)      ; floating point exception
      (define sigkill  9)      ; kill signal
      (define sigsegv 11)      ; bad memory access
      (define sigpipe 13)      ; broken pipe
      (define sigalrm 14)      ; alarm(2)
      (define sigterm 15)      ; termination signal

      ;; pid signal → success?
      (define (kill pid signal)
         (sys 21 pid signal))

      ;;;
      ;;; Filesystem operation
      ;;;

      (define (umask mask)
         (sys 37 mask))

      (define (getcwd)
         (sys 36))

      (define (chdir path)
         (sys 20 path))

      (define (readlink path)
         (sys 35 path))

      (define (symlink src dst)
         (sys 34 src dst))

      (define (link src dst)
         (sys 33 src dst))

      (define (rename src dst)
         (sys 32 src dst))

      (define (unlink path)
         (sys 22 path))

      (define (rmdir path)
         (sys 23 path))

      (define (mknod path type mode dev)
         (sys 24 path (cons type mode) dev))

      (define (mkdir path mode)
         (mknod path 4 mode 0))

      (define (mkfifo path mode)
         (mknod path 0 mode 0))

      (define (directory? path)
         (let ((dh (open-dir path)))
            (and dh (begin (close-dir dh) #true))))

      (define (file? path)
         (let ((fd (fopen path 0)))
            (and fd (begin (fclose fd) #true))))

      (define seek/set 0) ;; set position to pos
      (define seek/cur 1) ;; increment position by pos
      (define seek/end 2) ;; set position to file end + pos

      (define (lseek fd pos whence)
         (sys 25 fd pos whence))

      (define (seek-end fd)
         (lseek fd 0 seek/end))

      (define (seek-current fd)
         (lseek fd 0 seek/cur))

      (define (seek-set fd pos)
         (lseek fd pos seek/set))

      ;;;
      ;;; Environment variables
      ;;;

      ;; str → bvec | F
      (define (getenv str)
         (let ((bvec (sys 16 str)))
            (and bvec (bytes->string (vector->list bvec)))))

      (define (setenv var val)
         (sys 28 var val))

      (define (unsetenv var)
         (setenv var #false))

      ;;;
      ;;; terminal control
      ;;;

      (define (set-terminal-rawness bool)
         (sys 26 bool))
))


