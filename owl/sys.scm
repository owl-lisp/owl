;;; Owl sys library exports various operating system calls and helper 
;;; functions for using them.

(define-library (owl sys)
   (export
      dir-fold
      dir->list
      dir->list-all
      exec
      fork
      pipe
      wait
      waitpid
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
      stat
      chmod
      chown
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
      set-terminal-rawness
      mem-string      ;; pointer to null terminated string → raw string
      mem-strings     ;; **string → (raw-string ...)
      ;peek-word       ;; these are mainly for internal (owl sys) use
      ;peek-byte       ;;
      get-environment
      )

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
               ;; strings should generally be null-terminated
               (c-string x))
            (else x)))

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
      
      (define (n-byte-machine)
         (sys 8))
      
      (define (peek-byte ptr)
         (sys 41 ptr 1))
      
      (define (peek-word ptr)
         (sys 41 ptr (n-byte-machine)))
            
      (define (mem-string-bytes ptr)
         (let ((this (peek-byte ptr)))
            (if (eq? this 0)
               null
               (cons this (mem-string-bytes (+ ptr 1))))))
       
      (define (raw-string x)
         (raw x type-string))
      
      (define (mem-string ptr)
         (if (eq? ptr 0)
            #false
            (raw-string
               (mem-string-bytes ptr))))
       
      (define (mem-array-map ptr func)
         (if (eq? ptr 0)
            #false
            (let ((nb (n-byte-machine)))
               (let loop ((ptr ptr))
                  (let ((next (peek-word ptr)))
                     (if (eq? next 0)
                        null
                        (cons 
                           (func next)
                           (loop (+ ptr nb)))))))))
      
      (define (mem-strings ptr)
         (mem-array-map ptr mem-string))
      
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

      ;; unsafe-dirfd → #false | eof | raw-string
      (define (read-dir obj)
         (if (number? obj)
            (let ((ptrp (sys 12 obj)))
               (if (number? ptrp)
                  (mem-string ptrp)
                  ptrp))
            #false))

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

      ;; list->tuple + internal conversion might also be worth doing in sys-arg instead
      (define (exec path args)
         (lets ((args (map c-string args)))
            (if (all (λ (x) x) args)
               (sys 17 path args)
               #false)))

      ;; → #false on failure, else '(read-fd . write-fd)
      (define (pipe)
         (let ((fdpair (sys 31)))
            (if fdpair
               (cons (fd->port (car fdpair))
                     (fd->port (cdr fdpair)))
               #false)))

      ;; → #false = fork failed, #true = ok, we're in child, n = ok, child pid is n
      (define (fork)
         (sys 18))

      ;; warning, easily collides with owl wait
      (define (waitpid pid)
         (let ((res (sys 19 pid (cons #false #false))))
            (cond
               ((not res) res)
               ((eq? res #true)
                  (interact 'iomux (tuple 'alarm 100))
                  (waitpid pid))
               (else
                  ;; pair of (<exittype> . <result>)
                  res))))

      (define wait waitpid)

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

      (define (stat arg follow)
         (zip cons
            '(dev ino mode nlink uid gid rdev size atim mtim ctim blksize blocks)
            (sys 38 arg follow)))

      (define (chmod arg mode follow)
         (sys 39 arg mode follow))

      (define (chown arg uid gid follow)
         (sys 40 arg (cons uid gid) follow))

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
      
      (define (getenv str)
         (let ((ptr (sys 16 str)))
            (if (eq? ptr 0)
               #false
               (mem-string ptr))))

      (define (setenv var val)
         (sys 28 var val))

      (define (unsetenv var)
         (setenv var #false))
      
      (define (get-environment-pointer)
         (sys 42))
       
      (define (split-env-value bytes)
         (let loop ((l null) (r bytes))
            (cond
               ((null? r)
                  (values (reverse l) null))
               ((eq? (car r) #\=)
                  (values (reverse l) (cdr r)))
               (else
                  (loop (cons (car r) l) (cdr r))))))

      ;; ((keystr . valstr) ...)                  
      (define (get-environment)
         (mem-array-map 
            (get-environment-pointer) 
            (λ (ptr)
               (lets ((k v (split-env-value (mem-string-bytes ptr))))
                  (cons
                     (raw-string k)
                     (raw-string v))))))
      
      ;;;
      ;;; terminal control
      ;;;

      (define (set-terminal-rawness bool)
         (sys 26 bool))
))
