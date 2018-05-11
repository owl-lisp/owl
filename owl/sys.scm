;;; Owl sys library exports various operating system calls and helper 
;;; functions for using them.

(define-library (owl sys)
   (export
      dir-fold
      dir->list
      dir->list-all
      errno
      strerror
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
      stat
      directory?
      file?
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
      O_RDONLY
      O_WRONLY
      O_APPEND
      O_CREAT
      O_TRUNC
      stdin
      stdout
      stderr
      stdio?
      close
      fcntl
      open
      dupfd
      read
      write
      port->non-blocking
      set-terminal-rawness
      mem-string      ;; pointer to null terminated string → raw string
      mem-strings     ;; **string → (raw-string ...)
      ;peek-word       ;; these are mainly for internal (owl sys) use
      ;peek-byte       ;;
      get-environment
      )

   (import
      (owl defmac)
      (owl eof)
      (owl string)
      (owl math)
      (owl equal)
      (owl syscall)
      (owl port)
      (owl list)
      (owl vector))

   (begin

      (define-syntax sc
         (syntax-rules (sys-const)
            ((sc name index) (define name (sys-const index)))))

      (define (sys-const i)
         (lambda () (sys-prim 8 i #f #f)))

      (define stdin  (fd->port 0))
      (define stdout (fd->port 1))
      (define stderr (fd->port 2))
      (define stdio? (C memq (list stdin stdout stderr)))

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
         (sys 8 1))

      (define (peek-byte ptr)
         (sys 41 ptr 1))

      (define (peek-word ptr)
         (sys 41 ptr))

      (define (mem-string-bytes ptr)
         (let ((this (peek-byte ptr)))
            (if (eq? this 0)
               null
               (cons this (mem-string-bytes (+ ptr 1))))))

      (define raw-string
         (C raw type-string))

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

      (define mem-strings
         (C mem-array-map mem-string))

      (sc E2BIG 9)
      (sc EACCES 10)
      (sc EADDRINUSE 11)
      (sc EADDRNOTAVAIL 12)
      (sc EAFNOSUPPORT 13)
      (sc EAGAIN 14)
      (sc EALREADY 15)
      (sc EBADF 16)
      (sc EBADMSG 17)
      (sc EBUSY 18)
      (sc ECANCELED 19)
      (sc ECHILD 20)
      (sc ECONNABORTED 21)
      (sc ECONNREFUSED 22)
      (sc ECONNRESET 23)
      (sc EDEADLK 24)
      (sc EDESTADDRREQ 25)
      (sc EDOM 26)
      (sc EDQUOT 27)
      (sc EEXIST 28)
      (sc EFAULT 29)
      (sc EFBIG 30)
      (sc EHOSTUNREACH 31)
      (sc EIDRM 32)
      (sc EILSEQ 33)
      (sc EINPROGRESS 34)
      (sc EINTR 35)
      (sc EINVAL 36)
      (sc EIO 37)
      (sc EISCONN 38)
      (sc EISDIR 39)
      (sc ELOOP 40)
      (sc EMFILE 41)
      (sc EMLINK 42)
      (sc EMSGSIZE 43)
      (sc EMULTIHOP 44)
      (sc ENAMETOOLONG 45)
      (sc ENETDOWN 46)
      (sc ENETRESET 47)
      (sc ENETUNREACH 48)
      (sc ENFILE 49)
      (sc ENOBUFS 50)
      (sc ENODATA 51)
      (sc ENODEV 52)
      (sc ENOENT 53)
      (sc ENOEXEC 54)
      (sc ENOLCK 55)
      (sc ENOLINK 56)
      (sc ENOMEM 57)
      (sc ENOMSG 58)
      (sc ENOPROTOOPT 59)
      (sc ENOSPC 60)
      (sc ENOSR 61)
      (sc ENOSTR 62)
      (sc ENOSYS 63)
      (sc ENOTCONN 64)
      (sc ENOTDIR 65)
      (sc ENOTEMPTY 66)
      (sc ENOTRECOVERABLE 67)
      (sc ENOTSOCK 68)
      (sc ENOTSUP 69)
      (sc ENOTTY 70)
      (sc ENXIO 71)
      (sc EOPNOTSUPP 72)
      (sc EOVERFLOW 73)
      (sc EOWNERDEAD 74)
      (sc EPERM 75)
      (sc EPIPE 76)
      (sc EPROTO 77)
      (sc EPROTONOSUPPORT 78)
      (sc EPROTOTYPE 79)
      (sc ERANGE 80)
      (sc EROFS 81)
      (sc ESPIPE 82)
      (sc ESRCH 83)
      (sc ESTALE 84)
      (sc ETIME 85)
      (sc ETIMEDOUT 86)
      (sc ETXTBSY 87)
      (sc EWOULDBLOCK 88)
      (sc EXDEV 89)

      (define (errno)
         (sys 9 0))

      (define (strerror errnum)
         (mem-string (sys 14 errnum)))

      (sc O_EXEC 93)
      (sc O_RDONLY 94)
      (sc O_RDWR 95)
      (sc O_SEARCH 96)
      (sc O_WRONLY 97)
      (sc O_APPEND 98)
      (sc O_CLOEXEC 99)
      (sc O_CREAT 100)
      (sc O_DIRECTORY 101)
      (sc O_DSYNC 102)
      (sc O_EXCL 103)
      (sc O_NOCTTY 104)
      (sc O_NOFOLLOW 105)
      (sc O_NONBLOCK 106)
      (sc O_RSYNC 107)
      (sc O_SYNC 108)
      (sc O_TRUNC 109)
      (sc O_TTY_INIT 110)
      (sc O_ACCMODE 111)
      (sc FD_CLOEXEC 112)
      (sc F_DUPFD 113)
      (sc F_DUPFD_CLOEXEC 114)
      (sc F_GETFD 115)
      (sc F_SETFD 116)
      (sc F_GETFL 117)
      (sc F_SETFL 118)
      (sc F_GETOWN 119)
      (sc F_SETOWN 120)
      (sc F_GETLK 121)
      (sc F_SETLK 122)
      (sc F_SETLKW 123)
      (sc F_RDLCK 124)
      (sc F_UNLCK 125)
      (sc F_WRLCK 126)

      (define (close fd)
         (sys 2 fd))

      (define (fcntl port cmd arg)
         (sys 15 port cmd arg))

      (define (toggle-file-status-flag port flag on?)
         (if-lets ((flags (fcntl port (F_GETFL) 0)))
            (or
               (eq? (band flags flag) (if on? flag 0))
               (fcntl port (F_SETFL) (bxor flags flag)))))

      (define (port->non-blocking port)
         (if port
            (toggle-file-status-flag port (O_NONBLOCK) (not (stdio? port))))
         port)

      (define (open path flags mode)
         (port->non-blocking (sys 1 path flags mode)))

      ;; → (fixed ? fd == new-fd : fd >= new-fd) | #false
      (define (dupfd port new-fd fixed?)
         (let ((port
                  (if fixed?
                     (sys 30 port new-fd)
                     (let ((fd (fcntl port (F_DUPFD) new-fd)))
                        (and fd (fd->port fd))))))
            (if (stdio? port)
               (toggle-file-status-flag port (O_NONBLOCK) #f))
            port))

      (define (read port len)
         (or
            (sys 5 port len)
            (let ((err (errno)))
               (or (eq? (EAGAIN) err) (eq? (EWOULDBLOCK) err)))))

      (define (write port data len)
         (or
            (sys 0 port data len)
            (and
               (let ((err (errno)))
                  (or (eq? (EAGAIN) err) (eq? (EWOULDBLOCK) err)))
               0)))

      ;;;
      ;;; Unsafe operations not to be exported
      ;;;

      ;; string → #false | unsafe-dirptr
      (define (open-dir path)
         (sys 11 path))

      ;; unsafe-dirfd → #false | eof | raw-string
      (define (read-dir obj)
         (and
            (integer? obj)
            (or
               (mem-string (sys 12 obj))
               (and (zero? (errno)) (eof-object)))))

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
                     (if (eof-object? val)
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
            (and (every self args) (sys 17 path args))))

      ;; → #false on failure, else '(read-port . write-port)
      (define (pipe)
         (let ((ports (sys 31)))
            (if (pair? ports)
               (begin
                  (port->non-blocking (car ports))
                  (port->non-blocking (cdr ports))))
            ports))

      ;; → #false = fork failed, #true = ok, we're in child, n = ok, child pid is n
      (define (fork)
         (let ((pid (sys 18)))
            (or (eq? pid 0) pid)))

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

      (sc S_IFMT 0)
      (sc S_IFBLK 2)
      (sc S_IFCHR 3)
      (sc S_IFIFO 4)
      (sc S_IFREG 5)
      (sc S_IFDIR 6)
      (sc S_IFLNK 7)
      (sc S_IFSOCK 8)

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
         (mknod path (S_IFDIR) mode 0))

      (define (mkfifo path mode)
         (mknod path 0 mode 0))

      (define (stat arg follow)
         (zip cons
            '(dev ino mode nlink uid gid rdev size atim mtim ctim blksize blocks)
            (sys 38 arg follow)))

      (define (file-type? path type)
         (let ((mode (assq 'mode (stat path #t))))
            (and mode (= (band (S_IFMT) (cdr mode)) type))))

      (define (directory? path)
         (file-type? path (S_IFDIR)))

      (define (file? path)
         (file-type? path (S_IFREG)))

      (define (chmod arg mode follow)
         (sys 39 arg mode follow))

      (define (chown arg uid gid follow)
         (sys 40 arg (cons uid gid) follow))

      (sc SEEK_SET 90)
      (sc SEEK_CUR 91)
      (sc SEEK_END 92)

      (define (lseek port pos whence)
         (sys 25 port pos whence))

      (define (seek-end port)
         (lseek port 0 (SEEK_END)))

      (define (seek-current port)
         (lseek port 0 (SEEK_CUR)))

      (define (seek-set port pos)
         (lseek port pos (SEEK_SET)))

      ;;;
      ;;; Environment variables
      ;;;

      (define (getenv str)
         (mem-string (sys 16 str)))

      (define (setenv var val)
         (sys 28 var val))

      (define (unsetenv var)
         (sys 28 var))

      (define (get-environment-pointer)
         (sys 9 1))

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
