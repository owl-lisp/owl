(define-library (scheme process-context)

   (import
      (scheme base)
      (only (owl primop) halt)
      (only (owl ff) get)
      (only (owl syscall) error)
      (only (owl sys) getenv get-environment)
      (only (owl variable) link-variable))

   (export
      command-line
      emergency-exit
      exit
      get-environment-variable
      get-environment-variables)

   (begin

      ;; link to app state variable started at repl startup
      (define owl-state
         (link-variable '*state*))

      (define (command-line)
         (get (owl-state) 'command-line-arguments #false))

      (define (exit . x)
         (halt
            (if (pair? x)
               (let ((x (car x))) (if x (if (integer? x) x 0) 1))
               0)))

      (define emergency-exit exit)

      (define get-environment-variable
         getenv)

      (define get-environment-variables
         get-environment)

))
