(define-library (owl port)
   (export
      port?
      fd->port
      port->fd
      stdin
      stdout
      stderr
      stdio-port?)

   (import
      (owl defmac)
      (only (owl list) memq))

   (begin

      (define (port? x) (eq? (type x) type-port))
      (define fd->port (C cast type-port))
      (define port->fd (C cast type-fix+))

      (define stdin (fd->port 0))
      (define stdout (fd->port 1))
      (define stderr (fd->port 2))

      (define stdio-port? (C memq (list stdin stdout stderr)))
))
