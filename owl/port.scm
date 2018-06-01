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
      (only (owl list) memq)
      (only (owl primop) create-type))

   (begin

      (define (port? x) (eq? (type x) type-port))
      (define fd->port (H fxbxor (create-type type-port)))
      (define port->fd (H fxbxor 0))

      (define stdin (fd->port 0))
      (define stdout (fd->port 1))
      (define stderr (fd->port 2))

      (define stdio-port? (C memq (list stdin stdout stderr)))
))
