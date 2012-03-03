(define-library (owl port)
   (export port? port->fd fd->port)

   (import
      (owl defmac))

   (begin

      (define type-port       12)
      (define type-socket     (fxbor type-port 32))
      (define type-tcp-client (fxbor type-port 54))

      (define (port? x)       (eq? (type x) 98))
      (define (fd->port fd)   (cast fd 12))
      (define (port->fd port) (cast port 0))

))
