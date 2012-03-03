(define-library (owl port)
   (export 
      port?
      socket? 
      tcp?

      fd->port
      fd->socket
      fd->tcp

      port->fd)  ;; port | socket | tcp → fd

   (import
      (owl defmac))

   (begin

      (define type-port       12)
      (define type-socket     (fxbor type-port 32))
      (define type-tcp-client (fxbor type-port 54))

      (define (port? x)       (eq? (type x) 98))
      (define (socket? x)     (eq? (type x) 354))
      (define (tcp? x)        (eq? (type x) 498))

      (define (fd->port fd)   (cast fd type-port))
      (define (fd->socket fd) (cast fd type-socket))
      (define (fd->tcp fd)    (cast fd type-tcp-client))

      (define (port->fd port) (cast port 0))

))
