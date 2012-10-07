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

      (define (port? x)       (eq? (type x) type-port))
      (define (socket? x)     (eq? (type x) type-socket))
      (define (tcp? x)        (eq? (type x) type-tcp-client))

      (define (fd->port fd)   (cast fd type-port))
      (define (fd->socket fd) (cast fd type-socket))
      (define (fd->tcp fd)    (cast fd type-tcp-client))

      (define (port->fd port) (cast port type-fix+))

))
