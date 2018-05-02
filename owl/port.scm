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
      (define (socket? x)     (eq? (type x) type-tcp-socket))
      (define (tcp? x)        (eq? (type x) type-tcp-client))

      (define fd->port (C cast type-port))
      (define fd->socket (C cast type-tcp-socket))
      (define fd->tcp (C cast type-tcp-client))

      (define port->fd (C cast type-fix+))

))
