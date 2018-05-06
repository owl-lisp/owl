(define-library (owl port)
   (export
      port?
      fd->port
      port->fd)  ;; port | socket | tcp → fd

   (import
      (owl defmac))

   (begin

      (define (port? x) (eq? (type x) type-port))
      (define fd->port (C cast type-port))
      (define port->fd (C cast type-fix+))

))
