(define-library (scheme write)

   (import
      (scheme base)
      (only (owl io) display-to write))

   (export
      display
      write
      write-shared
      write-simple)

   (begin

      (define (display obj . port)
         (display-to
            (if (null? port) (current-output-port) (car port))
            obj))

      (define write-shared write)

      (define (write-simple . x)
         (error "implementation restriction: " "write-simple is currently missing"))
))
