(define-library (scheme write)

   (import
      (scheme base)
      (only (owl io) display write))

   (export
      display
      write
      write-shared
      write-simple)

   (begin

      (define write-shared write)

      (define (write-simple . x)
         (error "implementation restriction: " "write-simple is currently missing"))
))
