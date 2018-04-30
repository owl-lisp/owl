(define-library (owl eof)

   (export
      eof-object
      eof-object?)

   (import
      (owl defmac))

   (begin

      (define eof-object
         (let ((eof (cast 4 13)))
            (λ () eof)))

      (define (eof-object? obj)
         (eq? (eof-object) obj))

))
