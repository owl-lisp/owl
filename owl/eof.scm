(define-library (owl eof)

   (export
      eof-object
      eof-object?)

   (import
      (owl defmac))

   (begin

      (define *eof-object*
         (cast 4 13))

      (define (eof-object)
         *eof-object*)

      (define eof-object?
         (H eq? *eof-object*))

))
