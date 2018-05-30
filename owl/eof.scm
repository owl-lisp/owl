(define-library (owl eof)

   (export
      eof-object
      eof-object?)

   (import
      (owl defmac))

   (begin

      (define *eof-object*
         (fxbxor '() 4))

      (define (eof-object)
         *eof-object*)

      (define eof-object?
         (C eq? *eof-object*))
))
