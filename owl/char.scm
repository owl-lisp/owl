(define-library (owl char)
   (export char? char->integer integer->char)
   (import
      (owl defmac)
      (owl math))
   (begin
      (define char? number?)
      (define char->integer self)
      (define integer->char self)))

