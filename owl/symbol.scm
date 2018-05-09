;;;
;;; Symbols
;;;

(define-library (owl symbol)

   (export
      string->symbol
      symbol?
      symbol=?
      symbol->string
      render-symbol)

   (import
      (owl defmac)
      (only (owl list) all)
      (owl string)
      (only (owl syscall) error interact))

   (begin

      (define string->symbol
         (H interact 'intern))

      (define (symbol? x)
         (eq? (type x) type-symbol))

      (define (symbol=? x . lst)
         (and (symbol? x) (all (C eq? x) lst)))

      (define (symbol->string x)
         (if (symbol? x)
            (ref x 1)
            (error "Not a symbol: " x)))

      (define (render-symbol sym tl)
         (let ((str (ref sym 1)))
            (cond
               ((string=? str "") (ilist #\| #\| tl))
               ((m/ / str) (cons #\| (render-string str (cons #\| tl))))
               (else (render-string str tl)))))
))
