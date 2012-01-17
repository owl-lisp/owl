;;;
;;; Symbols
;;;


(define-library (owl symbol)

   (export symbol? symbol->string render)

   (import
      (owl defmac)
      (owl string)
      (only (owl syscall) error)
      (only (owl function) render))

   (begin
      (define (symbol? x) (eq? (type x) 38))

      (define (symbol->string x) 
         (if (eq? (type x) 38)
            (let ((str (ref x 1)))
               (cond
                  ((string=? str "")
                     "||") ;; make empty symbols less invisible
                  ((m/ / str) ;; fixme: doesn't quote internal |:s yet
                     (string-append (string-append "|" str) "|"))
                  (else str)))
            (error "Not a symbol: " x)))

      (define render
         (lambda (self obj tl)
            (if (symbol? obj)
               (self self (symbol->string obj) tl)
               (render self obj tl))))))
