;; test a few uses of ellipsis

(define-syntax xzip
   (syntax-rules ()
      ((xzip (a ...) (b ...))
         (list (cons a b) ...))))

(print (xzip (1 2 3 4 5) (11 22 33 44 55)))

(define-syntax xunzip 
   (syntax-rules ()
      ((xunzip (a b) ...)
         (list (list a ...) (list b ...)))))

(print (xunzip (1 11) (2 22) (3 33) (4 44) (5 55)))

(define-syntax xwaldo
   (syntax-rules (waldo there-he-is)
      ((xwaldo a ... waldo b ...)
         '(a ... there-he-is b ...))))

(print (xwaldo frank eunice armstrong luke trurl watson ben agatha waldo eunice kip jane))

