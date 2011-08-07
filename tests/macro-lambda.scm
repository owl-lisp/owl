
(define foo 42)

(define-syntax mac 
   (syntax-rules ()
      ((mac x) (list x (lambda (x) x)))))

(define res (mac foo))

(print (car res))         ; -> 42
(print ((cadr res) 'ok))  ; -> ok

