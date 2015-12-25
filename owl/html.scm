(define-library (owl html)

   (export
      html
      ; html->sexp
      )

   (import
      (owl defmac)
      (owl base))

   (begin

      (define (gen-tail ls ret tail gen)
         (cond
            ((pair? ls)
               (gen (car ls) ret 
                  (gen-tail (cdr ls) ret tail gen)))
            ((null? ls) 
               tail)
            (else
               (ret #false))))
         
      (define (gen exp ret tail)
         (cond
            ((null? exp)
               tail)
            ((not exp)
               tail)
            ((pair? exp)
               (let ((hd (car exp)))
                  (cond
                     ((eq? hd 'p)
                        (list* #\< #\p #\> 
                           (gen-tail (cdr exp) ret tail gen)))
                     ((eq? hd 'b)
                        (list* #\< #\b #\> 
                           (gen-tail (cdr exp) ret 
                              (list* #\< #\/ #\b #\> tail)
                              gen)))
                     ((eq? hd 'i)
                        (list* #\< #\i #\> 
                           (gen-tail (cdr exp) ret 
                              (list* #\< #\/ #\i #\> tail) gen)))
                     ((eq? hd 'div)
                        (list* #\< #\d #\i #\v #\> 
                           (gen-tail (cdr exp) ret 
                              (list* #\< #\/ #\d #\i #\v #\> tail) gen)))
                     ((eq? hd 'ul)
                        (list* #\< #\u #\l #\> 
                           (gen-tail (cdr exp) ret 
                              (list* #\< #\/ #\u #\l #\> tail) gen)))
                     ((eq? hd 'li)
                        (list* #\< #\l #\i #\> 
                           (gen-tail (cdr exp) ret 
                              (list* #\< #\/ #\l #\i #\> tail) gen)))
                     ((eq? hd 'ol)
                        (list* #\< #\o #\l #\> 
                           (gen-tail (cdr exp) ret 
                              (list* #\< #\/ #\o #\l #\> tail))))
                     (else
                        (ret #false)))))
            (else
               (render exp tail))))

      (define (html->x exp x)
         (call/cc 
            (lambda (ret)
               (let ((bs (gen exp ret null)))
                  (if bs (x bs) bs)))))

      (define (html exp)
         (html->x exp runes->string))
))

(import (owl html))

(print
   (html
      '(div
         (ul
            (li (p "foo" (b "bar") "baz"))
            (li (p "BAR"))))))



      
