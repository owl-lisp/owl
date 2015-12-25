(define-library (owl html)

   (export
      html
      ; html->sexp
      )

   (import
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


      (define (gen-attrs pairs tail)
         (if (null? pairs)
            tail
            (let ((fst (car pairs)))
               ;; no quoting yet
               (cons #\space 
                  (render (car fst)
                     (list* #\= #\"
                        (render (cadr fst)
                           (cons #\" (gen-attrs (cdr pairs) tail)))))))))

      (define open-only?
         (lets
            ((keys '(p img))
             (ff (list->ff (zip cons keys keys))))
            (λ (x) (get ff x #false))))

      (define closed-tag?
         (lets 
            ((keys '(a b i div html head title h1 h2 h3 h4 h5 table tr td iframe ul ol li))
             (ff (list->ff (zip cons keys keys))))
            (λ (x) (get ff x #false))))

      (define (generate ret)

         (define (maybe-gen-attrs lst tail)
            (cond
               ((null? lst) tail)
               ((and (pair? (car lst)) (pair? (caar lst)))
                  (gen-attrs (car lst)
                     (cons #\>
                        (foldr gen tail (cdr lst)))))
               (else
                  (cons #\> 
                     (foldr gen tail lst)))))

         (define (gen exp tail)
            (cond
               ((null? exp)
                  tail)
               ((not exp)
                  tail)
               ((pair? exp)
                  (let ((hd (car exp)))
                     (cond
                        ((open-only? hd)
                           (cons #\< 
                              (render hd
                                 (maybe-gen-attrs (cdr exp) tail))))
                        ((closed-tag? hd)
                           (cons #\< 
                              (render hd 
                                 (maybe-gen-attrs (cdr exp)
                                    (list* #\< #\/ 
                                       (render hd (cons #\> tail)))))))
                        (else
                           (ret #false)))))
               (else
                  (render exp tail))))

         gen)

      (define (html->x exp x)
         (call/cc 
            (lambda (ret)
               (let ((bs ((generate ret) exp null)))
                  (if bs (x bs) bs)))))

      (define (html exp)
         (html->x exp runes->string))
))

(import (owl html))

(print
   (html
      '(div
         (ul
            (li (a ((href "http://lol") (target "_top")) "trololo"))
            (li (p "foo" (b "bar") "baz"))
            (li (p "BAR"))))))


;; how about attributes?
;; (a (foo . bar) ...)?
;; (a (href . "foo") ..)

      
