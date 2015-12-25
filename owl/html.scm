(define-library (owl html)

   (export html)

   (import (owl base))

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

      ;; quote \:s and ":s with \. e.g. a""b → a\"\"b → a\\\"\\\"b → ...

      (define (quote-once lst tail)
         (foldr
            (λ (char tail)
               (if (or (eq? char #\") (eq? char #\\))
                  (ilist #\\ char tail)
                  (cons char tail)))
            tail lst))

      (define (render-quoted val tail)
         (cond
            ((string? val)
               (let ((cs (string->list val)))
                  (quote-once cs tail)))
            (else
               (error "render-quoted: cannot handle this yet: " val))))

      (define (gen-attrs pairs tail)
         (if (null? pairs)
            tail
            (let ((fst (car pairs)))
               ;; no quoting yet
               (cons #\space 
                  (render (car fst)
                     (list* #\= #\"
                        (render-quoted (cadr fst)
                           (cons #\" (gen-attrs (cdr pairs) tail)))))))))

      (define open-only?
         (lets
            ((keys '(p img area base basefont br hr col embed input keygen 
               link meta param track wbr))
             (ff (list->ff (zip cons keys keys))))
            (λ (x) (get ff x #false))))

      (define closed-tag?
         (lets 
            ((keys '(a abbr acronym address applet article aside audio b
               bdi bdo big blockquote body i div html head header title h1
               h2 h3 h4 h5 h6 table tbody tr textarea tfoot th thead time
               title td iframe ul ol li button canvas caption center cite
               code colgroup datalist dl dd del details dfn dialog dir dt em
               fieldset figcaption figure font footer form frame frameset
               ins kbd label legend main map mark menu menuitem meter nav
               noframes noscript object optgroup select output prep progress
               q rp rt ruby s samp script section select small source span
               strike strong style sub summary sup tt u ul var video))
             (ff (list->ff (zip cons keys keys))))
            (λ (x) (get ff x #false))))

      (define (render-quoting-tags str tail)
         (str-foldr
            (λ (c tail)
               (cond
                  ((eq? c #\<) (ilist #\& #\l #\t #\; tail))
                  ((eq? c #\>) (ilist #\& #\g #\t #\; tail))
                  (else (cons c tail))))
            tail str))

      (define (generate ret)

         (define (maybe-gen-attrs lst tail)
            (cond
               ((null? lst) tail)
               ((and (pair? (car lst)) (pair? (caar lst)))
                  (gen-attrs (car lst)
                     (cons #\> (foldr gen tail (cdr lst)))))
               (else
                  (cons #\> 
                     (foldr gen tail lst)))))

         (define (gen exp tail)
            (cond
               ((null? exp) tail)
               ((not exp) tail)
               ((pair? exp)
                  (let ((hd (car exp)))
                     (cons #\<
                        (cond
                           ((open-only? hd)
                              (render hd (maybe-gen-attrs (cdr exp) tail)))
                           ((closed-tag? hd)
                              (render hd 
                                 (maybe-gen-attrs (cdr exp)
                                    (list* #\< #\/ 
                                       (render hd (cons #\> tail))))))
                           (else
                              (ret #false))))))
               ((string? exp)
                  (render-quoting-tags exp tail))
               (else
                  (render exp tail))))

         gen)

      (define (html->x exp x)
         (call/cc 
            (lambda (ret)
               (let 
                  ((bs ((generate ret) exp null)))
                  ;; <!DOCTYPE html> could be added at this point
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
            (li ((style "foo: \"lol\"")) "ke<script>alert(42);</script>kekek")
            (li (p "BAR"))))))

      
