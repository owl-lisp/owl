;;;
;;; owl cfg parsing combinators and macros
;;;

(define-library (owl parse)
      
   (export
      let-parses
      byte
      imm
      seq
      epsilon ε 
      byte-if
      rune
      either
      any
      star
      plus
      greedy-star
      greedy-plus
      byte-between
      parse-head
      backtrack
      try-parse
      word

      ;; old compat names      
      get-imm get-byte get-kleene+ get-kleene* get-epsilon get-byte-between get-either
      get-byte-if get-rune get-rune-if get-greedy* get-greedy+ get-word
     
      ;; old ones
      fd->exp-stream
      file->exp-stream
      )
      
   (import
      (owl defmac)
      (owl lazy)
      (owl list)
      (owl string)
      (owl math)
      (owl unicode)
      (owl io)
      (owl syscall))
     
   (begin 
      
        
      ; (parser l r ok) → (ok l' r' val) | (backtrack l r why)
      ;   ... → l|#f r result|error
      
      (define (backtrack l r reason)
         (if (null? l)
            (values #f r reason)
            (let ((hd (car l)))
               (if (eq? (type hd) type-fix+)
                  (backtrack (cdr l) (cons hd r) reason)
                  (hd (cdr l) r reason)))))
               
      (define eof-error "end of input")
                  
      (define (byte l r ok)
         (cond
            ((null? r) (backtrack l r eof-error))
            ((pair? r) (ok (cons (car r) l) (cdr r) (car r)))
            (else      (byte l (r) ok))))
      
      (define (imm x)
         (λ (l r ok)
            (cond
               ((null? r) 
                  (backtrack l r eof-error))
               ((pair? r) 
                  (if (eq? (car r) x)
                     (ok (cons (car r) l) (cdr r) x)
                     (backtrack l r 'bad-byte)))
               (else 
                  ((imm x) l (r) ok)))))
      
      (define (ε val)
         (λ (l r ok)
            (ok l r val)))
      
      (define epsilon ε)
      
      (define (either a b)
         (λ (l r ok)
            (a (cons (λ (l r why) (b l r ok)) l) r ok)))
               
      (define (seq a b)
         (λ (l r ok)
            (a l r
               (λ (l r av)
                  (b l r 
                     (λ (l r bv)
                        (ok l r (cons av bv))))))))
      
      (define (star-vals a vals)
         (λ (l r ok)
            (a 
               (cons 
                  (λ (l r why) (ok l r (reverse vals))) l)
                r 
                (λ (l r val)
                   ((star-vals a (cons val vals)) l r ok)))))
     
      (define (star a)
         (star-vals a null))

      (define (drop l x)
         (if (eq? (car l) x)
            (cdr l)
            (cons (car l) (drop (cdr l) x))))

      (define (greedy-star-vals a vals)
         (λ (l r ok)
            (let ((bt (λ (l r why) (ok  l r (reverse vals)))))
               (a
                  (cons bt l)
                  r
                  (λ (l r val)
                     ((greedy-star-vals a (cons val vals))
                        (drop l bt) r ok))))))

      (define-syntax let-parses
         (syntax-rules (verify eval)
            ((let-parses 42 l r ok ((val (eval term)) . rest) body)
               (let ((val term))
                  (let-parses 42 l r ok rest body)))
            ((let-parses 42 l r ok ((val parser) . rest) body)
               (parser l r
                  (λ (l r val)
                     (let-parses 42 l r ok rest body))))
            ((let-parses 42 l r ok () body) 
               (ok l r body))
            ((let-parses 42 l r ok ((verify term msg) . rest) body)
               (if term
                  (let-parses 42 l r ok rest body)
                  (backtrack l r msg)))
            ((let-parses ((a . b) ...) body)
               (λ (l r ok)
                  (let-parses 42 l r ok ((a . b) ...) body)))))
      
      (define (greedy-star a)
         (greedy-star-vals a null) )

      (define (greedy-plus a)
         (let-parses
            ((first a)
             (rest (greedy-star a)))
            (cons first rest)))
      
      (define (word s val)
         (let ((bytes (string->bytes s)))
            (λ (l r ok)
               (let loop ((l l) (r r) (left bytes))
                  (cond
                     ((null? left)
                        (ok l r val))
                     ((null? r)
                        (backtrack l r eof-error))
                     ((pair? r)
                        (if (eq? (car r) (car left))
                           (loop (cons (car r) l) (cdr r) (cdr left))
                           (backtrack l r "bad byte")))
                     (else
                        (loop l (r) left)))))))
         
       
      
      (define-syntax any
         (syntax-rules ()
            ((any a) a)
            ((any a b) (either a b))
            ((any a b . c) (either a (any b . c)))))
            
      (define (plus parser)
         (let-parses
            ((a parser)
             (as (star parser)))
            (cons a as)))
      
      (define (byte-if pred)
         (let-parses
            ((a byte)
             (verify (pred a) "checked"))
            a))
               
      ; #b10xxxxxx
      (define extension-byte 
         (let-parses
            ((b byte)
             (verify (eq? #b10000000 (fxband b #b11000000)) "Bad extension byte"))
            b))
      
      (define (byte-between lo hi)
         (byte-if
            (λ (x)
               (and (lesser? lo x)
                    (lesser? x hi)))))
            
      (define rune
         (any
            (byte-if (λ (x) (lesser? x 128)))
            (let-parses
               ((a (byte-between 127 224))
                (verify (not (eq? a #b11000000)) "blank leading 2-byte char") ;; would be non-minimal
                (b extension-byte))
               (two-byte-point a b))
            (let-parses
               ((a (byte-between 223 240))
                (verify (not (eq? a #b11100000)) "blank leading 3-byte char") ;; would be non-minimal
                (b extension-byte) (c extension-byte))
               (three-byte-point a b c))
            (let-parses
               ((a (byte-between 239 280))
                (verify (not (eq? a #b11110000)) "blank leading 4-byte char") ;; would be non-minimal
                (b extension-byte) (c extension-byte) (d extension-byte))
               (four-byte-point a b c d))))
      
     
      (define (rune-if pred)
         (let-parses
            ((val rune)
             (verify (pred val) "bad rune"))
            val))
        
      (define (parser-succ l r v)
         (values l r v))
           
      (define (parse-head parser ll def)
         (lets ((l r val (parser null ll parser-succ)))
            (if l (cons val r) def)))
     
      ; (parser l r ok) → (ok l' r' val) | (backtrack l r why)
      ;   ... → l|#f r result|error
      ;; prompt removed from here - it belongs elsewhere
      (define (fd->exp-stream fd prompt parser fail re-entry?)
         (let loop ((ll (port->byte-stream fd)))
            (lets
               ((lp r val 
                   (parser null ll parser-succ)))
               (cond
                  (lp ;; something parsed successfully
                     (pair val (loop r)))
                  ((null? r) ;; end of input
                     ;; typically there is whitespace, so this does not happen
                     null)
                  (else
                     ;; error handling being converted
                     ;(print-to stderr "fd->exp-stream: syntax error")
                     null)))))
      
      (define (file->exp-stream path prompt parser fail re-entry?)
         ;(print "file->exp-stream: trying to open " path)
         (let ((fd (open-input-file path)))
            ;(print "file->exp-stream: got fd " fd)
            (if fd
               (fd->exp-stream  fd prompt parser fail re-entry?)
               #false)))
      
      (define get-imm imm)
      (define get-byte byte)
      (define get-byte-if byte-if)
      (define get-rune rune)
      (define get-rune-if rune-if)
      (define get-kleene+ plus)
      (define get-kleene* star)
      (define get-greedy+ greedy-plus)
      (define get-greedy* greedy-star)
      (define get-epsilon ε)
      (define get-byte-between byte-between)
      (define get-either either)
      (define get-word word)
       
      (define (try-parse parser data maybe-path maybe-error-msg fail-fn) 
         (lets ((l r val (parser null data parser-succ)))
            (cond
               ((not l)
                  (fail-fn 0 null))
               ((lpair? r) =>
                  (λ (r)
                     (backtrack l r "trailing garbage")))
               (else
                  ;; full match
                  val))))
))
      
