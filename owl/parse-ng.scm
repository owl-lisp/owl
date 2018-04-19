;;;
;;; owl cfg parsing combinators and macros
;;;

(define-library (owl parse-ng)
      
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
      parse-head)
      
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
      (define get-extension-byte 
         (let-parses
            ((b byte)
             (verify (eq? #b10000000 (fxband b #b11000000)) "Bad extension byte"))
            b))
      
      (define (get-between lo hi)
         (let-parses
            ((a byte)
             (verify (lesser? lo a))
             (verify (lesser? a hi)))
            a))
            
      (define rune
         (any
            (byte-if (λ (x) (lesser? x 128)))
            (let-parses
               ((a (get-between 127 224))
                (verify (not (eq? a #b11000000)) "blank leading 2-byte char") ;; would be non-minimal
                (b get-extension-byte))
               (two-byte-point a b))
            (let-parses
               ((a (get-between 223 240))
                (verify (not (eq? a #b11100000)) "blank leading 3-byte char") ;; would be non-minimal
                (b get-extension-byte) (c get-extension-byte))
               (three-byte-point a b c))
            (let-parses
               ((a (get-between 239 280))
                (verify (not (eq? a #b11110000)) "blank leading 4-byte char") ;; would be non-minimal
                (b get-extension-byte) (c get-extension-byte) (d get-extension-byte))
               (four-byte-point a b c d))))
      
       
      (define (parser-succ l r v)
         (values l r v))
           
      (define (parse-head parser ll def)
         (lets ((l r val (parser null ll parser-succ)))
            (if l (cons val r) def)))))
      
