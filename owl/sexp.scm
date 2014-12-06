;; todo: remove implicit UTF-8 conversion from parser and move to a separate pass

(define-library (owl sexp)
   
   (export 
      sexp-parser 
      read-exps-from
      list->number
      get-sexps       ;; greedy* get-sexp
		string->sexp
      vector->sexps
      list->sexps)

   (import
      (owl parse)
      (owl defmac)
      (owl math)
      (owl string)
      (owl list)
      (owl math-extra)
      (owl vector)
      (owl eof)
      (owl list-extra)
      (owl ff)
      (owl lazy)
      (owl io) ; testing
      (owl primop)
      (owl unicode)
      (only (owl syscall) error)
      (only (owl intern) intern-symbols string->uninterned-symbol)
      (only (owl regex) get-sexp-regex))

   (begin

      (define (between? lo x hi)
         (and (<= lo x) (<= x hi)))

      (define special-symbol-chars (string->bytes "+-=<>!*%?_/"))

      (define (symbol-lead-char? n) 
         (or 
            (between? #\a n #\z)
            (between? #\A n #\Z)
            (has? special-symbol-chars n)
            (> n 127)))         ;; allow high code points in symbols

      (define (symbol-char? n) 
         (or (symbol-lead-char? n) 
            (or
               (between? #\0 n #\9)
               (> n 127))))         ;; allow high code points in symbols

      (define get-symbol 
         (get-either
            (let-parses 
               ((head (get-rune-if symbol-lead-char?))
                (tail (get-greedy* (get-rune-if symbol-char?)))
                (next (peek get-byte))
                (foo (assert (lambda (b) (not (symbol-char? b))) next))) ; avoid useless backtracking
               (string->uninterned-symbol (runes->string (cons head tail))))
            (let-parses
               ((skip (get-imm #\|))
                (chars (get-greedy+ (get-rune-if (λ (x) (not (eq? x #\|))))))
                (skip (get-imm #\|)))
               (string->uninterned-symbol (runes->string chars)))))

      (define (digit-char? x) 
         (or (between? 48 x 57)
            (between? 65 x 70)
            (between? 97 x 102)))

      (define digit-values 
         (list->ff
            (foldr append null
               (list
                  (map (lambda (d) (cons d (- d 48))) (iota 48 1 58))  ;; 0-9
                  (map (lambda (d) (cons d (- d 55))) (iota 65 1 71))  ;; A-F
                  (map (lambda (d) (cons d (- d 87))) (iota 97 1 103)) ;; a-f
                  ))))

      (define (digit-char? base)
         (if (eq? base 10)
            (λ (n) (between? 48 n 57))
            (λ (n) (< (get digit-values n 100) base))))

      (define (bytes->number digits base)
         (fold
            (λ (n digit)
               (let ((d (get digit-values digit #false)))
                  (cond
                     ((or (not d) (>= d base))
                        (error "bad digit " digit))
                     (else
                        (+ (* n base) d)))))
            0 digits))

      (define get-sign
         (get-any-of (get-imm 43) (get-imm 45) (get-epsilon 43)))
        
      (define bases
         (list->ff
            (list
               (cons #\b  2)
               (cons #\o  8)
               (cons #\d 10)
               (cons #\x 16))))

      ; fixme, # and cooked later
      (define get-base 
         (get-any-of
            (let-parses
               ((skip (get-imm #\#))
                (char (get-byte-if (λ (x) (getf bases x)))))
               (getf bases char))
            (get-epsilon 10)))

      (define (get-natural base)
         (let-parses
            ((digits (get-greedy+ (get-byte-if (digit-char? base)))))
            (bytes->number digits base)))

      (define (get-integer base)
         (let-parses
            ((sign-char get-sign) ; + / -, default +
             (n (get-natural base)))
            (if (eq? sign-char 43) n (- 0 n))))

      ;; → n, to multiply with
      (define (get-exponent base)
         (get-either
            (let-parses
               ((skip (get-imm 101)) ; e
                (pow (get-integer base)))
               (expt base pow))
            (get-epsilon 1)))

      (define get-signer
         (let-parses ((char get-sign))
            (if (eq? char 43)
               (λ (x) x)
               (λ (x) (- 0 x)))))


      ;; separate parser with explicitly given base for string->number
      (define (get-number-in-base base)
         (let-parses
            ((sign get-signer) ;; default + <- could allow also an optional base here
             (num (get-natural base))
             (tail ;; optional after dot part be added
               (get-either
                  (let-parses
                     ((skip (get-imm 46))
                      (digits (get-greedy* (get-byte-if (digit-char? base)))))
                     (/ (bytes->number digits base)
                        (expt base (length digits))))
                  (get-epsilon 0)))
             (pow (get-exponent base)))
            (sign (* (+ num tail) pow))))

      ;; a sub-rational (other than as decimal notation) number
      (define get-number-unit
         (let-parses
            ((base get-base) ;; default 10
             (val (get-number-in-base base)))
            val))

      ;; anything up to a rational
      (define get-rational
         (let-parses
            ((n get-number-unit)
             (m (get-either
                  (let-parses
                     ((skip (get-imm 47))
                      (m get-number-unit)
                      (verify (not (eq? 0 m)) "zero denominator"))
                     m)
                  (get-epsilon 1))))
            (/ n m)))

      (define get-imaginary-part
         (let-parses
            ((sign (get-either (get-imm #\+) (get-imm #\-)))
             (imag (get-either get-rational (get-epsilon 1))) ; we also want 0+i
             (skip (get-imm #\i)))
            (if (eq? sign #\+)
               imag
               (- 0 imag))))

      (define get-number
         (let-parses
            ((real get-rational) ;; typically this is it
             (imag (get-either get-imaginary-part (get-epsilon 0))))
            (if (eq? imag 0)
               real
               (complex real imag))))
              
      (define get-rest-of-line
         (let-parses
            ((chars (get-greedy* (get-byte-if (lambda (x) (not (eq? x 10))))))
             (skip (get-imm 10))) ;; <- note that this won't match if line ends to eof
            chars))

      ;; #!<string>\n parses to '(hashbang <string>)
      (define get-hashbang
         (let-parses
            ((hash (get-imm 35))
             (bang (get-imm 33))
             (line get-rest-of-line))
            (list 'quote (list 'hashbang (list->string line)))))
      
      ;; skip everything up to |#
      (define (get-block-comment)
         (get-either
            (let-parses
               ((skip (get-imm #\|))
                (skip (get-imm #\#)))
               'comment)
            (let-parses
               ((skip get-byte)
                (skip (get-block-comment)))
               skip)))

      (define get-a-whitespace
         (get-any-of
            ;get-hashbang   ;; actually probably better to make it a symbol as above
            (get-byte-if (lambda (x) (has? '(9 10 32 13) x)))
            (let-parses
               ((skip (get-imm #\;))
                (skip get-rest-of-line))
               'comment)
            (let-parses
               ((skip (get-imm #\#))
                (skip (get-imm #\|))
                (skip (get-block-comment)))
               'comment)))

      (define maybe-whitespace (get-kleene* get-a-whitespace))
      (define whitespace (get-greedy+ get-a-whitespace))

      (define (get-list-of parser)
         (let-parses
            ((lp (get-imm 40))
             (things
               (get-kleene* parser))
             (skip maybe-whitespace)
             (tail
               (get-either
                  (let-parses ((rp (get-imm 41))) null)
                  (let-parses
                     ((dot (get-imm 46))
                      (fini parser)
                      (skip maybe-whitespace)
                      (skip (get-imm 41)))
                     fini))))
            (if (null? tail)
               things
               (append things tail))))

      (define quoted-values
         (list->ff
            '((#\a . #x0007)
              (#\b . #x0008)
              (#\t . #x0009)
              (#\n . #x000a)
              (#\r . #x000d)
              (#\" . #x0022)
              (#\\ . #x005c))))

      (define get-quoted-string-char 
         (let-parses
            ((skip (get-imm #\\))
             (char 
               (get-either
                  (let-parses
                     ((char (get-byte-if (λ (byte) (getf quoted-values byte)))))
                     (getf quoted-values char))
                  (let-parses
                     ((skip (get-imm #\x))
                      (hexes (get-greedy+ (get-byte-if (digit-char? 16))))
                      (skip (get-imm #\;)))
                     (bytes->number hexes 16)))))
            char))

      (define get-string
         (let-parses
            ((skip (get-imm #\"))
             (chars
               (get-kleene*
                  (get-either
                     get-quoted-string-char
                     (get-rune-if (lambda (x) (not (has? '(#\" #\\) x)))))))
             (skip (get-imm #\")))
            (runes->string chars)))

      (define quotations
         (list->ff '((39 . quote) (44 . unquote) (96 . quasiquote) (splice . unquote-splicing))))

      (define (get-quoted parser)
         (let-parses
            ((type 
               (get-either
                  (let-parses ((_ (get-imm 44)) (_ (get-imm 64))) 'splice) ; ,@
                  (get-byte-if (λ (x) (get quotations x #false)))))
             (value parser))
            (list (get quotations type #false) value)))
     
      (define get-named-char
         (get-any-of
            (get-word "null" 0)
            (get-word "alarm" 7)
            (get-word "backspace" 8)
            (get-word "tab" 9)
            (get-word "newline" 10)
            (get-word "return" 13)
            (get-word "escape" 27)
            (get-word "space" 32)
            (get-word "delete" 127)))

      ;; fixme: add named characters #\newline, ...
      (define get-quoted-char
         (let-parses
            ((skip (get-imm #\#))
             (skip (get-imm #\\))
             (codepoint (get-either get-named-char get-rune)))
            codepoint))

      ;; most of these are to go via type definitions later
      (define get-funny-word
         (get-any-of
            (get-word "..." '...)
            (let-parses
               ((skip (get-imm #\#))
                (val
                  (get-any-of
                     (get-word "true" #true)    ;; get the longer ones first if present
                     (get-word "false" #false)
                     (get-word "empty" #empty)
                     (get-word "t" #true)
                     (get-word "f" #false)
                     (get-word "T" #true)
                     (get-word "F" #false)
                     (get-word "e" #empty)
                     (let-parses
                        ((bang (get-imm #\!))
                         (line get-rest-of-line))
                        (list 'quote (list 'hashbang (list->string line)))))))
               val)))

      (define (get-vector-of parser)
         (let-parses
            ((skip (get-imm #\#))
             (fields (get-list-of parser)))
            (let ((fields (intern-symbols fields)))
               (if (first pair? fields #false)
                  ;; vector may have unquoted stuff, so convert it to a sexp constructing a vector, which the macro handler can deal with
                  (cons '_sharp_vector fields) ; <- quasiquote macro expects to see this in vectors
                  (list->vector fields)))))

      (define (get-sexp)
         (let-parses
            ((skip maybe-whitespace)
             (val
               (get-any-of
                  ;get-hashbang
                  get-number         ;; more than a simple integer
                  get-sexp-regex     ;; must be before symbols, which also may start with /
                  get-symbol
                  get-string
                  get-funny-word
                  (get-list-of (get-sexp))
                  (get-vector-of (get-sexp))
                  (get-quoted (get-sexp))
                  (get-byte-if eof?)
                  get-quoted-char)))
            val))

      (define (ok? x) (eq? (ref x 1) 'ok))
      (define (ok exp env) (tuple 'ok exp env))
      (define (fail reason) (tuple 'fail reason))

      (define sexp-parser 
         (let-parses
            ((sexp (get-sexp))
             (foo maybe-whitespace))
            (intern-symbols sexp)))

      (define get-sexps
         (get-greedy* sexp-parser))

      ;; fixme: new error message info ignored, and this is used for loading causing the associated issue
      (define (read-exps-from data done fail)
         (lets/cc ret  ;; <- not needed if fail is already a cont
            ((data 
               (utf8-decoder data 
                  (λ (self line data) 
                     (ret (fail (list "Bad UTF-8 data on line " line ": " (ltake line 10))))))))
            (sexp-parser data
               (λ (data drop val pos)
                  (cond
                     ((eof? val) (reverse done))
                     ((null? data) (reverse (cons val done))) ;; only for non-files
                     (else (read-exps-from data (cons val done) fail))))
               (λ (pos reason)
                  (if (null? done)
                     (fail "syntax error in first expression")
                     (fail (list 'syntax 'error 'after (car done) 'at pos))))
               0)))

      (define (list->number lst base)
         (try-parse (get-number-in-base base) lst #false #false #false))

      (define (string->sexp str fail)
         (try-parse (get-sexp) (str-iter str) #false #false fail))

      ;; parse all contents of vector to a list of sexps, or fail with 
      ;; fail-val and print error message with further info if errmsg 
      ;; is non-false 

      (define (vector->sexps vec fail errmsg)
         ; try-parse parser data maybe-path maybe-error-msg fail-val
         (let ((lst (vector->list vec)))
            (try-parse get-sexps lst #false errmsg #false)))

      (define (list->sexps lst fail errmsg)
         ; try-parse parser data maybe-path maybe-error-msg fail-val
         (try-parse get-sexps lst #false errmsg #false))
))
