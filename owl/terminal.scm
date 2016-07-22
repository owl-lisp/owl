(define-library (owl terminal)

   (import
      (owl defmac)
      (owl math)
      (owl primop)
      (owl list)
      (owl string)
      (owl lazy)
      (owl ff)
      (owl list-extra)
      (scheme base)
      (owl io)
      (only (owl unicode) utf8-decoder utf8-encode)
      (only (owl sexp) string->sexp)
      (owl sys))

   (export
      set-terminal-rawness
      normal-text
      bold-text
      lowint-text
      underlined-text
      reverse-text
      invisible-text
     
      get-terminal-size
      port->readline-sexp-stream)

   (begin
     
      (define (num->bytes n tl)
         (if (eq? (type n) type-fix+)
            (append (string->list (number->string n 10)) tl)
            (cons #\0 tl)))

      ;;; ^[[<n><op>
      (define (unary-op n op)
         (write-bytes stdout
            (ilist 27 #\[ (num->bytes n (list op)))))

      (define (set-terminal-rawness rawp)
         (sys-prim 26 rawp #f #f))

      ;;; Text mode

      (define (normal-text)     (write-byte-vector stdout #(27 #\[     #\m)))
      (define (bold-text)       (write-byte-vector stdout #(27 #\[ #\1 #\m)))
      (define (lowint-text)     (write-byte-vector stdout #(27 #\[ #\2 #\m)))
      (define (underlined-text) (write-byte-vector stdout #(27 #\[ #\4 #\m)))
      (define (blinking-text)   (write-byte-vector stdout #(27 #\[ #\5 #\m)))
      (define (reverse-text)    (write-byte-vector stdout #(27 #\[ #\7 #\m)))
      (define (invisible-text)  (write-byte-vector stdout #(27 #\[ #\8 #\m)))

      ;;; Clearing content

      (define (clear-line)       (write-byte-vector stdout #(27 #\[ #\2 #\K)))
      (define (clear-line-right) (write-byte-vector stdout #(27 #\[ #\K)))
      (define (clear-line-left)  (write-byte-vector stdout #(27 #\[ #\1 #\K)))

      (define (clear-screen) (write-byte-vector stdout #(27 #\[ #\2 #\J)))
      (define (clear-screen-top) (write-byte-vector stdout #(27 #\[ #\1 #\J)))
      (define (clear-screen-bottom) (write-byte-vector stdout #(27 #\[ #\J)))

      ;;; Terminal input stream

      (define (get-natural ll def)
        (let loop ((n 0) (first? #true) (ll ll))
          (lets ((x ll (uncons ll #false)))
            (cond
              ((not x) (values def ll)) 
              ((< 47 x 58)
                (loop (+ (* n 10) (- x 48)) #false ll))
              (first?
                (values def (cons x ll)))
              (else
                (values n (cons x ll)))))))
      
      (define (get-imm ll val)
        (lets ((x ll (uncons ll #false)))
          (if (eq? x val)
            (values x ll)
            (values #false (cons x ll)))))
  
      (define (terminal-input)
        (let loop ((ll (utf8-decoder (port->byte-stream stdin) (λ (loop line ll) (print-to stderr "Bad UTF-8 in terminal input") null))))
          (cond
            ((pair? ll)
              (lets ((hd ll ll))
                (cond
                  ((eq? hd 27) ;; decode escape sequence
                    (lets ((op ll (uncons ll #false)))
                      (cond
                        ((eq? op 91)
                          (lets ((op ll (uncons ll #false)))
                            (cond
                              ((eq? op 65) (cons (tuple 'arrow 'up) (loop ll)))
                              ((eq? op 66) (cons (tuple 'arrow 'down) (loop ll)))
                              ((eq? op 67) (cons (tuple 'arrow 'right) (loop ll)))
                              ((eq? op 68) (cons (tuple 'arrow 'left) (loop ll)))
                              (else 
                                (lets
                                  ((a ll (get-natural (cons op ll) #false)))
                                  (if a
                                    (lets ((x ll (uncons ll #false)))
                                      (cond
                                        ((not x)
                                          null)
                                        ((eq? x #\;)
                                          (lets ((b ll (get-natural ll #false)))
                                            (if b
                                              (lets ((op ll (uncons ll #false)))
                                                (if op
                                                  (cond
                                                    ((eq? op #\R)
                                                      (cons (tuple 'cursor-position a b) (loop ll)))
                                                    (else
                                                      (cons 
                                                        (tuple 'esc-unknown-binop a ";" b (list->string (list op)))
                                                        null)))
                                                  null))
                                              null)))
                                        ((and (eq? a 3) (eq? x #\~))
                                          (cons (tuple 'delete) (loop ll)))
                                        (else
                                          (cons (tuple 'esc-unknown-unary-op a (list->string (list x))) ll))))
                                    null))))))
                        (else
                          (pair (tuple 'esc-unknown op) null)))))
                  ((eq? hd 127) (cons (tuple 'backspace) (loop ll)))
                  ((eq? hd 13)  (cons (tuple 'enter) (loop ll)))
                  ((eq? hd 21)  (cons (tuple 'nak) (loop ll))) ;; ^u
                  ((eq? hd 3)  (cons (tuple 'end-of-text) (loop ll))) ;; ^c
                  ((eq? hd 4)  (cons (tuple 'end-of-transmission) (loop ll))) ;; ^d
                  ((eq? hd 16)  (cons (tuple 'data-link-escape) (loop ll))) ;; ^p
                  ((eq? hd 14)  (cons (tuple 'shift-out) (loop ll))) ;; ^n
                  (else
                    (cons (tuple 'key hd) (loop ll))))))
            ((null? ll) ll)
            (else (λ () (loop (ll)))))))

      ;;; Cursor movement

      (define (cursor-pos x y)
         (write-bytes stdout
            (ilist 27 #\[ (num->bytes y (cons #\; (num->bytes x (list #\f)))))))
    
      (define (cursor-up n) 
         (if (eq? n 1)
            (write-byte-vector stdout #(27 #\[ #\A))
            (unary-op n #\A)))

      (define (cursor-down n) 
         (if (eq? n 1)
            (write-byte-vector stdout #(27 #\[ #\B))
            (unary-op n #\B)))

      (define (cursor-right n) 
         (if (eq? n 1)
            (write-byte-vector stdout #(27 #\[ #\C))
            (unary-op n #\C)))

      (define (cursor-left n) 
         (if (eq? n 1)
            (write-byte-vector stdout #(27 #\[ #\D))
            (unary-op n #\D)))

      (define (cursor-top-left n) 
         (write-byte-vector stdout #(27 #\[ #\H)))

      ;; Interaction with terminal

      ;; ^[6n = get cursor position ^[<x>;<y>R
      ;; ^[5n = check terminal status -> ^[0n = ok, ^[3n = not ok
      ;; ^[[c = get terminal type -> 
      ;; input: up    27 91 65
      ;;        down  27 91 66
      ;;        right 27 91 67
      ;;        left  27 91 68
      ;;        enter 13
      ;;        bs    127
      ;;        ^K    11  -- remove line right
      ;;        ^U    21  -- remove line left

      (define (wait-cursor-position ll)
        (let loop ((head null) (ll ll))
          (lets ((this ll (uncons ll #false)))
            (cond
              ((not this)
                (values #false #false ll))
              ((eq? 'cursor-position (ref this 1))
                (values (ref this 3) (ref this 2) (append (reverse head) ll)))
              (else
                (loop (cons this head) ll))))))
                
      ;; ll → cols rows ll'
      (define (get-cursor-position ll)
        ;; request cursor position
        (write-byte-vector stdout #(27 #\[ #\6 #\n))
        (wait-cursor-position ll))

      (define (get-terminal-size ll)
        (lets 
          ((x y ll (get-cursor-position ll))
           (res (cursor-pos 4095 4095))
           (xm ym ll (get-cursor-position ll)))
          (cursor-pos x y)
          (values xm ym ll)))

      (define (get-terminal-byte)
         (car (vector->list (get-block stdin 1))))

      (define (get-esc-input)
         (let ((b (get-terminal-byte)))
            (if (eq? b 91) ;; arrow
               (let ((op (get-terminal-byte)))
                  (cond
                     ((eq? op 65) 'up)
                     ((eq? op 66) 'down)
                     ((eq? op 67) 'right)
                     ((eq? op 68) 'left)
                     (else
                        (print-to stderr (list 'unknown-escape 27 91 op)
                        #false))))
               (begin
                  (print-to stderr (list 'unknown-escape 27 b))
                  #false))))

      ;;; Minimal readline-ish reading support

      (define (read-byte)
         (let ((block (get-block stdin 1)))
            (cond
               ((eof? block) block)
               (block
                  (vector-ref block 0))
               (else block))))
               
      (define (interactive-readline left right)
         (let ((b (read-byte)))
            (cond
               ((eq? b 13) 
                  (append (reverse left) right))
               ((eq? b 127)
                  (if (null? left)
                     (interactive-readline left right)
                     (begin
                        (cursor-left 1)
                        (clear-line-right)
                        (if (pair? right)
                           (begin
                              (display (list->string right))
                              (cursor-left (length right))))
                        (interactive-readline (cdr left) right))))
               ((eq? b 21) ;; ^U
                  (cursor-left (length left))
                  (clear-line-right)
                  (if (pair? right)
                     (begin
                        (display (list->string right))
                        (cursor-left (length right))))
                  (interactive-readline null right))
               ((eq? b 11) ;; ^K
                  (clear-line-right)
                  (interactive-readline left null))
               ((eq? b 27)
                  (let ((op (get-esc-input)))
                     (cond
                        ((eq? op 'left)
                           (if (null? left)
                              (interactive-readline left right)
                              (begin
                                 (cursor-left 1)
                                 (interactive-readline (cdr left) (cons (car left) right)))))
                        ((eq? op 'right)
                           (if (null? right)
                              (interactive-readline left right)
                              (begin
                                 (cursor-right 1)
                                 (interactive-readline (cons (car right) left) (cdr right)))))
                        (else
                           ;(print-to stderr (list 'wat))
                           (interactive-readline left right)))))
               ((eof? b) #false)
               ((not b) #false)
               (else
                  (display (list->string (list b)))
                  (if (pair? right)
                     (begin
                        (display (list->string right))
                        (cursor-left (length right))))
                  (interactive-readline (cons b left) right)))))
    

      ;; show as much of right as fits after cx (cursor x)
      ;; return cursor to cx
      (define (update-line-right right w cx)
        (clear-line-right)
        (if (pair? right)
          (let ((visible-right (list->string (take right (- w cx)))))
            (display visible-right)
            (cursor-left (string-length visible-right)))))
      
      ;; → cx
      (define (update-line-left x y off left)
        (lets
          ((visible-left (list->string (drop (reverse left) off)))
           (cx (+ x (string-length visible-left))))
          (cursor-pos x y)
          (clear-line-right)
          (display visible-left)
          cx))

      ;; upgrade a possible string to readline state at end of it
      (define (history->state elem off)
        (cond
          ((string? elem)
            ;; compute a suitable offset
            (let ((len (string-length elem)))
              (values 
                (reverse (string->list elem)) 
                null 
                (max 0 (* (- (quot len off) 1) off)))))
          (else
            (values (ref elem 1) (ref elem 2) (ref elem 3)))))

      (define (readline ll history)
        (lets 
          ((w h ll (get-terminal-size ll))
           (x y ll (get-cursor-position ll))
           (history (cons null history))  ; (newer . older)
           (offset-delta (+ 1 (div (- w x) 2)))
           (width (- w x)))
          (let loop ((ll ll) (hi history) (left null) (right null) (cx x) (off 0))
            (lets ((op ll (uncons ll #false)))
              (tuple-case op
                ((key k)
                  (let ((left (cons k left))
                        (cx (+ cx 1)))
                    (display (list->string (list k)))
                    (update-line-right right w cx)
                    (if (= cx w)
                      (lets  ;; share
                        ((off (+ off offset-delta))
                         (visible-left (list->string (drop (reverse left) off))))
                        (cursor-pos x y)
                        (clear-line-right)
                        (display visible-left)
                        (update-line-right right w cx)
                        (loop ll hi left right (+ x (string-length visible-left)) off))
                      (loop ll hi left right cx off))))
                ((backspace)
                  (if (= cx x) ;; beginning
                    (if (= off 0) ;; no scroll, do nothing
                      (loop ll hi left right cx off)
                      (lets ;; update, share
                        ((off (- off offset-delta))
                         (visible-left (list->string (drop (reverse left) off)))
                         (cx (+ x (string-length visible-left))))
                        (cursor-pos x y)
                        (clear-line-right)
                        (display visible-left)
                        (update-line-right right w cx)
                        (loop (cons op ll) hi left right cx off)))
                    (let ((cx (- cx 1)))
                      (cursor-left 1)
                      (update-line-right right w cx)
                      (loop ll hi (cdr left) right cx off))))
                ((delete)
                  (if (pair? right)
                    (let ((right (cdr right)))
                      (update-line-right right w cx)
                      (loop ll hi left right cx off))))
                ((arrow dir)
                  (cond
                    ((eq? dir 'left)
                      (if (= cx x) ;; beginning
                        (if (= off 0) ;; no scroll, nop
                          (loop ll hi left right cx off)
                          (lets ;; unscroll + recurse, shared
                            ((off (- off offset-delta))
                             (visible-left (list->string (drop (reverse left) off)))
                             (cx (+ x (string-length visible-left))))
                            (cursor-pos x y)
                            (clear-line-right)
                            (display visible-left)
                            (update-line-right right w cx)
                            (loop (cons op ll) hi left right cx off)))
                        (begin
                          (cursor-left 1)
                          (loop ll hi (cdr left) (cons (car left) right) (- cx 1) off))))
                    ((eq? dir 'right)
                      (cond
                        ((null? right) ;; no way to go
                          (loop ll hi left right cx off))
                        ((= cx w) ;; end, scroll + recurse, share
                          (lets
                            ((off (+ off offset-delta))
                             (visible-left (list->string (drop (reverse left) off)))
                             (cx (+ x (string-length visible-left))))
                            (cursor-pos x y)
                            (clear-line-right)
                            (display visible-left)
                            (update-line-right right w cx)
                            (loop (cons op ll) hi left right cx off)))
                        (else
                          (cursor-right 1)
                          (loop ll hi (cons (car right) left) (cdr right) (+ cx 1) off))))
                    ((eq? dir 'up)
                      (cond
                        ((null? (cdr hi)) ;; nothing oldr available
                          (loop ll hi left right cx off))
                        (else
                          (lets 
                            ((new old hi)
                             (current (tuple left right off))
                             (left right off (history->state (car old) offset-delta))
                             (cx (update-line-left x y off left)))
                            (update-line-right right w cx)
                            (loop ll 
                              (cons (cons current new) (cdr old))
                              left right cx off)))))
                    ((eq? dir 'down)
                      (cond
                        ((null? (car hi)) ;; nothing newer available
                          (loop ll hi left right cx off))
                        (else
                          (lets 
                            ((new old hi)
                             (current (tuple left right off))
                             (left right off (history->state (car new) offset-delta))
                             (cx (update-line-left x y off left)))
                            (update-line-right right w cx)
                            (loop ll 
                              (cons (cdr new) (cons current old))
                              left right cx off)))))
                    (else
                      (tuple 'unsupported-arrow dir))))
                ((enter)
                  ;; debug
                  ; (print "readline -> " (append (reverse left) right))
                  (values ll
                    (list->string (append (reverse left) right))))
                ((nak)
                  (cursor-pos x y)
                  (update-line-right right w x)
                  (loop ll hi null right x off))
                ((end-of-text)
                  (values null #false))
                ((end-of-transmission)
                  (values null #false))
                ((data-link-escape) ;; ^p -> up
                   (loop (cons (tuple 'arrow 'up) ll) hi left right cx off))
                ((shift-out) ;; ^n -> down
                   (loop (cons (tuple 'arrow 'down) ll) hi left right cx off))
                (else
                  (values ll (tuple 'wat op))))))))

      (define editable-readline
        (case-lambda
          (() (readline (terminal-input) null))
          ((ll) (readline ll null))
          ((ll history) (readline ll history))))

      (define (read-line-interactive)
         (set-terminal-rawness #true)
         (let ((res (interactive-readline null null)))
            (set-terminal-rawness #false)
            res))

       (define failed "x")

       (define (port->readline-sexp-stream port prompt)
        (let loop ((history null) (ll (terminal-input)))
          (if prompt (display prompt))
          (set-terminal-rawness #true)
          (lets 
            ((x y ll (get-terminal-size (terminal-input)))
             (ll res (editable-readline ll history)))
            (set-terminal-rawness #false)
            (write-byte-vector stdout #(10))
            (if res
              (lets ((val (string->sexp res failed)))
                (if (eq? val failed)
                  (begin
                    (print ";; syntax error")
                    (loop (cons res history) ll))
                  (pair val (loop (cons res history) ll))))
              null))))

      (define (port->readline-line-stream port prompt)
        (let loop ((history null) (ll (terminal-input)))
          (if prompt (display prompt))
          (set-terminal-rawness #true)
          (lets 
            ((x y ll (get-terminal-size (terminal-input)))
             (ll res (editable-readline ll history)))
            (set-terminal-rawness #false)
            (write-byte-vector stdout #(10))
            (if res
              (pair res (loop (cons res history) ll))
              null))))

      '(lets/cc exit ()
        (lfold
          (λ (nth line) (print "\n" line) (if (equal? line "quit") (exit nth) (+ nth 1)))
          0 (port->readline-sexp-stream stdin "> ")))
      '(set-terminal-rawness #false)))


