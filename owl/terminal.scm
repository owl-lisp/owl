(define-library (owl terminal)

   (import
      (owl base)
      (owl io)
      (owl sys))

   (export
      set-terminal-rawness
      normal-text
      bold-text
      lowint-text
      underlined-text
      reverse-text
      invisible-text)

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
         
      (define (read-line-interactive)
         (set-terminal-rawness #true)
         (let ((res (interactive-readline null null)))
            (set-terminal-rawness #false)
            res))

      ;(display "Interactive readline: ") (print (read-line-interactive))
      
      ))


