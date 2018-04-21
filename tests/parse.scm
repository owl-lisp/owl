(import
   (owl parse))
   
(define (enlist x)
   (if (string? x)
      (string->list x)
      x))

(define (try data parser want-value want-tail)
   (print "testing '" data "' = " want-value)
   (lets
      ((input (string->list data))
       (res (parse-head parser input #f)))
      (if (pair? res)
         (if want-value
            (begin
               (if (not (equal? (enlist want-value) (enlist (car res))))
                  (print "ERROR: wanted '" want-value "', but got '" (car res) "'"))
               (if (not (equal? (enlist want-tail) (cdr res)))
                  (print "ERROR: tailing data is " (cdr res) " instead of " want-tail)))
            (print "ERROR: wanted failure, but got " res))
         (if want-value
            (print "ERROR: wanted " want-value ", but failed")))))

(define (cat a b)
   (cond
      ((number? a)
         (cat (list->string (list a)) b))
      ((number? b)
         (cat a (list->string (list b))))
      ((and (string? a) (string? b))
         (string-append a b))
      ((null? a) b)
      ((null? b) a)
      (else
         (list a b))))

(define (seq pa pb)
   (let-parses
      ((a pa)
       (b pb))
      (cat a b)))

(try "x" (ε 42) 42 "x")
   
(try "x" (imm #\x)
   #\x "")

(try "x" (imm #\a)
   #false "x")

(try "xxxa" (star (imm #\x))
   "xxx" "a")

(try "ababax" (star (seq (imm #\a) (imm #\b)))
   '("ab" "ab") "ax")
   
(try "abax" (plus (seq (imm #\a) (imm #\b)))
   '("ab") "ax")

(try "ax" (plus (seq (imm #\a) (imm #\b)))
   #false "ax")

(define aab (seq (plus (imm #\a)) (imm #\b)))
(define aac (seq (plus (imm #\a)) (imm #\c)))
(define aad (seq (plus (imm #\a)) (imm #\d)))

(try "adxy"
   (seq
      (star aad)
      (seq (imm #\a) 
           (imm #\d)))
   "ad"
   "xy")

(try "ax"
   (seq
      (either (imm #\a) (ε 42))
      (imm #\a))
   "*a"
   "x")
      
(try "abc"
   (seq (imm #\a)
      (seq (imm #\b)
         (imm #\d)))
   #false
   #false)
      
(try "abc"
   (either
      (let-parses
         ((a (imm #\a))
          (b (imm #\b))
          (d (imm #\d)))
         (list a b d))
      (imm #\a))
   97
   "bc")

(try "aaaax"
   (let-parses
      ((as1 (plus (imm #\a)))
       (as2 (plus (imm #\a)))
       (as3 (plus (imm #\a))))
      (list as1 as2 as3))
   '((#\a #\a) (#\a) (#\a))
   "x")

(define get-num
   (let-parses
      ((digits
         (plus
            (byte-between 47 58))))
      (fold
         (λ (n t) (+ (* n 10) (- t 48)))
         0 digits)))

(try "1234x"
   get-num
   1234
   "x")

(define ws
   (star
      (byte-if
         (λ (x) (has? '(#\space #\tab #\newline #\return) x)))))

(define get-exp
   (let-parses
      ((drop ws)
       (exp get-num))
      exp))

(define get-list
   (let-parses
      ((drop ws)
       (left (imm #\{))
       (ns 
          (star get-exp))
       (drop ws)
       (right (imm #\})))
      ns))
      
(try "           {  11 22   33 44 }x"
   get-list
   (list 11 22 33 44)
   "x")

