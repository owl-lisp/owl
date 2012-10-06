;;;
;;; Testing the intersection of R5RS and Owl
;;;

(define (print thing)
   ;; silly R5RS & Owl version
   (display thing)
   (display "
"))

;; print something if bad result
(define (check name thing wanted)
   (display name)
   (display " ")
   (if (equal? thing wanted)
      (print "ok")
      (begin 
         (print " *** WRONG *** ")
         (print (list 'got thing))
         (print (list 'not wanted)))))

(define-syntax compare-results
   (syntax-rules (chapter ===>)
      ((compare-results chapter name . stuff)
         (begin
            (print name)
            (compare-results . stuff)))
      ((compare-results term ===> wanted . rest)
         (let ((val term))
            (if (equal? val (quote wanted))
               (compare-results . rest)
               (print (list (quote term) " evaluates to " val " instead of " (quote wanted))))))
      ((compare-results) 'ok)))

;; definitions used in comparisons 
(define x 28)
(define reverse-subtract
  (lambda (x y) (- y x)))
(define add4
  (let ((x 4))
      (lambda (y) (+ x y))))
(define e '((a 1) (b 2) (c 3)))
(define (f) (make-string 3 #\*))
(define (g) "***")

(define a-stream
  (letrec ((next
            (lambda (n)
              (cons n (delay (next (+ n 1)))))))
    (next 0)))
(define head car)
(define tail
  (lambda (stream) (force (cdr stream))))

(define list-length
  (lambda (obj)
    (call-with-current-continuation
      (lambda (return)
        (letrec ((r
                  (lambda (obj)
                    (cond ((null? obj) 0)
                          ((pair? obj)
                           (+ (r (cdr obj)) 1))
                          (else (return #f))))))
          (r obj))))))



(compare-results

   chapter "1.3.4" 

      (* 5 8)                         ===> 40

   chapter "4.1.1"

      x                               ===> 28

   chapter "4.1.2"

      (quote a)                       ===>  a
      (quote #(a b c))                ===>  #(a b c)
      (quote (+ 1 2))                 ===>  (+ 1 2)
      'a                              ===>  a
      '#(a b c)                       ===>  #(a b c)
      '()                             ===>  ()
      '(+ 1 2)                        ===>  (+ 1 2)
      '(quote a)                      ===>  (quote a)
      ''a                             ===>  (quote a)
      '"abc"                          ===>  "abc"
      "abc"                           ===>  "abc"
      '145932                         ===>  145932
      145932                          ===>  145932
      '#t                             ===>  #t
      #t                              ===>  #t

   chapter "4.1.3"

      (+ 3 4)                         ===>  7
      ((if #f + *) 3 4)               ===>  12

   chapter "4.1.4"

      ((lambda (x) (+ x x)) 4)        ===>  8
      (reverse-subtract 7 10)         ===>  3
      (add4 6)                        ===>  10

   chapter "4.1.5"

      (if (> 3 2) 'yes 'no)           ===>  yes
      (if (> 2 3) 'yes 'no)           ===>  no
      (if (> 3 2)
         (- 3 2)
         (+ 3 2))                     ===>  1

   chapter "4.2.1" 

      (cond 
         ((> 3 2) 'greater)
         ((< 3 2) 'less))             ===>  greater
      (cond ((> 3 3) 'greater)
         ((< 3 3) 'less)
         (else 'equal))               ===>  equal
      (cond 
         ((assv 'b '((a 1) (b 2))) => cadr)
         (else #f))                   ===>  2

      (case (* 2 3)
         ((2 3 5 7) 'prime)
         ((1 4 6 8 9) 'composite))    ===>  composite
      ;(case (car '(c d))
      ;   ((a) 'a)
      ;   ((b) 'b))                   ===>  unspecified
      (case (car '(c d)) 
         ((a e i o u) 'vowel) 
         ((w y) 'semivowel) 
         (else 'consonant))           ===>  consonant

      (and (= 2 2) (> 2 1))           ===>  #t
      (and (= 2 2) (< 2 1))           ===>  #f
      (and 1 2 'c '(f g))             ===>  (f g)
      (and)                           ===>  #t

      (or (= 2 2) (> 2 1))            ===>  #t
      (or (= 2 2) (< 2 1))            ===>  #t
      (or #f #f #f)                   ===>  #f
      (or (memq 'b '(a b c)) 
          (/ 3 0))                    ===>  (b c)

   chapter "4.2.2"

      (let ((x 2) (y 3))
        (* x y))                      ===>  6

      (let ((x 2) (y 3))
         (let ((x 7) (z (+ x y)))
            (* z x)))                 ===>  35

      (let ((x 2) (y 3))
        (let* ((x 7)
               (z (+ x y)))
               (* z x)))              ===>  70

      (letrec 
         ((even? (lambda (n) (if (zero? n) #t (odd? (- n 1)))))
          (odd? (lambda (n) (if (zero? n) #f (even? (- n 1)))))) 
         (even? 88))                  ===>  #t

   chapter "4.2.4"

      (let ((x '(1 3 5 7 9)))
         (do 
            ((x x (cdr x)) (sum 0 (+ sum (car x)))) 
            ((null? x) sum)))         ===>  25

      (let loop ((numbers '(3 -2 1 6 -5))
                 (nonneg '())
                 (neg '()))
        (cond ((null? numbers) (list nonneg neg))
              ((>= (car numbers) 0)
               (loop (cdr numbers)
                     (cons (car numbers) nonneg)
                     neg))
              ((< (car numbers) 0)
               (loop (cdr numbers)
                     nonneg
                     (cons (car numbers) neg)))))   
                                      ===>  ((6 1 3) (-5 -2))

   chapter "4.2.6"

      `(list ,(+ 1 2) 4)              ===>  (list 3 4)
      (let ((name 'a)) `(list ,name ',name))            
                                      ===>  (list a (quote a))
      `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)             
                                      ===>  (a 3 4 5 6 b)
      `(( foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))) 
                                      ===>  ((foo 7) . cons)
      `#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8) ;; note: will fail unless sqrt preserves exactness
                                     ===>  #(10 5 2 4 3 8)

     `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)          
                                      ===>  (a `(b ,(+ 1 2) ,(foo 4 d) e) f)
     (let ((name1 'x) (name2 'y)) `(a `(b ,,name1 ,',name2 d) e))           
                                      ===>  (a `(b ,x ,'y d) e)


      (quasiquote (list (unquote (+ 1 2)) 4))           
                                      ===>  (list 3 4)
      '(quasiquote (list (unquote (+ 1 2)) 4))          
                                      ===>  `(list ,(+ 1 2) 4)

   chapter "4.3.2"

      ;; FIXME no let*-syntax
      ;(let ((=> #f)) (cond (#t => 'ok))) 
      ;                               ===> ok ;; FIXME behaves wrong

   chapter "5.2.2"

      (let ((x 5))
        (define foo (lambda (y) (bar x y)))
        (define bar (lambda (a b) (+ (* a b) a)))
        (foo (+ x 3)))                ===>  45


   chapter "6.1" 

      (eqv? 'a 'a)                    ===>  #t
      (eqv? 'a 'b)                    ===>  #f
      (eqv? 2 2)                      ===>  #t
      (eqv? '() '())                  ===>  #t
      (eqv? 100000000 100000000)      ===>  #t
      ;(eqv? (cons 1 2) (cons 1 2))   ===>  #f  ;; FIXME compat eqv? is too equal? atm
      (eqv? (lambda () 1)
            (lambda () 2))            ===>  #f
      (eqv? #f 'nil)                  ===>  #f
      (let ((p (lambda (x) x)))
        (eqv? p p))                   ===>  #t

      (letrec 
         ((f (lambda () (if (eqv? f g) 'f 'both)))
          (g (lambda () (if (eqv? f g) 'g 'both))))
         (eqv? f g))                  ===>  #f

      (let ((x '(a)))
        (eqv? x x))                   ===>  #t

      (eq? 'a 'a)                     ===>  #t
      (eq? (list 'a) (list 'a))       ===>  #f
      (eq? '() '())                   ===>  #t
      (eq? car car)                   ===>  #t
      (let ((x '(a)))
        (eq? x x))                    ===>  #t
      (let ((x '#()))
        (eq? x x))                    ===>  #t
      (let ((p (lambda (x) x)))
        (eq? p p))                    ===>  #t
      (equal? 'a 'a)                  ===>  #t
      (equal? '(a) '(a))              ===>  #t
      (equal? '(a (b) c)
              '(a (b) c))             ===>  #t
      (equal? "abc" "abc")            ===>  #t
      (equal? 2 2)                    ===>  #t
      ;(equal? (make-vector 5 'a) 
      ;   (make-vector 5 'a))         ===>  #t ;; TODO make-vector not there

   chapter "6.2.5"
                                                                                                                                                  
      (complex? 3+4i)                 ===>  #t
      (complex? 3)                    ===>  #t
      (real? 3)                       ===>  #t
      (real? -2.5+0.0i)               ===>  #t
      ;(real? #e1e10)                 ===>  #t ;; FIXME: unsupported number syntax
      (rational? 6/10)                ===>  #t
      (rational? 6/3)                 ===>  #t
      (integer? 3+0i)                 ===>  #t
      (integer? 3.0)                  ===>  #t
      (integer? 8/4)                  ===>  #t

      (- 3 4)                         ===>  -1
      (- 3 4 5)                       ===>  -6
      (- 3)                           ===>  -3
      (/ 3 4 5)                       ===>  3/20
      (/ 3)                           ===>  1/3
      
      (abs -7)                        ===>  7

      (modulo 13 4)                   ===>  1
      (remainder 13 4)                ===>  1

      (modulo -13 4)                  ===>  3
      (remainder -13 4)               ===>  -1

      (modulo 13 -4)                  ===>  -3
      (remainder 13 -4)               ===>  1

      (modulo -13 -4)                 ===>  -1
      (remainder -13 -4)              ===>  -1

      (remainder -13 -4.0)            ===>  -1.0

      (gcd 32 -36)                    ===>  4
      (gcd)                           ===>  0
      (lcm 32 -36)                    ===>  288
      (lcm 32.0 -36)                  ===>  288.0  ; inexact ;; no it's not --owl
      (lcm)                           ===>  1
      
      (numerator (/ 6 4))             ===>  3
      (denominator (/ 6 4))           ===>  2
      (denominator
        (exact->inexact (/ 6 4)))     ===> 2.0

      (floor -4.3)                    ===>  -5.0
      (ceiling -4.3)                  ===>  -4.0
      (truncate -4.3)                 ===>  -4.0
      (round -4.3)                    ===>  -4.0
      (floor 3.5)                     ===>  3.0
      (ceiling 3.5)                   ===>  4.0
      (truncate 3.5)                  ===>  3.0
      (round 3.5)                     ===>  4.0  ; inexact ;; no it's not --owl
      (round 7/2)                     ===>  4    ; exact
      (round 7)                       ===>  7

      (max 3 4)                       ===>  4    ; exact
      (max 3.9 4)                     ===>  4.0  ; inexact ;; no it's not --owl

      ;; additions
      (sqrt (expt 11111111111 2))     ===> 11111111111 ;; note: will fail unless sqrt preserves exactness
      (sqrt -4)                       ===> 0+2i ;; ditto
      (expt 0 0)                      ===> 1
      (expt 0 1)                      ===> 0
      (number->string 3333333333333333 2) 
                                      ===> "1011110101111010011000100101010000000101010101010101"
      (number->string 3333333333333333 3) 
                                      ===> "121012010100112222020212022011210"
      (number->string 3333333333333333 11) 
                                      ===> "886114800933a20"
      (number->string 3333333333333333 16) 
                                      ===> "bd7a625405555"
  
      ;; FIXME string->number is different from R5RS

   chapter "6.3.1"

      (not #t)                        ===>  #f
      (not 3)                         ===>  #f
      (not (list 3))                  ===>  #f
      (not #f)                        ===>  #t
      (not '())                       ===>  #f
      (not (list))                    ===>  #f
      (not 'nil)                      ===>  #f

      (boolean? #f)                   ===>  #t
      (boolean? 0)                    ===>  #f
      (boolean? '())                  ===>  #f

   chapter "6.3.2"

      (pair? '(a . b))                ===>  #t
      (pair? '(a b c))                ===>  #t
      (pair? '())                     ===>  #f
      (pair? '#(a b))                 ===>  #f

      (cons 'a '())                   ===>  (a)
      (cons '(a) '(b c d))            ===>  ((a) b c d)
      (cons "a" '(b c))               ===>  ("a" b c)
      (cons 'a 3)                     ===>  (a . 3)
      (cons '(a b) 'c)                ===>  ((a b) . c)

      (car '(a b c))                  ===>  a
      (car '((a) b c d))              ===>  (a)
      (car '(1 . 2))                  ===>  1

      (cdr '((a) b c d))              ===>  (b c d)
      (cdr '(1 . 2))                  ===>  2
      
      (list? '(a b c))                ===>  #t
      (list? '())                     ===>  #t
      (list? '(a . b))                ===>  #f

      (length '(a b c))               ===>  3
      (length '(a (b) (c d e)))       ===>  3
      (length '())                    ===>  0
      
      (append '(x) '(y))              ===>  (x y)
      (append '(a) '(b c d))          ===>  (a b c d)
      (append '(a (b)) '((c)))        ===>  (a (b) (c))
      
      (append '(a b) '(c . d))        ===>  (a b c . d)
      (append '() 'a)                 ===>  a

      (reverse '(a b c))              ===>  (c b a)
      (reverse '(a (b c) d (e (f))))  ===>  ((e (f)) d (b c) a)

      (memq 'a '(a b c))              ===>  (a b c)
      (memq 'b '(a b c))              ===>  (b c)
      (memq 'a '(b c d))              ===>  #f
      (memq (list 'a) '(b (a) c))     ===>  #f
      (member (list 'a)
              '(b (a) c))             ===>  ((a) c)
      (memv 101 '(100 101 102))       ===>  (101 102)

      (assq 'a e)                     ===>  (a 1)
      (assq 'b e)                     ===>  (b 2)
      (assq 'd e)                     ===>  #f
      (assq (list 'a) '(((a)) ((b)) ((c))))
                                      ===>  #f                                                                                                                            
      (assoc (list 'a) '(((a)) ((b)) ((c))))
                                      ===>  ((a))                                                                                                              
      (assv 5 '((2 3) (5 7) (11 13)))
                                      ===>  (5 7)                    

   chapter "6.3.3"

      (symbol? 'foo)                  ===>  #t
      (symbol? (car '(a b)))          ===>  #t
      (symbol? "bar")                 ===>  #f
      (symbol? 'nil)                  ===>  #t
      (symbol? '())                   ===>  #f
      (symbol? #f)                    ===>  #f                   

      (symbol->string 'flying-fish)   ===>  "flying-fish"
      (symbol->string 'martin)        ===>  "martin"     ;; note, not 'Martin
      (symbol->string (string->symbol "Malvina")) 
                                      ===> "Malvina"

   chapter "6.3.5" ;; strings
      
      ; additions
      (string=? "a" "")               ===> #f
      (string-ci=? "Aa" "aA")         ===> #t
      (string<? "a" "b")              ===> #t
      (string<=? "a" "b")             ===> #t
      (string>=? "a" "b")             ===> #f
      (string>? "a" "b")              ===> #f
      (string-ci<? "Aa" "AAa")        ===> #t
      (string-ci>? "Aa" "AAA")        ===> #f
      
      (substring "xkappaz" 1 6)       ===> "kappa"

      (list->string 
         (string->list "abc"))        ===> "abc"

      (string-append "foo" "bar")     ===> "foobar"

      (string-copy "a")               ===> "a"

      (make-string 3 #\a)             ===> "aaa"

   chapter "6.3.6"

      (vector->list '#(dah dah didah)) 
                                      ===>  (dah dah didah) 
      (list->vector '(dididit dah))   ===>  #(dididit dah)

      ;; additions

      (make-vector 3 'betelgeuse)     ===> #(betelgeuse betelgeuse betelgeuse)
      (vector? '(foo))                ===> #f
      (vector 'a 'b 'c)               ===>  #(a b c)

      (vector-ref '#(1 1 2 3 5 8 13 21) 5)  
                                      ===>  8
   chapter "6.4"

      (procedure? car)                ===>  #t
      (procedure? 'car)               ===>  #f
      (procedure? (lambda (x) (* x x)))
                                      ===>  #t
      (procedure? '(lambda (x) (* x x)))
                                      ===>  #f
      (call-with-current-continuation procedure?) 
                                      ===>  #t

      (map cadr '((a b) (d e) (g h))) ===>  (b e h)
      (map (lambda (n) (expt n n)) '(1 2 3 4 5))
                                      ===>  (1 4 27 256 3125)                                                  

      (apply + (list 3 4))            ===>  7    ;; not a primop yet though

      (map cadr '((a b) (d e) (g h))) ===>  (b e h)

      
      (map (lambda (n) (expt n n)) '(1 2 3 4 5))
                                      ===>  (1 4 27 256 3125)

      (force (delay (+ 1 2)))         ===>  3

      (let ((p (delay (+ 1 2))))
        (list (force p) (force p)))   ===>  (3 3)   ;; note -- NO CACHING there being no side effects

      (head (tail (tail a-stream)))   ===>  2

      (call-with-current-continuation
        (lambda (exit)
          (for-each (lambda (x)
                      (if (negative? x)
                          (exit x)))
                    '(54 0 37 -3 245 19))
          #t))                        ===>  -3

      
      (list-length '(1 2 3 4))        ===>  4

      (list-length '(a b . c))        ===>  #f

      (call-with-values (lambda () (values 4 5)) (lambda (a b) b))
                                      ===>  5

      ;; FIXME: environment fetches missing
      ;(eval '(* 7 3) (scheme-report-environment 5))
      ;                               ===>  21

      ;(eval 'x (interaction-environment))       ;; defined above 
      ;                               ===> 28

      ;; most of the IO and system interaction don't apply to owl (yet)
)
