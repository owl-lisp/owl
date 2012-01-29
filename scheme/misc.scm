(define-library (scheme misc)
   (export 
      member memq memv 
      assoc assv assq
      apply rationalize
      string->integer)

   (import 
      (owl defmac)
      (owl equal)
      (owl list)
      (only (owl syscall) error)
      (owl string)
      (owl math))

   (begin
      ;; scheme member functions don't follow the argument conventions of other functions 
      (define (member x lst)
         (cond
            ((null? lst) #false)
            ((equal? x (car lst)) lst)
            (else (member x (cdr lst)))))

      (define memv member)

      (define (memq x lst)
         (cond
            ((null? lst) #false)
            ((eq? x (car lst)) lst)
            (else (memq x (cdr lst)))))

      (define (assq k l)
         (cond
            ((null? l) #f)
            ((eq? (caar l) k) (car l))
            (else (assq k (cdr l)))))
      
      (define (assv k l)
         (cond
            ((null? l) #f)
            ((equal? (caar l) k) (car l))
            (else (assv k (cdr l)))))

      (define assoc assv)

      ;; a silly non-primitive apply
      (define (apply func l)
         (if (null? l)
            (func)
            (lets ((a l l)) (if (null? l) (func a)
            (lets ((b l l)) (if (null? l) (func a b)
            (lets ((c l l)) (if (null? l) (func a b c)
            (lets ((d l l)) (if (null? l) (func a b c d)
            (lets ((e l l)) (if (null? l) (func a b c d e)
            (lets ((f l l)) (if (null? l) (func a b c d e f)
               (error "apply: too many arguments: " (ilist a b c d e f l))))))))))))))))

      ;; owl doesn't have inexact numbers, so any argument
      ;; coming in will always be rational differing by 0
      (define (rationalize n max-delta) n)

;; todo: move string->integer elsewhere
;;; string base -> number | #false
(define string->integer/base

   (define (byte->digit val base)
      (cond
         ((and (<= 48 val) (<= val 57))
            (let ((val (- val 48)))
               (if (< val base) val #false)))
         ((and (<= 97 val) (<= val 122))
            (let ((val (+ (- val 97) 10)))
               (if (< val base) val #false)))
         (else #false)))

   (define (digits->number s pos n base)
      (cond
         ((= pos (string-length s))
            n)
         ((byte->digit (refb s pos) base) =>
            (λ (this)
               (digits->number s (+ pos 1) (+ (* n base) this) base)))
         (else #false)))

   (λ (s base)
      (let ((len (string-length s)))
            (if (> len 0)
               (let ((first (refb s 0)))
                  (cond
                     ((eq? first 43)
                        (digits->number s 1 0 base))
                     ((eq? first 45)
                        (cond
                           ((digits->number s 1 0 base) =>
                              (λ (num) (- 0 num)))
                           (else #false)))
                     (else
                        (digits->number s 0 0 base))))
               #false))))

(define (string->integer str)
   (string->integer/base str 10))



   ))
