(define-library (scheme misc)
   (export 
      member memq memv 
      assoc assv assq
      rationalize
      string->integer
      string->number
      )

   (import 
      (owl defmac)
      (owl equal)
      (owl list)
      (only (owl syscall) error)
      (owl string)
      (only (owl sexp) list->number)
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
      ;(define (apply func l)
      ;   (if (null? l)
      ;      (func)
      ;      (lets ((a l l)) (if (null? l) (func a)
      ;      (lets ((b l l)) (if (null? l) (func a b)
      ;      (lets ((c l l)) (if (null? l) (func a b c)
      ;      (lets ((d l l)) (if (null? l) (func a b c d)
      ;      (lets ((e l l)) (if (null? l) (func a b c d e)
      ;      (lets ((f l l)) (if (null? l) (func a b c d e f)
      ;         (error "apply: too many arguments: " (ilist a b c d e f l))))))))))))))))

      ;; owl doesn't have inexact numbers, so any argument
      ;; coming in will always be rational differing by 0
      (define (rationalize n max-delta) n)

   
   (define (string->number str base)
      (list->number (string->list str) base))

   (define (string->integer str)
      (let ((n (string->number str 10)))
         (cond
            ((eq? (type n) type-fix+) n)
            ((eq? (type n) type-fix-) n)
            ((eq? (type n) type-int+) n)
            ((eq? (type n) type-int-) n)
            (else #false))))

   ))
