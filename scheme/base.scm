(define-library (scheme base)
  (export 
      *
      +
      -
      ...
      /
      <
      <=
      =
      =>
      >
      >=
      abs
      and
      append
      apply
      assoc
      assq
      assv
      begin
      binary-port?
      boolean=?
      boolean?
      bytevector
      bytevector-append
      bytevector-copy
      bytevector-copy!
      bytevector-length
      bytevector-u8-ref
      bytevector-u8-set!
      bytevector?
      caar
      cadr
      call-with-current-continuation
      call-with-port
      call-with-values
      call/cc
      car
      case
      cdar
      cddr
      cdr
      ceiling
      char->integer
      char-ready?
      char<=?
      char<?
      char=?
      char>=?
      char>?
      char?
      close-input-port
      close-output-port
      close-port
      complex?
      cond
      cond-expand
      cons
      current-error-port
      current-input-port
      current-output-port
      define
      define-record-type
      define-syntax
      define-values
      denominator
      do
      dynamic-wind
      else
      eof-object
      eof-object?
      eq?
      equal?
      eqv?
      error
      error-object-irritants
      error-object-message
      error-object?
      even?
      exact
      exact-integer-sqrt
      exact-integer?
      exact?
      expt
      features
      file-error?
      floor
      floor-quotient
      floor-remainder
      floor/
      flush-output-port
      for-each
      gcd
      get-output-bytevector
      get-output-string
      guard
      if
      include
      include-ci
      inexact
      inexact?
      input-port-open?
      input-port?
      integer->char
      integer?
      lambda
      lcm
      length
      let
      let*
      let*-values
      let-syntax
      let-values
      letrec
      letrec*
      letrec-syntax
      list
      list->string
      list->vector
      list-copy
      list-ref
      list-set!
      list-tail
      list?
      make-bytevector
      make-list
      make-parameter
      make-string
      make-vector
      map
      max
      member
      memq
      memv
      min
      modulo
      negative?
      newline
      not
      null?
      number->string
      number?
      numerator
      odd?
      open-input-bytevector
      open-input-string
      open-output-bytevector
      open-output-string
      or
      output-port-open?
      output-port?
      pair?
      parameterize
      peek-char
      peek-u8
      port?
      positive?
      procedure?
      quasiquote
      quote
      quotient
      raise
      raise-continuable
      rational?
      rationalize
      read-bytevector
      read-bytevector!
      read-char
      read-error?
      read-line
      read-string
      read-u8
      real?
      remainder
      reverse
      round
      set!
      set-car!
      set-cdr!
      square
      string
      string->list
      string->number
      string->symbol
      string->utf8
      string->vector
      string-append
      string-copy
      string-copy!
      string-fill!
      string-for-each
      string-length
      string-map
      string-ref
      string-set!
      string<=?
      string<?
      string=?
      string>=?
      string>?
      string?
      substring
      symbol->string
      symbol=?
      symbol?
      syntax-error
      syntax-rules
      textual-port?
      truncate
      truncate-quotient
      truncate-remainder
      truncate/
      u8-ready?
      unless
      unquote
      unquote-splicing
      utf8->string
      values
      vector
      vector->list
      vector->string
      vector-append
      vector-copy
      vector-copy!
      vector-fill!
      vector-for-each
      vector-length
      vector-map
      vector-ref
      vector-set!
      vector?
      when
      with-exception-handler
      write-bytevector
      write-char
      write-string
      write-u8
      zero?
      string->integer ;; NOT R7RS but used currently in many places in owl stuff
      )

   (import 
      (owl defmac)
      (owl equal)
      (owl list)
      (only (owl syscall) error)
      (owl string)
      (owl primop)
      (owl math-extra)
      (owl intern)
      (owl vector)
      (owl port)
      (owl symbol)
      (only (owl sexp) list->number)
      (owl list-extra)
      (owl io)
      (owl boolean)
      (owl math))

   (begin

      (define-syntax define-symbols
        (syntax-rules ()
          ((define-symbols x ...)
            (define-values (x ...) 
              (values (quote x) ...)))))

      (define-symbols ... => unquote unquote-splicing)

      (define-syntax define-missing-bad 
         (syntax-rules ()
            ((define-missing-bad name)
               (define name 
                  (lambda args
                     (error "Implementation restriction:" (cons (quote name) args)))))))

       (define-syntax define-missing-my-bad 
         (syntax-rules ()
            ((define-missing-my-bad name)
               (define name 
                  (lambda args
                     (error "Currently missing or incompatible:" (cons (quote name) args)))))))  


      ;; grr, scheme member functions don't follow the argument conventions of other functions used in owl...

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

      (define string->number
         (case-lambda
            ((str base)
               (list->number (string->list str) base))
            ((str)
               (list->number (string->list str) 10))))

      (define (string->integer str)
         (let ((n (string->number str 10)))
            (cond
               ((eq? (type n) type-fix+) n)
               ((eq? (type n) type-fix-) n)
               ((eq? (type n) type-int+) n)
               ((eq? (type n) type-int-) n)
               (else #false))))

      (define (number->string/base n base)
         (list->string (render-number n null base)))

      (define-syntax when
        (syntax-rules ()
          ((when test exp ...)
            (if test (begin exp ...)))))

      (define number->string
         (case-lambda
            ((n) (number->string/base n 10))
            ((n base) (number->string/base n base))))

      (define (square x) (* x x))

      (define-missing-bad write-u8)
      (define-missing-bad write-string)
      (define-missing-bad write-char)
      (define-missing-bad write-bytevector)
      (define-missing-bad with-exception-handler)
      (define-missing-bad vector-set!)
      (define-missing-bad vector-map)
      (define-missing-bad vector-for-each)
      (define-missing-bad vector-fill!)
      (define-missing-bad vector-copy!)
      (define-missing-bad vector-copy)
      (define-missing-bad vector-append)
      (define-missing-bad vector->string)
      (define-missing-bad utf8->string)
      (define-missing-bad unless)
      (define-missing-bad u8-ready?)
      (define-missing-bad truncate/)
      (define-missing-bad truncate-remainder)
      (define-missing-bad truncate-quotient)
      (define-missing-bad textual-port?)
      (define-missing-bad syntax-rules)
      (define-missing-bad symbol=?)
      (define-missing-bad symbol->string)
      (define-missing-bad string-set!)
      (define-missing-bad string-map)
      (define-missing-bad string-for-each)
      (define-missing-bad string-fill!)
      (define-missing-bad string-copy!)
      (define-missing-bad string->vector)
      (define-missing-bad string->utf8)
      (define-missing-bad set-cdr!)
      (define-missing-bad set-car!)
      (define-missing-bad set!)
      (define-missing-bad read-u8)
      (define-missing-bad read-string)
      (define-missing-bad read-line)
      (define-missing-bad read-error?)
      (define-missing-bad read-char)
      (define-missing-bad read-bytevector!)
      (define-missing-bad read-bytevector)
      (define-missing-bad raise-continuable)
      (define-missing-bad raise)
      (define-missing-bad procedure?)
      (define-missing-bad peek-u8)
      (define-missing-bad peek-char)
      (define-missing-bad parameterize)
      (define-missing-bad output-port?)
      (define-missing-bad output-port-open?)
      (define-missing-bad open-output-string)
      (define-missing-bad open-output-bytevector)
      (define-missing-bad open-input-string)
      (define-missing-bad open-input-bytevector)
      (define-missing-bad newline)
      (define-missing-bad make-parameter)
      (define-missing-bad make-list)
      (define-missing-bad make-bytevector)
      (define-missing-bad list-tail)
      (define-missing-bad list-set!)
      (define-missing-bad list-copy)
      (define-missing-bad letrec-syntax)
      (define-missing-bad let-values)
      (define-missing-bad let-syntax)
      (define-missing-bad integer->char)
      (define-missing-bad input-port?)
      (define-missing-bad input-port-open?)
      (define-missing-bad inexact)
      (define-missing-bad include-ci)
      (define-missing-bad include)
      (define-missing-bad guard)
      (define-missing-bad get-output-string)
      (define-missing-bad get-output-bytevector)
      (define-missing-bad flush-output-port)
      (define-missing-bad floor/)
      (define-missing-bad floor-remainder)
      (define-missing-bad floor-quotient)
      (define-missing-bad file-error?)
      (define-missing-bad features)
      (define-missing-bad exact-integer?)
      (define-missing-bad exact)
      (define-missing-bad error-object?)
      (define-missing-bad error-object-message)
      (define-missing-bad error-object-irritants)
      (define-missing-bad eof-object?)
      (define-missing-bad eof-object)
      (define-missing-bad else)
      (define-missing-bad dynamic-wind)
      (define-missing-bad current-output-port)
      (define-missing-bad current-input-port)
      (define-missing-bad current-error-port)
      (define-missing-bad cond-expand)
      (define-missing-bad close-output-port)
      (define-missing-bad close-input-port)
      (define-missing-bad char?)
      (define-missing-bad char>?)
      (define-missing-bad char>=?)
      (define-missing-bad char<?)
      (define-missing-bad char<=?)
      (define-missing-bad char-ready?)
      (define-missing-bad char->integer)
      (define-missing-bad call-with-port)
      (define-missing-bad bytevector?)
      (define-missing-bad bytevector-u8-set!)
      (define-missing-bad bytevector-u8-ref)
      (define-missing-bad bytevector-length)
      (define-missing-bad bytevector-copy!)
      (define-missing-bad bytevector-copy)
      (define-missing-bad bytevector-append)
      (define-missing-bad bytevector)
      (define-missing-bad boolean=?)
      (define-missing-bad binary-port?)
))
