
(define-library (owl date)
   (import
      (owl base))

   (export
      date
      leap-year?
      next-date ;; d m y → d' m' y'
      )

   (begin

      ;; 0 = epoch @ Thu Jan 1 1970

      (define minute 60)

      (define hour (* 60 minute))

      (define day (* 24 hour))
      
      (define week (* day 7))

      (define (day-name n)
         (vector-ref #(Thursday Friday Saturday Sunday Monday Tuesday Wednesday) n))

      (define (day-of x)
         (let loop ((x (modulo x week)) (daynum 0))
            (if (< x day)
               (day-name daynum)
               (loop (- x day) (+ daynum 1)))))

      (define (date-of x)
         (print x " = " (day-of x)))

      (define date
         (case-lambda
            (()  (date-of (time)))
            ((x) (date-of x))))

      ;;;
      ;;; D M Y calculations
      ;;;

      (define (leap-year? n)
         (let ((q (band n #b11)))
            (if (eq? q 0)
               ;; divisible by 4
               (let ((n (>> n 2)))
                  (if (eq? 0 (remainder n 25))
                     ;; divisible by 100
                     (eq? 0 (remainder n 100))
                     #true))
               #false)))

      (define month-durations 
         (tuple 31 #false 31 30 31 30 31 31 30 31 30 31))
      
      (define (days-in-month month year)
         (if (eq? month 2)
            (if (leap-year? year) 29 28)
            (ref month-durations month)))

      (define (next-date day month year)
         (cond
            ((< day 28)
               (values (+ day 1) month year))
            ((= day (days-in-month month year))
               (if (= month 12)
                  (values 1 1 (+ year 1))
                  (values 1 (+ month 1) year)))
            (else
               (values (+ day 1) month year))))

      (define (valid-date? d m y)
         (and
            (and (fixnum? m) (<= 1 m 12)))
            (integer? y)
            (and (fixnum? d) (>= d 1))
            (<= d (days-in-month m y)))

      ;;;
      ;;; Weekday calculations
      ;;;

      (define (leap-years-before y)
         (cond
            ((< y 5) 
               (if (< y 1)
                  (error "year must be >=1, but was  " y)
                  1))
            (else
               (lets
                  ((y   (- y 1))
                   (a _ (quotrem y 4))
                   (b _ (quotrem y 100))
                   (c _ (quotrem y 400)))
                  (+ 1 (- (+ (- a 0) (- c 0)) (- b 0)))))))

      ;; fixme, this holds from 1200-ish onwards. what happened there?
      (define (year-start-day y)
         (+ 1 (remainder (+ 5 (* 365 y) (leap-years-before y)) 7)))

      ;; not there yet
      (define (week-info d m y)
         (let loop ((rd 1) (rm m) (week 1) (day (year-start-day y)))
            (if (and (= rd d) (= rm m))
               (values week day)
               (lets ((rd rm y (next-date rd rm y))
                      (day (if (= day 7) 1 (+ day 1)))
                      (week (if (= day 1) (+ week 1) week)))
                  (loop rd rm week day)))))

))


