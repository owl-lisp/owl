
(define-library (owl date)
   (import
      (owl base))

   (export
      date
      leap-year?  ;; y → bool
      valid-date? ;; d m y → bool
      next-date   ;; d m y → d' m' y'
      week-info   ;; d m y → week weekday
      day-names-fi
      day-names-en
      date-str
      minute hour day week year leap-year)

   (begin

      ;; 0 = epoch @ Thu Jan 1 1970

      (define minute 60)
      (define hour (* 60 minute))
      (define day (* 24 hour))
      (define week (* day 7))
      (define year (* day 365))
      (define leap-year (+ year day))

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

      ;; date is valid *and* date computations work for it
      (define (valid-date? d m y)
         (and
            (and (fixnum? m) (<= 1 m 12))
            (and (integer? y) (> y 1200)) ;; check prior years also
            (and (fixnum? d) (>= d 1) 
               (<= d (days-in-month m y)))))

      ;;;
      ;;; Weekish calculations
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

      (define day-names-fi
         (tuple "maanantai" "tiistai" "keskiviikko" "torstai" "perjantai" "lauantai" "sunnuntai"))

      (define day-names-en
         (tuple "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))

      ;; naive-ish version
      (define (week-info d m y)
         (let loop ((rd 1) (rm 1) (week 1) (day (year-start-day y)))
            (if (and (= rd d) (= rm m))
               (values week day)
               (lets ((rd rm y (next-date rd rm y))
                      (day (if (= day 7) 1 (+ day 1)))
                      (week (if (= day 1) (+ week 1) week)))
                  (loop rd rm week day)))))

      ;;;
      ;;; UNIXish time
      ;;;

      (define leap-years-since-epoch   
         (let ((before-epoch (leap-years-before 1970)))
            (lambda (y) (- (leap-years-before y) before-epoch))))

      (define (seek-year s)
         (let loop ((s s) (y 1970))
            (let ((year-len (if (leap-year? y) leap-year year)))
               (if (> s year-len)
                  (loop (- s year-len) (+ y 1))
                  (values s y)))))

      (define (naive-date s)
         (lets ((s y (seek-year s)))
            (let loop ((d 1) (m 1) (y y) (s s))
               (if (< s day)
                  (lets
                     ((hour s (quotrem s hour))
                      (min s (quotrem s minute)))
                     (values d m y hour min s))
                  (lets ((d m y (next-date d m y)))
                     (loop d m y (- s day)))))))

      (define (zpad n)
         (if (< n 10) "0" ""))

      (define date
         (case-lambda
            (() (naive-date (time))) 
            ((sec) (naive-date sec))))

      (define (date-str s)
         (lets ((d m y H M S (naive-date s)))
            (str (zpad H) H ":" (zpad M) M ":" (zpad S) S " " d "." m "." y)))

))

