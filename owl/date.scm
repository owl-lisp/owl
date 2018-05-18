;;; This library attempts to implement date processing functions. Dates
;;; are typically represented as seconds or milliseconds since UNIX Epoch 1.1.1970.
;;; These are generally a good way to work with time, apart from assuming
;;; that an absolute time exists at all. Sometimes it is however necessary
;;; to convert time stamps to human readable form, which means splitting
;;; the time according to various more and less sensible rules.
;;;
;;; ```
;;;    (time)                → current time in seconds since epoch
;;;    (time-ms)             → current time in milliseconds since epoch
;;;    (date-str 0)          → "00:00:00 1.1.1970 UTC+00:00"
;;;    (date-str (time) 2.5) → "20:49:08 19.3.2018 UTC+02:30"
;;;    (date 0)              → 1 1 1970 0 0 0 ; as multiple values
;;;    (leap-year? 2018)     → #false
;;; ```

;; todo: check how far before 1970 these hold
;; todo: --rfc-2822
;; todo: date-str timezone offsets

(define-library (owl date)

   (import
      (owl defmac)
      (owl math)
      (owl proof)
      (owl render)
      (owl time)
      (owl syscall))

   (export
      date
      leap-year?  ;; y → bool
      valid-date? ;; d m y → bool
      next-date   ;; d m y → d' m' y'
      next-date-with-week   ;; d m y wd wn → d' m' y' wd' wn'
      week-info   ;; d m y → week weekday
      day-names-fi
      day-names-en
      date-str    ;; secs [tz-offset-hours] -> str
      date-str-tz ;; secs tz-offset-hours -> str
      year-start-day
      year-start-week-info
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

      (define (year-start-day y)
         (+ 1 (remainder (+ 5 (* 365 y) (leap-years-before y)) 7)))

      (define day-names-fi
         (tuple "maanantai" "tiistai" "keskiviikko" "torstai" "perjantai" "lauantai" "sunnuntai"))

      (define day-names-en
         (tuple "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))

      ;; compute day of week and ISO-compliant week number
      (define (year-start-week-info y)
        (let ((d (year-start-day y)))
          (cond
            ((< d 5) ;; mon-wed, week 1
              (values d 1))
            ((eq? d 5) ;; friday - week 53 of previous
               (values d 53))
            ((eq? d 6) ;; saturday - week 52, or 53 if previous was a leap year
               (values d (+ 52 (if (leap-year? (- y 1)) 1 0))))
            (else ;; sunday
               (values d 52)))))

      ;; the days during the last week of the year belong instead to week
      ;; 1 of the subsequent year, if the thursday of the week belongs to
      ;; the next year

      (define (days-to-thursday d)
        (if (< d 5)
          (- 4 d)
          (+ 4 (- 7 d))))

      (define days-to-sunday
         (H - 7))

      (define (maybe-swap-year y md week day)
         (cond
            ((< week 52) (values week day))
            ((> day 3)
              ;; thursday already contained in this week
              (values week day))
            ((< (+ md (days-to-sunday day)) 32)
              ;; whole week fits the year
              (values week day))
            ((< (+ md (days-to-thursday day)) 32)
              ;; partial week, but switch happens before thursday
              (values week day))
            (else
              ;; subsequent thursday falls to next year
              (values 1 day))))

      ;; naive-ish but corret version, against which to add tests
      (define (week-info d m y)
         (lets ((day week (year-start-week-info y))
                (reset? (not (eq? week 1)))) ;; reset to week 1 on next monday
           (let loop ((rd 1) (rm 1) (week week) (day day) (reset? reset?))
              (if (and (= rd d) (= rm m))
                 (maybe-swap-year y d week day)
                 (lets ((rd rm y (next-date rd rm y))
                        (day (if (eq? day 7) 1 (+ day 1))))
                   (if (eq? day 1)
                      (if reset?
                        (loop rd rm 1 day #false)
                        (loop rd rm (+ week 1) day reset?))
                      (loop rd rm week day reset?)))))))

      (define (next-date-with-week day month year week-day week-num)
         (lets ((d m y (next-date day month year)))
            (if (eq? week-day 7)
               (if (< week-num 52)
                  (values d m y 1 (+ week-num 1))
                  (lets ((wn wd (week-info d m y)))
                     (values d m y wd wn)))
               (values d m y (+ week-day 1) week-num))))

      (example
         (next-date-with-week 31 12 1971 5 1) = (values 1 1 1972 6 1)
         (next-date-with-week 27 12 1970 7 52) = (values 28 12 1970 1 53))

      ;;;
      ;;; UNIXish time
      ;;;

      (define leap-years-since-epoch
         (B (C - (leap-years-before 1970)) leap-years-before))

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

      (define (hours->secs h)
         (floor (* h 3600)))

      (define (date-str-tz s tz)
         (lets ((d m y H M S (naive-date (+ s tz)))
                (tz-sign (if (< tz 0) "-" "+"))
                (tz (abs tz))
                (tz-mins _ (quotrem tz 60))
                (tz-hours tz-mins (quotrem tz-mins 60)))
            (str (zpad H) H ":" (zpad M) M ":" (zpad S) S
                 " " d "." m "." y
                 " UTC" tz-sign (zpad tz-hours) tz-hours ":" (zpad tz-mins) tz-mins)))

      (define (date-str s . tz)
         (if (null? tz)
            (date-str-tz s 0)
            (date-str-tz s (hours->secs (car tz)))))

      ; TZ=GMT date -d @1234567890
      (example
         (date-str 0)            = "00:00:00 1.1.1970 UTC+00:00"
         (date-str 0 2.5)        = "02:30:00 1.1.1970 UTC+02:30"
         (date-str 1234567890 0) = "23:31:30 13.2.2009 UTC+00:00")

))
