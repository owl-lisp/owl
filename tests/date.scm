
(import (owl date))

(print "x " (valid-date? 0 1 1300))
(print "o " (valid-date? 16 1 2300))
(print "x " (valid-date? 32 1 3300))
(print "x " (valid-date? 1 0 4300))
(print "o " (valid-date? 1 6 9999999999999))
(print "x " (valid-date? 1 6.5 3424))
(print "x " (valid-date? 1 13 4324132))

(print "--")

(define (check-date d m y want-day want-week)
   (lets ((week day (week-info d m y))
          (day (ref day-names-en day)))
      (cond
        ((not (equal? day want-day))
         (print d "." m "." y " is " day " of week " week ", but wanted day " want-day))
        ((not (equal? week want-week))
         (print d "." m "." y " is " day " of week " week ", but wanted week " want-week))
        (else
         (print d "." m "." y " is " day " of week " week)))))

(check-date  1  1 1970 "Thursday"  1)
(check-date 31 12 1970 "Thursday" 53)
(check-date  1  1 1971 "Friday" 53)
(check-date 31 12 1971 "Friday" 52)
(check-date  1  1 1972 "Saturday" 52)
(check-date 31 12 1972 "Sunday" 52)
(check-date  1  1 1973 "Monday" 1)
(check-date 31 12 1973 "Monday" 1)
(check-date  1  1 1974 "Tuesday" 1)
(check-date 31 12 1974 "Tuesday" 1)
(check-date  1  1 1975 "Wednesday" 1)
(check-date 31 12 1975 "Wednesday" 1)
(check-date  1  1 1976 "Thursday" 1)
(check-date 31 12 1976 "Friday" 53)
(check-date  1  1 1977 "Saturday" 53)
(check-date 31 12 1977 "Saturday" 52)
(check-date  1  1 1978 "Sunday" 52)
(check-date 31 12 1978 "Sunday" 52)
(check-date  1  1 1979 "Monday" 1)
(check-date 31 12 1979 "Monday" 1)
(check-date  1  1 1980 "Tuesday" 1)
(check-date 31 12 1980 "Wednesday" 1)
(check-date  1  1 1981 "Thursday" 1)
(check-date 31 12 1981 "Thursday" 53)
(check-date  1  1 1982 "Friday" 53)
(check-date 31 12 1982 "Friday" 52)
(check-date 19 1 2038 "Tuesday" 3)
(check-date 23 2 2016 "Tuesday" 8)
(check-date 1  5 2061 "Sunday" 17)
(check-date 31 12 9999 "Friday" 52)





