
(import (owl date))

(print "x " (valid-date? 0 1 1300))
(print "o " (valid-date? 16 1 2300))
(print "x " (valid-date? 32 1 3300))
(print "x " (valid-date? 1 0 4300))
(print "o " (valid-date? 1 6 9999999999999))
(print "x " (valid-date? 1 6.5 3424))
(print "x " (valid-date? 1 13 4324132))

(print "--")

(define (check-date d m y want)
   (lets ((week day (week-info d m y))
          (day (ref day-names-en day)))
      (if (equal? day want)
         (print d "." m "." y " is " day " of week " week)
         (print d "." m "." y " is " want " of week " week ", but computed " day))))

(check-date 1 1 1970 "Thursday")
(check-date 15 6 1215 "Monday")
(check-date 19 1 2038 "Tuesday")
(check-date 23 2 2016 "Tuesday")
(check-date 1  5 2061 "Sunday")
(check-date 31 12 9999 "Friday")

