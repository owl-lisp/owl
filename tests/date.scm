
; $ LC_ALL=C date -u --date @0 +"%s = %V %A, %H:%M:%S, %d.%m.%Y"
; 0 = 01 Thursday, 00:00:00, 01.01.1970

; on-line test:
; (cat owl/date.scm tests/date.scm; while true; do sleep 0.00$RANDOM;  LC_ALL=C date -u --date @$(ol -e '(lets ((rs x (rand (seed->rands (time-ms)) 10000000000))) x)') +"(check-posix->date %s \"%V %A %H:%M:%S %d.%m.%Y\")"; done) | ol | tee /tmp/out


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

(define (pad x) (if (< x 10) (str "0" x) x))

(define (check-posix->date posix wanted)
  (lets
    ((d m y h min s (date posix))
     (week day (week-info d m y))
     (day (ref day-names-en day))
     (computed (str (pad week) " " day " " (pad h) ":" (pad min) ":" (pad s) " " (pad d) "." (pad m) "." y)))
    (if (equal? wanted computed)
      (print posix " = " computed " - OK")
      (print posix "\n   wanted: " wanted "\n      got: " computed))))

; $ LC_ALL=C date -u --date @$(ol -e '(lets ((rs x (rand (seed->rands (time-ms)) 10000000000))) x)') +"(check-posix->date %s \"%V %A %H:%M:%S %d.%m.%Y\")"

(check-posix->date 0 "01 Thursday 00:00:00 01.01.1970")
(check-posix->date 5241218141 "05 Thursday 05:55:41 02.02.2136")
(check-posix->date 1869012742 "12 Saturday 02:12:22 24.03.2029")
(check-posix->date 2858868529 "32 Wednesday 18:08:49 04.08.2060")
(check-posix->date 8781225841 "14 Friday 13:24:01 07.04.2248")
(check-posix->date 7036395411 "51 Friday 18:16:51 21.12.2192")
(check-posix->date 5358673852 "43 Friday 16:30:52 23.10.2139")
(check-posix->date 8311463898 "20 Sunday 11:58:18 19.05.2233")
(check-posix->date 7120281600 "34 Wednesday 16:00:00 19.08.2195")
(check-posix->date 5459337257 "01 Monday 18:34:17 31.12.2142")
(check-posix->date 1885790310 "40 Thursday 06:38:30 04.10.2029")
(check-posix->date 4922466546 "52 Wednesday 23:49:06 26.12.2125")
(check-posix->date 1869013295 "12 Saturday 02:21:35 24.03.2029")
(check-posix->date 1080484167 "13 Sunday 14:29:27 28.03.2004")
(check-posix->date 8026251652 "19 Wednesday 10:20:52 05.05.2224")
(check-posix->date 4586922395 "19 Friday 09:06:35 10.05.2115")
(check-posix->date 7791370706 "47 Sunday 21:38:26 24.11.2216")
(check-posix->date 1533469193 "31 Sunday 11:39:53 05.08.2018")
(check-posix->date 7623598630 "31 Friday 02:17:10 02.08.2211")
(check-posix->date 4167492156 "04 Monday 20:42:36 23.01.2102")
(check-posix->date 8210801247 "10 Thursday 10:07:27 11.03.2230")
(check-posix->date 5190902481 "26 Tuesday 21:21:21 29.06.2134")
(check-posix->date 3932611312 "32 Saturday 08:01:52 14.08.2094")
(check-posix->date 224846623 "07 Tuesday 09:23:43 15.02.1977")
(check-posix->date 2103894844 "36 Monday 15:14:04 01.09.2036")
(check-posix->date 4603700065 "47 Wednesday 13:34:25 20.11.2115")
(check-posix->date 8747672462 "11 Tuesday 05:01:02 16.03.2247")
(check-posix->date 2070340560 "32 Friday 06:36:00 10.08.2035")
(check-posix->date 6717629434 "46 Friday 08:10:34 15.11.2182")
(check-posix->date 6734406801 "22 Wednesday 12:33:21 28.05.2183")
(check-posix->date 8496014550 "13 Monday 12:02:30 25.03.2239")
(check-posix->date 9620088079 "45 Friday 14:41:19 06.11.2274")
(check-posix->date 1013376303 "06 Sunday 21:25:03 10.02.2002")
(check-posix->date 2640766309 "36 Saturday 10:11:49 06.09.2053")
(check-posix->date 9972409749 "01 Tuesday 09:49:09 05.01.2286")
(check-posix->date 2808538688 "01 Tuesday 05:38:08 31.12.2058")
(check-posix->date 4519814883 "12 Friday 16:08:03 24.03.2113")
(check-posix->date 5996209931 "01 Saturday 13:52:11 05.01.2160")
(check-posix->date 7539713850 "48 Sunday 04:57:30 04.12.2208")
(check-posix->date 5711558749 "53 Tuesday 00:05:49 29.12.2150")



