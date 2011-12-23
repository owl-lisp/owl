(define (check val wanted)
   (if (not (eq? val wanted))
      (display "boo!")))

(check (string<? "" "")      False)
(check (string<? "" "a")     True)
(check (string<? "a" "")     False)
(check (string<? "a" "a")    False)
(check (string<? "aa" "ab")  True)

(check (string<=? "" "")     False)
(check (string<=? "" "a")    True)
(check (string<=? "a" "")    False)
(check (string<=? "a" "a")   True)
(check (string<=? "aa" "ab") True)

(check (string>? "" "")      False)
(check (string>? "" "a")     False)
(check (string>? "a" "")     True)
(check (string>? "a" "a")    False)
(check (string>? "aa" "ab")  False)

(check (string>=? "" "")     True)
(check (string>=? "" "a")    False)
(check (string>=? "a" "")    True)
(check (string>=? "a" "a")   True)
(check (string>=? "aa" "ab") False)

(check (string-ci<? "a" "A")    False)
(check (string-ci<? "A" "a")    False)
(check (string-ci<? "Aa" "aB")  True)
(check (string-ci<? "aA" "Ab")  True)

(check (string-ci<? "åäö" "ÅÄÖλ") True)
(check (string-ci<? "ÅÄÖ" "åäöλ") True)

(check (string-ci>? "αβγδεζηθx" "ΑΒΓΔΕΖΗΘ") True)
(check (string-ci>? "ΑΒΓΔΕΖΗΘx" "αβγδεζηθ") True)

(check (string-ci>=? "ΑΒΓΔΕΖΗΘ" "αβγδεζηθ") True)
(check (string-ci<=? "ΑΒΓΔΕΖΗΘ" "αβγδεζηθ") True)

