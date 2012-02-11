(define-syntax check
   (syntax-rules ()
      ((check (op a b) res)
         (if (not (eq? res (op a b)))
            (begin 
               (write (list op a b))
               (print "FAIL"))))))

(check (string<? "" "")      #false)
(check (string<? "" "a")     #true)
(check (string<? "a" "")     #false)
(check (string<? "a" "a")    #false)
(check (string<? "aa" "ab")  #true)

(check (string<=? "" "")     #true)
(check (string<=? "" "a")    #true)
(check (string<=? "a" "")    #false)
(check (string<=? "a" "a")   #true)
(check (string<=? "aa" "ab") #true)

(check (string>? "" "")      #false)
(check (string>? "" "a")     #false)
(check (string>? "a" "")     #true)
(check (string>? "a" "a")    #false)
(check (string>? "aa" "ab")  #false)

(check (string>=? "" "")     #true)
(check (string>=? "" "a")    #false)
(check (string>=? "a" "")    #true)
(check (string>=? "a" "a")   #true)
(check (string>=? "aa" "ab") #false)

(check (string-ci<? "a" "A")    #false)
(check (string-ci<? "A" "a")    #false)
(check (string-ci<? "Aa" "aB")  #true)
(check (string-ci<? "aA" "Ab")  #true)

(check (string-ci<? "åäö" "ÅÄÖλ") #true)
(check (string-ci<? "ÅÄÖ" "åäöλ") #true)

(check (string-ci>? "αβγδεζηθx" "ΑΒΓΔΕΖΗΘ") #true)
(check (string-ci>? "ΑΒΓΔΕΖΗΘx" "αβγδεζηθ") #true)

(check (string-ci>=? "ΑΒΓΔΕΖΗΘ" "αβγδεζηθ") #true)
(check (string-ci<=? "ΑΒΓΔΕΖΗΘ" "αβγδεζηθ") #true)

