;;;
;;; Testing
;;;

; ,r "lib/regex.scm" (import lib-regex) ;; when testing without a heap rebuild

;; regex str → did-match-ok?
(define (test regex input should?)
   (cond
      ((string->regex regex) =>
         (λ (rex)
            (let ((res (rex input)))
               (cond
                  ((equal? res should?)
                     (print* (list " | '" regex "' + '" input "' = '" should? "'"))
                     True)
                  (else
                     (print* (list " +---> '" regex "' + '" input "' = '" res "' instead of '" should? "'(FAAAAAAAAAAAIL)"))
                     False)))))
      (else
         (print* (list " - ERROR: failed to even compile '" regex "'"))
         False)))

;; note that slashes must be quoted in strings. no need to do that in embedded regexps.
(define regex-tests
  '(  ;; matching tests: <regex> <str> match?/replace-result
      ("/a/" "axx" T)
      ("/b*/" "" T) ;; fixme: this fails now
      ("/b/" "axx" F)
      ("/./" "" F) ;; ??
      ("/./" "ax" T)
      ("/aa/" "aaxx" T)
      ;; fixme: character classes and strings do handle \t and pals at all. also check if it is ok to support \xff and \uffff in both.
      ;("/\\t/" "	" T) ;; todo: not entirely sure if this kind of quotations are allowed
      ("/(a)\\1/" "aax" T)
      ("/a*/" "aaaaaaaaaaaaxx" T)
      ("/a*b*/" "aaaaaaaaaaaabbbbbbxx" T)
      ("/a*b*/" "bbbbbbxxx" T)
      ("/a*b*/" "xxx" T)
      ("/a*bc*/" "abx" T)
      ("/slartibartfast/" "slartibartfastening" T)
      ("/(ab)/" "abx" T)
      ("/(ab)*/" "abababx" T)
      ("/(ab)*c/" "abababcx" T)
      ("/^(ab)*c$/" "ababac" F)
      ("/(a(bc)*)*/" "aabcabcbcabcbcbcx" T)
      ("/a(b*)(c*)x/" "abbcccx" T) ;; double quote needed since the first one is handled by owl
      ("/(a*)b\\1/" "aabaax" T)
      ("/(a*)b\\1/" "aabaaaxe" T)
      ("/a|b/" "abc" T)
      ("/b|a/" "abc" T)
      ("/aa|bb/" "aabc" T)
      ("/bb|aa/" "aabc" T)
      ("/aa(bb|cc)dd/" "aabbddx" T)
      ("/aa(bb|cc)dd/" "aaddddx" F)
      ("/aa(bb|cc)dd/" "aacddx" F)
      ("/aa(bb|cc)dd/" "aaccddxx" T)
      ("/aa(b|c|d)*dd/" "aaddx" T)
      ("/aa(b|c|d)*dd/" "aabcdbcdbcdddx" T)
      ("/a+/" "ax" T)
      ("/a+/" "bx" F)
      ("/a?/" "ax" T)
      ("/a?/" "bx" T)
      ("/a?a?a?/" "aax" T)
      ("/^(a|bb)+$/" "bbabbabbbbaaa" T)
      ("/^(a|bb)+c$/" "bbabbabbbbaaac" T)
      ("/^(a|bb)+c$/" "bbabbabbbaaac" F)
      ("/^a{10}c$/" "aaaaaaaaac" F)
      ("/^a{10}c$/" "aaaaaaaaaac" T)
      ("/^a{10}c$/" "aaaaaaaaaaac" F)
      ("/<(.*)>.*<\\/\\1>/" "<song>trolololoo</song>x" T)
      ("/swap (.*) (.*) = \\2 \\1/" "swap A B = B Ax" T)
      ("/a{3}/" "aax" F)
      ("/a{3}/" "aaax" T)
      ("/a{3}/" "aaaax" T)
      ("/a{3,4}/" "aax" F)
      ("/a{3,4}/" "aaax" T)
      ("/a{3,4}/" "aaaax" T)
      ("/^a{3,4}c/" "aaaaacx" F)
      ("/^a{3,10}/" "aax" F)
      ("/^a{3,10}/" "aaax" T)
      ("/^a{3,10}/" "aaaaaaaax" T)
      ("/^a{3,10}/" "aaaaaaaaaax" T)
      ("/^a{3,10}c/" "aaaaaaaaaaacx" F)
      ("/(foo){3}/" "foofoofoox" T)
      ("/(foo|bar){3}/" "foobarfoox" T)
      ("/(aa|(bb){2}){3}/" "aabbbbaax" T)
      ("/(.*)( \\1){2} tapong/" "eki eki tapongx" F)
      ("/^(.*)( \\1){2} tapong$/" "eki eki eki tapong" T)
      ("/^(.*)( \\1){2} tapong$/" "eki eki eki eki tapong" F)
      ;; primality testing (minor changes to a regexp taken from the internets)
      ("/^(11+?)\\1+$/" "111" F) ;; 3 prime
      ("/^(11+?)\\1+$/" "1111" T) ;; 4 composite
      ("/^(11+?)\\1+$/" "11111" F) ;; 5 prime
      ("/^(11+?)\\1+$/" "111111" T) ;; 6 composite
      ("/^(11+?)\\1+$/" "1111111" F) ;; 7 prime
      ("/^(11+?)\\1+$/" "11111111" T) ;; 8 composite
      ("/^(11+?)\\1+$/" "111111111" T) ;; 9 composite
      ("/^(11+?)\\1+$/" "1111111111" T) ;; 10 composite
      ("/^(11+?)\\1+$/" "11111111111" F) ;; 11 prime
      ("/^(11+?)\\1+$/" "111111111111" T) ;; 12 composite
      ("/^(11+?)\\1+$/" "1111111111111111111111111111111" F) ;; 31 prime
      ("s/^(11+?)\\1+$/\\1/" "11111111111111111111111111111111111" "11111") ;; 5 is smallest factor of 35
      ("s/^(11+)\\1+$/\\1/"  "11111111111111111111111111111111111" "1111111") ;; 7 is largest factor of 35
      ("/(.*b(.*)) \\1 \\2/" "abc abc c" T)
      ("/(.*b(.*)) \\2 \\1/" "abc abc c" F)
      ("/(.(.(.(.))))\\1\\2\\3\\4/" "abcdabcdbcdcddx" T)
      ("s/<.*?>//" "<foo> <bar>" " <bar>")
      ("s/<.*>//"  "<foo> <bar>" "")
      ("s/(<.*?> *)+?/X/"  "<foo> <bar>" "X<bar>")
      ("s/(<.*?> *)+?/X/g"  "<foo> <bar>" "XX")
      ("s/(<.*?> *)+/X/"  "<foo> <bar>" "X")
      ("s/(a??)(a??)(a??)x/1\\1 2\\2 3\\3/"  "aax" "1 2a 3a")
      ("/[a]/" "ax" T)
      ("/[abcdef]{10}/" "abcdefabcdx" T)
      ("/<[λ→x.( )]*>/" "<(λx.(x x) λx.x) (λx.x λx.x)>x" T)
      ("/[a-w]+/" "trolololox" T)
      ("/[a-λ]+/" "trolololoλfoo→" T)
      ("/[\\t]/" "	" T)
      ("/^[\\\\\\t\\x61\\u03bb]+$/" "\\	aλ" T)
      ("/([a-z]+) ([a-z]+) tech \\1/" "dr prof tech dr provost zakharov" T)
      ("/[^a]bba/" "abba" F)
      ("/[^a]bba/" "bbba" T)
      ("s/[^a-z][a-z]+/X/g" "foo 9bar 2low 1" "foo X X 1")
      ("s/[^a-z]//g" "a0b£!#%cde(((f))_)g" "abcdefg")
      ("/\\./" "." T)
      ("/\\./" "a" F)
      ;; ip address (ish)
      ("/<((25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])\\.){3}(25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])>/" "<128.10.0.255>" T)
      ("/<((25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])\\.){3}(25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])>/" "<1.10.100.0>" T)
      ("/<((25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])\\.){3}(25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])>/" "<12.123.224.256<" F)
      ("/a\\d+b/" "a0123456789bx" T)
      ("/a\\d+b/" "a\/0123456789bx" F)
      ("/a\\d+b/" "a0123456789\:bx" F)
      ("/a\\d+b/" "a0123456789λbx" F)
      ;; date match
      ("/^(19|20)\\d\\d([-/.])(0?[1-9]|1[012])\\2(0?[1-9]|[12][0-9]|3[01])$/" "1999-12-01" T) ; 
      ("/^(19|20)\\d\\d([-/.])(0?[1-9]|1[012])\\2(0?[1-9]|[12][0-9]|3[01])$/" "2010/1/13" T)
      ("/^(19|20)\\d\\d([-/.])(0?[1-9]|1[012])\\2(0?[1-9]|[12][0-9]|3[01])$/" "2010.13.03" F)
      ("/^(19|20)\\d\\d([-/.])(0?[1-9]|1[012])\\2(0?[1-9]|[12][0-9]|3[01])$/" "2010.12/21" F)
      ;; is a sequence of 6 letters repeated at least 3 times
      ("/.*(.{6}).*\\1.*\\1.*/" "axqwertyogclsjgnwmqwertyfjxnsmsnchqwertyqwescd" T)
      ("/.*(.{6}).*\\1.*\\1.*/" "axqwertyogclsjgnwmqwertyfjxnsmsnchqwertiqwescd" F)
      ("/\\wx/" "ax" T)
      ("/\\wx/" "0x" T)
      ("/\\dx/" "2x" T)
      ("/\\Wx/" " x" T)
      ("/(.)\\1\\1/" "aaax" T)
      ("/a*$/" "aaa" T)
      ("/a+$/" "aaax" F)
      ;;; replace tests: <s/<regex>/pat/> <input> <output>
      ("s/(..)\\1/<\\0>/g" "trololo lalalaa" "tr<olol>o <lala>laa")
      ("s/<.*?>/xxx/g" "<foo> <bar>" "xxx xxx")
      ("s/<.*>/xxx/"  "<foo> <bar>" "xxx")
      ("s/(\\w+) (\\w+)/\\2 \\1/g" "it was a fine day" "was it fine a day")
      ("s/(.)*/\\1/" "abc" "c")
      ;("s/(.)+/\\1/" "abc" "c")
      ("s/((\\w+) (\\w+) )*/\\2 \\3 /" "it was a fine day" "a fine day")
      ("s/(..)(?:.*)(.)/\\1\\2/" "quel chance" "que")
      ("c/foo/" "franzfoolisp" ("franz" "lisp"))
      ("c/(\\d)\\1/" "121121221121221" ("12" "21" "" "21" "1"))
      ("c/λ/" "fooλbarλbaz" ("foo" "bar" "baz"))
      ;("c/^foo/" "foofoo" ("" "foo")) ; <- does not work yet
      ("/foo(?=bar)/" "foobarbaz" T)
      ("/foo(?=bar)/" "foobaz" F)
      ("s/foo(?=bar)/X/g" "foo foobar foobarber fooba" "foo Xbar Xbarber fooba")
      ("/foo(?!bar)/" "foobarbaz" F)
      ("/foo(?!bar)/" "foobaz" T)
      ("s/foo(?!bar)/X/g" "foo foobar foobarber fooba" "X foobar foobarber Xba")
      ("/a+(?<=aa)b/" "aab" T)
      ("/a+(?<=aa)b/" "ab" F)
      ("/a+(?<!aa)b/" "ab" T)
      ("/^[ae]+(?<!aaa)b/" "aaeaab" T)
      ("/^[ae]+(?<!aaa)b/" "aaeaaab" F)
      ("m/^fo+/" "foobar" (102 111 111))   ;; exerimental way to grab the matched area for other use
      ))

;; run a small battery of tests during load to check for issues
;; this being pretty experimental code still

(begin
   (print ".------------------------------------.")
   (print "| regexp tests: (all should succeed) |")
   (print "'+-----------------------------------'")
   (if (fold (λ (a t) (and a (test (car t) (cadr t) (caddr t))))
         True regex-tests)
   (print " '--> ~~\\o\\ ALL TESTS PASSED /o/~~ ")
   (error "ONE OR MORE TEST FAILED" "NNNOOOO000"))) ;; <- aborts heap building

