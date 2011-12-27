;;;
;;; Testing
;;;

;,r "owl/regex.l" 
;(import lib-regex) ;; when testing without a heap rebuild

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
      ("m/a/" "axx" T)
      ("m/b*/" "" T) ;; fixme: this fails now
      ("m/b/" "axx" F)
      ("m/./" "" F) ;; ??
      ("m/./" "ax" T)
      ("m/aa/" "aaxx" T)
      ;; fixme: character classes and strings do handle \t and pals at all. also check if it is ok to support \xff and \uffff in both.
      ;("m/\\t/" "	" T) ;; todo: not entirely sure if this kind of quotations are allowed
      ("m/(a)\\1/" "aax" T)
      ("m/a*/" "aaaaaaaaaaaaxx" T)
      ("m/a*b*/" "aaaaaaaaaaaabbbbbbxx" T)
      ("m/a*b*/" "bbbbbbxxx" T)
      ("m/a*b*/" "xxx" T)
      ("m/a*bc*/" "abx" T)
      ("m/slartibartfast/" "slartibartfastening" T)
      ("m/(ab)/" "abx" T)
      ("m/(ab)*/" "abababx" T)
      ("m/(ab)*c/" "abababcx" T)
      ("m/^(ab)*c$/" "ababac" F)
      ("m/(a(bc)*)*/" "aabcabcbcabcbcbcx" T)
      ("m/a(b*)(c*)x/" "abbcccx" T) ;; double quote needed since the first one is handled by owl
      ("m/(a*)b\\1/" "aabaax" T)
      ("m/(a*)b\\1/" "aabaaaxe" T)
      ("m/a|b/" "abc" T)
      ("m/b|a/" "abc" T)
      ("m/aa|bb/" "aabc" T)
      ("m/bb|aa/" "aabc" T)
      ("m/aa(bb|cc)dd/" "aabbddx" T)
      ("m/aa(bb|cc)dd/" "aaddddx" F)
      ("m/aa(bb|cc)dd/" "aacddx" F)
      ("m/aa(bb|cc)dd/" "aaccddxx" T)
      ("m/aa(b|c|d)*dd/" "aaddx" T)
      ("m/aa(b|c|d)*dd/" "aabcdbcdbcdddx" T)
      ("m/a+/" "ax" T)
      ("m/a+/" "bx" F)
      ("m/a?/" "ax" T)
      ("m/a?/" "bx" T)
      ("m/a?a?a?/" "aax" T)
      ("m/^(a|bb)+$/" "bbabbabbbbaaa" T)
      ("m/^(a|bb)+c$/" "bbabbabbbbaaac" T)
      ("m/^(a|bb)+c$/" "bbabbabbbaaac" F)
      ("m/^a{10}c$/" "aaaaaaaaac" F)
      ("m/^a{10}c$/" "aaaaaaaaaac" T)
      ("m/^a{10}c$/" "aaaaaaaaaaac" F)
      ("m/<(.*)>.*<\\/\\1>/" "<song>trolololoo</song>x" T)
      ("m/swap (.*) (.*) = \\2 \\1/" "swap A B = B Ax" T)
      ("m/a{3}/" "aax" F)
      ("m/a{3}/" "aaax" T)
      ("m/a{3}/" "aaaax" T)
      ("m/a{3,4}/" "aax" F)
      ("m/a{3,4}/" "aaax" T)
      ("m/a{3,4}/" "aaaax" T)
      ("m/^a{3,4}c/" "aaaaacx" F)
      ("m/^a{3,10}/" "aax" F)
      ("m/^a{3,10}/" "aaax" T)
      ("m/^a{3,10}/" "aaaaaaaax" T)
      ("m/^a{3,10}/" "aaaaaaaaaax" T)
      ("m/^a{3,10}c/" "aaaaaaaaaaacx" F)
      ("m/(foo){3}/" "foofoofoox" T)
      ("m/(foo|bar){3}/" "foobarfoox" T)
      ("m/(aa|(bb){2}){3}/" "aabbbbaax" T)
      ("m/(.*)( \\1){2} tapong/" "eki eki tapongx" F)
      ("m/^(.*)( \\1){2} tapong$/" "eki eki eki tapong" T)
      ("m/^(.*)( \\1){2} tapong$/" "eki eki eki eki tapong" F)
      ;; primality testing (minor changes to a regexp taken from the internets)
      ("m/^(11+?)\\1+$/" "111" F) ;; 3 prime
      ("m/^(11+?)\\1+$/" "1111" T) ;; 4 composite
      ("m/^(11+?)\\1+$/" "11111" F) ;; 5 prime
      ("m/^(11+?)\\1+$/" "111111" T) ;; 6 composite
      ("m/^(11+?)\\1+$/" "1111111" F) ;; 7 prime
      ("m/^(11+?)\\1+$/" "11111111" T) ;; 8 composite
      ("m/^(11+?)\\1+$/" "111111111" T) ;; 9 composite
      ("m/^(11+?)\\1+$/" "1111111111" T) ;; 10 composite
      ("m/^(11+?)\\1+$/" "11111111111" F) ;; 11 prime
      ("m/^(11+?)\\1+$/" "111111111111" T) ;; 12 composite
      ("m/^(11+?)\\1+$/" "1111111111111111111111111111111" F) ;; 31 prime
      ("s/^(11+?)\\1+$/\\1/" "11111111111111111111111111111111111" "11111") ;; 5 is smallest factor of 35
      ("s/^(11+)\\1+$/\\1/"  "11111111111111111111111111111111111" "1111111") ;; 7 is largest factor of 35
      ("m/(.*b(.*)) \\1 \\2/" "abc abc c" T)
      ("m/(.*b(.*)) \\2 \\1/" "abc abc c" F)
      ("m/(.(.(.(.))))\\1\\2\\3\\4/" "abcdabcdbcdcddx" T)
      ("s/<.*?>//" "<foo> <bar>" " <bar>")
      ("s/<.*>//"  "<foo> <bar>" "")
      ("s/(<.*?> *)+?/X/"  "<foo> <bar>" "X<bar>")
      ("s/(<.*?> *)+?/X/g"  "<foo> <bar>" "XX")
      ("s/(<.*?> *)+/X/"  "<foo> <bar>" "X")
      ("s/(a??)(a??)(a??)x/1\\1 2\\2 3\\3/"  "aax" "1 2a 3a")
      ("m/[a]/" "ax" T)
      ("m/[abcdef]{10}/" "abcdefabcdx" T)
      ("m/<[λ→x.( )]*>/" "<(λx.(x x) λx.x) (λx.x λx.x)>x" T)
      ("m/[a-w]+/" "trolololox" T)
      ("m/[a-λ]+/" "trolololoλfoo→" T)
      ("m/[\\t]/" "	" T)
      ("m/^[\\\\\\t\\x61\\u03bb]+$/" "\\	aλ" T)
      ("m/([a-z]+) ([a-z]+) tech \\1/" "dr prof tech dr provost zakharov" T)
      ("m/[^a]bba/" "abba" F)
      ("m/[^a]bba/" "bbba" T)
      ("s/[^a-z][a-z]+/X/g" "foo 9bar 2low 1" "foo X X 1")
      ("s/[^a-z]//g" "a0b£!#%cde(((f))_)g" "abcdefg")
      ("m/\\./" "." T)
      ("m/\\./" "a" F)
      ;; ip address (ish)
      ("m/<((25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])\\.){3}(25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])>/" "<128.10.0.255>" T)
      ("m/<((25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])\\.){3}(25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])>/" "<1.10.100.0>" T)
      ("m/<((25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])\\.){3}(25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])>/" "<12.123.224.256<" F)
      ("m/a\\d+b/" "a0123456789bx" T)
      ("m/a\\d+b/" "a\/0123456789bx" F)
      ("m/a\\d+b/" "a0123456789\:bx" F)
      ("m/a\\d+b/" "a0123456789λbx" F)
      ;; date match
      ("m/^(19|20)\\d\\d([-/.])(0?[1-9]|1[012])\\2(0?[1-9]|[12][0-9]|3[01])$/" "1999-12-01" T) ; 
      ("m/^(19|20)\\d\\d([-/.])(0?[1-9]|1[012])\\2(0?[1-9]|[12][0-9]|3[01])$/" "2010/1/13" T)
      ("m/^(19|20)\\d\\d([-/.])(0?[1-9]|1[012])\\2(0?[1-9]|[12][0-9]|3[01])$/" "2010.13.03" F)
      ("m/^(19|20)\\d\\d([-/.])(0?[1-9]|1[012])\\2(0?[1-9]|[12][0-9]|3[01])$/" "2010.12/21" F)
      ;; is a sequence of 6 letters repeated at least 3 times
      ("m/.*(.{6}).*\\1.*\\1.*/" "axqwertyogclsjgnwmqwertyfjxnsmsnchqwertyqwescd" T)
      ("m/.*(.{6}).*\\1.*\\1.*/" "axqwertyogclsjgnwmqwertyfjxnsmsnchqwertiqwescd" F)
      ("m/\\wx/" "ax" T)
      ("m/\\wx/" "0x" T)
      ("m/\\dx/" "2x" T)
      ("m/\\Wx/" " x" T)
      ("m/(.)\\1\\1/" "aaax" T)
      ("m/a*$/" "aaa" T)
      ("m/a+$/" "aaax" F)
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
      ("m/foo(?=bar)/" "foobarbaz" T)
      ("m/foo(?=bar)/" "foobaz" F)
      ("s/foo(?=bar)/X/g" "foo foobar foobarber fooba" "foo Xbar Xbarber fooba")
      ("m/foo(?!bar)/" "foobarbaz" F)
      ("m/foo(?!bar)/" "foobaz" T)
      ("s/foo(?!bar)/X/g" "foo foobar foobarber fooba" "X foobar foobarber Xba")
      ("m/a+(?<=aa)b/" "aab" T)
      ("m/a+(?<=aa)b/" "ab" F)
      ("m/a+(?<!aa)b/" "ab" T)
      ("m/^[ae]+(?<!aaa)b/" "aaeaab" T)
      ("m/^[ae]+(?<!aaa)b/" "aaeaaab" F)
      ("g/^fo+/" "foobar" (102 111 111))   ;; exerimental way to grab the matched area for other use
      ("m/(.)\\1/" (#x31337 #x31337) T) ;; check that bignum back references match (not using eq? for them)
      ("m/^a{0,}/" "b" T)
      ("m/^a{0,}/" "ab" T)
      ("m/^a{1,}/" "b" F)
      ("m/^a{1,}/" "ab" T)
      ("m/^a{1,}/" "aab" T)
      ("m/^a{2,}/" "ab" F)
      ("m/^a{2,}/" "aab" T)
      ("m/^a{2,}/" "aaab" T)
      ("m/^a{3,}/" "aab" F)
      ("m/^a{3,}/" "aaab" T)
      ("m/^a{3,}/" "aaaab" T)
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

