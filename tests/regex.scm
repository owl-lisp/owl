;;;
;;; Testing
;;;

;,r "owl/regex.l" 
; (import (owl regex)) ;; when testing without a heap rebuild

;; regex str → did-match-ok?
(define (test regex input should?)
   (cond
      ((string->regex regex) =>
         (λ (rex)
            (let ((res (rex input)))
               (cond
                  ((equal? res should?)
                     (print* (list " | '" regex "' + '" input "' = '" should? "'"))
                     #true)
                  (else
                     (print* (list " +---> '" regex "' + '" input "' = '" res "' instead of '" should? "'(FAAAAAAAAAAAIL)"))
                     #false)))))
      (else
         (print* (list " - ERROR: failed to even compile '" regex "'"))
         #false)))

;; note that slashes must be quoted in strings. no need to do that in embedded regexps.
(define regex-tests
  '(  ;; matching tests: <regex> <str> match?/replace-result
      ("m/a/" "axx" #true)
      ("m/b*/" "" #true) ;; fixme: this fails now
      ("m/b/" "axx" #false)
      ("m/./" "" #false) ;; ??
      ("m/./" "ax" #true)
      ("m/aa/" "aaxx" #true)
      ;; fixme: character classes and strings do handle \t and pals at all. also check if it is ok to support \xff and \uffff in both.
      ("m/\\t/" "	" #true) ;; todo: not entirely sure if this kind of quotations are allowed
      ("m/(a)\\1/" "aax" #true)
      ("m/a*/" "aaaaaaaaaaaaxx" #true)
      ("m/a*b*/" "aaaaaaaaaaaabbbbbbxx" #true)
      ("m/a*b*/" "bbbbbbxxx" #true)
      ("m/a*b*/" "xxx" #true)
      ("m/a*bc*/" "abx" #true)
      ("m/slartibartfast/" "slartibartfastening" #true)
      ("m/(ab)/" "abx" #true)
      ("m/(ab)*/" "abababx" #true)
      ("m/(ab)*c/" "abababcx" #true)
      ("m/^(ab)*c$/" "ababac" #false)
      ("m/(a(bc)*)*/" "aabcabcbcabcbcbcx" #true)
      ("m/a(b*)(c*)x/" "abbcccx" #true) ;; double quote needed since the first one is handled by owl
      ("m/(a*)b\\1/" "aabaax" #true)
      ("m/(a*)b\\1/" "aabaaaxe" #true)
      ("m/a|b/" "abc" #true)
      ("m/b|a/" "abc" #true)
      ("m/aa|bb/" "aabc" #true)
      ("m/bb|aa/" "aabc" #true)
      ("m/aa(bb|cc)dd/" "aabbddx" #true)
      ("m/aa(bb|cc)dd/" "aaddddx" #false)
      ("m/aa(bb|cc)dd/" "aacddx" #false)
      ("m/aa(bb|cc)dd/" "aaccddxx" #true)
      ("m/aa(b|c|d)*dd/" "aaddx" #true)
      ("m/aa(b|c|d)*dd/" "aabcdbcdbcdddx" #true)
      ("m/a+/" "ax" #true)
      ("m/a+/" "bx" #false)
      ("m/a?/" "ax" #true)
      ("m/a?/" "bx" #true)
      ("m/a?a?a?/" "aax" #true)
      ("m/^(a|bb)+$/" "bbabbabbbbaaa" #true)
      ("m/^(a|bb)+c$/" "bbabbabbbbaaac" #true)
      ("m/^(a|bb)+c$/" "bbabbabbbaaac" #false)
      ("m/^a{10}c$/" "aaaaaaaaac" #false)
      ("m/^a{10}c$/" "aaaaaaaaaac" #true)
      ("m/^a{10}c$/" "aaaaaaaaaaac" #false)
      ("m/<(.*)>.*<\\/\\1>/" "<song>trolololoo</song>x" #true)
      ("m/swap (.*) (.*) = \\2 \\1/" "swap A B = B Ax" #true)
      ("m/a{3}/" "aax" #false)
      ("m/a{3}/" "aaax" #true)
      ("m/a{3}/" "aaaax" #true)
      ("m/a{3,4}/" "aax" #false)
      ("m/a{3,4}/" "aaax" #true)
      ("m/a{3,4}/" "aaaax" #true)
      ("m/^a{3,4}c/" "aaaaacx" #false)
      ("m/^a{3,10}/" "aax" #false)
      ("m/^a{3,10}/" "aaax" #true)
      ("m/^a{3,10}/" "aaaaaaaax" #true)
      ("m/^a{3,10}/" "aaaaaaaaaax" #true)
      ("m/^a{3,10}c/" "aaaaaaaaaaacx" #false)
      ("m/(foo){3}/" "foofoofoox" #true)
      ("m/(foo|bar){3}/" "foobarfoox" #true)
      ("m/(aa|(bb){2}){3}/" "aabbbbaax" #true)
      ("m/(.*)( \\1){2} tapong/" "eki eki tapongx" #false)
      ("m/^(.*)( \\1){2} tapong$/" "eki eki eki tapong" #true)
      ("m/^(.*)( \\1){2} tapong$/" "eki eki eki eki tapong" #false)
      ;; primality testing (minor changes to a regexp taken from the internets)
      ("m/^(11+?)\\1+$/" "111" #false) ;; 3 prime
      ("m/^(11+?)\\1+$/" "1111" #true) ;; 4 composite
      ("m/^(11+?)\\1+$/" "11111" #false) ;; 5 prime
      ("m/^(11+?)\\1+$/" "111111" #true) ;; 6 composite
      ("m/^(11+?)\\1+$/" "1111111" #false) ;; 7 prime
      ("m/^(11+?)\\1+$/" "11111111" #true) ;; 8 composite
      ("m/^(11+?)\\1+$/" "111111111" #true) ;; 9 composite
      ("m/^(11+?)\\1+$/" "1111111111" #true) ;; 10 composite
      ("m/^(11+?)\\1+$/" "11111111111" #false) ;; 11 prime
      ("m/^(11+?)\\1+$/" "111111111111" #true) ;; 12 composite
      ("m/^(11+?)\\1+$/" "1111111111111111111111111111111" #false) ;; 31 prime
      ("s/^(11+?)\\1+$/\\1/" "11111111111111111111111111111111111" "11111") ;; 5 is smallest factor of 35
      ("s/^(11+)\\1+$/\\1/"  "11111111111111111111111111111111111" "1111111") ;; 7 is largest factor of 35
      ("m/(.*b(.*)) \\1 \\2/" "abc abc c" #true)
      ("m/(.*b(.*)) \\2 \\1/" "abc abc c" #false)
      ("m/(.(.(.(.))))\\1\\2\\3\\4/" "abcdabcdbcdcddx" #true)
      ("s/<.*?>//" "<foo> <bar>" " <bar>")
      ("s/<.*>//"  "<foo> <bar>" "")
      ("s/(<.*?> *)+?/X/"  "<foo> <bar>" "X<bar>")
      ("s/(<.*?> *)+?/X/g"  "<foo> <bar>" "XX")
      ("s/(<.*?> *)+/X/"  "<foo> <bar>" "X")
      ("s/(a??)(a??)(a??)x/1\\1 2\\2 3\\3/"  "aax" "1 2a 3a")
      ("m/[a]/" "ax" #true)
      ("m/[abcdef]{10}/" "abcdefabcdx" #true)
      ("m/<[λ→x.( )]*>/" "<(λx.(x x) λx.x) (λx.x λx.x)>x" #true)
      ("m/[a-w]+/" "trolololox" #true)
      ("m/[a-λ]+/" "trolololoλfoo→" #true)
      ("m/[\\t]/" "	" #true)
      ("m/^[\\\\\\t\\x61\\u03bb]+$/" "\\	aλ" #true)
      ("m/([a-z]+) ([a-z]+) tech \\1/" "dr prof tech dr provost zakharov" #true)
      ("m/[^a]bba/" "abba" #false)
      ("m/[^a]bba/" "bbba" #true)
      ("s/[^a-z][a-z]+/X/g" "foo 9bar 2low 1" "foo X X 1")
      ("s/[^a-z]//g" "a0b£!#%cde(((f))_)g" "abcdefg")
      ("m/\\./" "." #true)
      ("m/\\./" "a" #false)
      ;; ip address (ish)
      ("m/<((25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])\\.){3}(25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])>/" "<128.10.0.255>" #true)
      ("m/<((25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])\\.){3}(25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])>/" "<1.10.100.0>" #true)
      ("m/<((25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])\\.){3}(25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])>/" "<12.123.224.256<" #false)
      ("m/a\\d+b/" "a0123456789bx" #true)
      ("m/a\\d+b/" "ax0123456789bx" #false)
      ("m/a\\d+b/" "a0123456789xbx" #false)
      ("m/a\\d+b/" "a0123456789λbx" #false)
      ;; date match
      ("m/^(19|20)\\d\\d([-/.])(0?[1-9]|1[012])\\2(0?[1-9]|[12][0-9]|3[01])$/" "1999-12-01" #true) ; 
      ("m/^(19|20)\\d\\d([-/.])(0?[1-9]|1[012])\\2(0?[1-9]|[12][0-9]|3[01])$/" "2010/1/13" #true)
      ("m/^(19|20)\\d\\d([-/.])(0?[1-9]|1[012])\\2(0?[1-9]|[12][0-9]|3[01])$/" "2010.13.03" #false)
      ("m/^(19|20)\\d\\d([-/.])(0?[1-9]|1[012])\\2(0?[1-9]|[12][0-9]|3[01])$/" "2010.12/21" #false)
      ;; is a sequence of 6 letters repeated at least 3 times
      ("m/.*(.{6}).*\\1.*\\1.*/" "axqwertyogclsjgnwmqwertyfjxnsmsnchqwertyqwescd" #true)
      ("m/.*(.{6}).*\\1.*\\1.*/" "axqwertyogclsjgnwmqwertyfjxnsmsnchqwertiqwescd" #false)
      ("m/\\wx/" "ax" #true)
      ("m/\\wx/" "0x" #true)
      ("m/\\dx/" "2x" #true)
      ("m/\\Wx/" " x" #true)
      ("m/(.)\\1\\1/" "aaax" #true)
      ("m/a*$/" "aaa" #true)
      ("m/a+$/" "aaax" #false)
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
      ("m/foo(?=bar)/" "foobarbaz" #true)
      ("m/foo(?=bar)/" "foobaz" #false)
      ("s/foo(?=bar)/X/g" "foo foobar foobarber fooba" "foo Xbar Xbarber fooba")
      ("m/foo(?!bar)/" "foobarbaz" #false)
      ("m/foo(?!bar)/" "foobaz" #true)
      ("s/foo(?!bar)/X/g" "foo foobar foobarber fooba" "X foobar foobarber Xba")
      ("m/a+(?<=aa)b/" "aab" #true)
      ("m/a+(?<=aa)b/" "ab" #false)
      ("m/a+(?<!aa)b/" "ab" #true)
      ("m/^[ae]+(?<!aaa)b/" "aaeaab" #true)
      ("m/^[ae]+(?<!aaa)b/" "aaeaaab" #false)
      ("g/^fo+/" "foobar" (102 111 111))   ;; exerimental way to grab the matched area for other use
      ("m/(.)\\1/" (#x31337 #x31337) #true) ;; check that bignum back references match (not using eq? for them)
      ("m/^a{0,}/" "b" #true)
      ("m/^a{0,}/" "ab" #true)
      ("m/^a{1,}/" "b" #false)
      ("m/^a{1,}/" "ab" #true)
      ("m/^a{1,}/" "aab" #true)
      ("m/^a{2,}/" "ab" #false)
      ("m/^a{2,}/" "aab" #true)
      ("m/^a{2,}/" "aaab" #true)
      ("m/^a{3,}/" "aab" #false)
      ("m/^a{3,}/" "aaab" #true)
      ("m/^a{3,}/" "aaaab" #true)
      ("m/\\t\\t/" "\t\t" #true)
      ("c/\\t/" "a\tb\tc" ("a" "b" "c"))
      ("c/[ \\t\\n\\r]+/" "a  \tb\tc\t\r\td" ("a" "b" "c" "d"))
      ))

;; run a small battery of tests during load to check for issues
;; this being pretty experimental code still

(begin
   (print ".------------------------------------.")
   (print "| regexp tests: (all should succeed) |")
   (print "'+-----------------------------------'")
   (if (fold (λ (a t) (and a (test (car t) (cadr t) (caddr t))))
         #true regex-tests)
   (print " '--> ~~\\o\\ ALL TESTS PASSED /o/~~ ")
   (error "ONE OR MORE TEST FAILED" "NNNOOOO000"))) ;; <- aborts heap building

