#!/bin/sh

ME=$$
HERE=$(pwd)

echo '(lambda (args) (print (foldr string-append "" (list "#!" (foldr (lambda (a b) (if (equal? b "") a (string-append a " " b))) "" (map (lambda (x) (string-append ' "\"$HERE/\"" ' x)) (cdr args))) "\n" "(print \"ohai\")"))))' | $@ --run - $@ > tmp/script-$ME

chmod +x tmp/script-$ME

# hashbangs have a fairly short maximum length set by kernel
test $(head -n 1 ./tmp/script-$ME | wc -c) -gt 127 && { echo ohai; exit 0; }

./tmp/script-$ME | grep "^ohai$" || exit 1

rm tmp/script-$ME
