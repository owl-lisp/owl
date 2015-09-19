#!/bin/sh

ME=$$
HERE=$(pwd)

echo '(lambda (args) (print (foldr string-append "" (list "#!" (foldr (lambda (a b) (string-append a (string-append " " b))) "" (map (lambda (x) (string-append ' "\"$HERE/\"" ' x)) (cdr args))) "\n" "(print \"ohai\")"))))' | $@ --run - $@ > tmp/script-$ME

chmod +x tmp/script-$ME

./tmp/script-$ME | grep "^ohai$" || exit 1

rm tmp/script-$ME

