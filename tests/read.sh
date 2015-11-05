#!/bin/sh

$@ -e '(read "foo bar")' | grep "^foo$" || exit 1
$@ -e '(read-ll "foo bar")' | grep "^(foo bar)$" || exit 1
echo "    (lambda (x )   42)" | $@ -e '(read stdin)' | grep "^(lambda (x) 42)$" || exit 1

exit 0

