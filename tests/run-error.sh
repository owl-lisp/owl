#!/bin/sh

FILE="tmp/fail-$$.scm"
FASL="tmp/fail-$$.fasl"
SOUT="tmp/fail-1-$$"
SERR="tmp/fail-2-$$"

echo '(lambda (args) (+ 1 "slartibartfast"))' > $FILE

$@ --run $FILE >$SOUT 2>$SERR

# check that nothing is leaked to stdout
test -s $SOUT && echo "junk at stdout" || echo "stdout clear"

# check that the reason for error appears to be at stdout
grep -q slartibartfast $SERR && echo "slartibartfast at stderr" || echo "stderr looks wrong"

rm $FILE $SOUT $SERR
