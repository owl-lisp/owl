#!/bin/sh

STDERR="tmp/errmsg-e2-$$"
STDOUT="tmp/errmsg-e1-$$"
$@ -e '(list 1 2 3 fiddlesticks)' 1>$STDOUT 2>$STDERR || echo 'got nonzero exit'
grep -q fiddlesticks $STDERR && echo 'found sticks'
test -s $STDOUT || echo 'stdout is blank'
rm $STDERR $STDOUT

