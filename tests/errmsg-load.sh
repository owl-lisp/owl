#!/bin/sh

BADFILE="tmp/errmsg-load-$$.scm"
STDERR="tmp/errmsg-load2-$$"
STDOUT="tmp/errmsg-load1-$$"
echo '(list 1 2 3 slartibartfast)' > $BADFILE
$@ $BADFILE 1>$STDOUT 2>$STDERR || echo 'got nonzero exit'
grep -q $BADFILE $STDERR && echo 'found file' || exit 1
grep -q slartibartfast $STDERR && echo 'found slartibartfast' || exit 2
test -s $STDOUT || echo 'stdout is blank' || exit 3
rm $STDERR $STDOUT $BADFILE

