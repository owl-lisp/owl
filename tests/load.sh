#!/bin/sh

FILE="tmp/test-load-$$.scm"
echo '(print "kissa")' > $FILE
$@ $FILE
rm $FILE

