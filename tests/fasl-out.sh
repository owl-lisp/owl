#!/bin/sh

FILE="tmp/fasl-$$.scm"
FASL1="tmp/fasl-1-$$.fasl"
FASL2="tmp/fasl-2-$$.fasl"

# test program
echo '(lambda (args) (print (cons "kissa" (cdr args))) 42)' > $FILE

# output fasls in 2 ways
$@ -x fasl -o $FASL1 $FILE
$@ -x fasl < $FILE > $FASL2
# check that outputs are equal
diff $FASL1 $FASL2

$@ --run $FILE 11 22
echo "run exit: $?"

# check that output is just (kissa 11 22)
$@ -l $FASL1 11 22
echo "load exit: $?"

bin/vm $FASL1 11 22
echo "vm exit: $?"


rm $FILE $FASL1 $FASL2

