#!/bin/sh

LISP="tmp/bare-$$.scm"
FASL="tmp/bare-$$.fasl"
C="tmp/bare-$$.c"
OBJ="tmp/bare-$$"

echo '(lambda (args) 42)' > $LISP # <- to be dumped as such, so program exits with return value 42

# check that all compile silently and produce equal code
$@ --run $LISP   # interpret and run
echo "interpret: $?"

$@ --no-threads -o $FASL $LISP
$@ --load $FASL
echo "fasl: $?"

$@ --no-threads -o $C $LISP
gcc -o $OBJ $C
$OBJ
echo "binary: $?"

rm $LISP $FASL $C $OBJ
