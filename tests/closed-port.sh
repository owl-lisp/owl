#!/bin/sh

# grep closes the port causing second print to fail, which may lead to
# unintended output and possibly infinite loop while trying to print it

$@ -e '(print "hello") (print "world")' | grep -q hello || exit 1

echo ok
