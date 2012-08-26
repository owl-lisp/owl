#!/bin/sh

ulimit -S -t 2

$@ -e '(print "hello") (print "world")' | grep -q hello || exit 1

echo ok

