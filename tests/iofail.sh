#!/bin/sh

# output errors in typical shell script use are errors 
$@ -e '(+ 40 2)' >/dev/full || echo ok 1

# output errors in interactive mode are fatal
$@ >/dev/full || echo ok 2

