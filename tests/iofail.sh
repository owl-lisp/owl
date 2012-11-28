#!/bin/sh

test -f /dev/full || exit 0 # doesn't exist on some systems

# output errors in typical shell script use are errors 
$@ -e '(+ 40 2)' >/dev/full && echo bad 1

# output errors in interactive mode are fatal
$@ >/dev/full && echo bad 2

