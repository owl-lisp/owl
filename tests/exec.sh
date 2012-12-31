#!/bin/sh

$@ -e '(import (owl sys)) (exec "/bin/echo" (list "/bin/echo" "foo")) (print "fail")'
