#!/bin/sh

# write must share and quote
$@ -t "(write (let ((foo (iota 0 1 3))) (list foo foo foo)))" | grep -q "^'(#1#=(0 1 2) #1# #1#)$" || echo bad1

# display and print shouldn't, and by extension command line eval shouldn't
$@ -e "(let ((foo (iota 0 1 3))) (list foo foo foo))" | grep -q "^((0 1 2) (0 1 2) (0 1 2))$" || echo bad2

