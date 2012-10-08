#!/bin/sh

$@ -t '"HELLO2"'
test $? = 0 || echo bad1

$@ -t '(= 1 2)'
test $? = 1 || echo bad2

$@ -t ')lambda(' 2>/dev/null # error should go to stderr
test $? = 127 || echo bad3

$@ -t '(/ 1 0)'  2>/dev/null # ditto
test $? = 127 || echo bad4

echo ok
