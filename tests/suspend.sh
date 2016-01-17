#!/bin/sh

$@ -t '(define foo "hello") (suspend "tmp/state.fasl") (print "foo: " foo)' || exit 1
$@ -l tmp/state.fasl

rm tmp/state.fasl

