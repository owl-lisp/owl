#!/bin/sh

$@ -e "(time-ms)" | sed -e 's/[0-9]/a/g'
