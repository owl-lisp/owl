#!/bin/bash

set -e

make

DIR=`bin/ol --version | tr '[A-Z]' '[a-z]' | sed -e 's/ /-/g'`

echo "Target is $DIR"

test -d $DIR && rm -rf $DIR

mkdir -p $DIR/{bin,c,doc,fasl,owl,tests}
cp tests/* $DIR/tests
cp owl/*.l $DIR/owl
cp doc/*.1 $DIR/doc
cp c/ovm.c $DIR/c
cp fasl/ol.fasl $DIR/fasl/init.fasl   # copy current fixed point as the start image
cp readme.txt Makefile $DIR
tar -f - -c $DIR | gzip -9 > $DIR.tar.gz
cd $DIR
make && echo target built ok 
