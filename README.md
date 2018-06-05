# Owl Lisp

Builds:
master [![Build Status](https://travis-ci.org/owl-lisp/owl.svg?branch=master)](https://travis-ci.org/owl-lisp/owl),
dev [![Build Status](https://travis-ci.org/owl-lisp/owl.svg?branch=develop)](https://travis-ci.org/owl-lisp/owl)

## Overview

Owl Lisp is a functional dialect of the Scheme programming language. It
is mainly based on the applicative subset of the R7RS standard.


## Requirements

You should have make and gcc or clang installed.


## Installation

To install system-wide to /usr
```
   $ make
   $ sudo make install
```

Alternatively you can try it out with
```
   $ make
   $ cp bin/ol /somewhere/convenient
   $ /somewhere/convenient/ol
   You see a prompt
   >
```

## Files

```
   bin/ol      - the owl interpreter/compiler
   c/ovm.c     - the virtual machine / shared owl lisp runtime
   owl/*.scm   - implementation of owl repl and compiler
   fasl/*.fasl - bytecode images for bin/vm used during boot
   bin/vm      - plain VM used during boot
   c/ol.c      - combined VM and REPL heap image
```

## Usage

Owl can be used either interactively, or to interpret code from files,
or compile programs to fasl-images or c-files. The difference between
an owl program and a plain script is that the program should just have
a function of one argument as the last value, which will be called with
the command line argument list when the program is executed.

In addition to being a regular interpreter, owl also tries to make it
easy to compile programs for different platforms. Owl programs can be
compiled with ol to C-files, which can be compiled to standalone binaries
without needing any owl-specific support files or libraries. The C files
also work on 32- and 64-bit systems, and compile as such at least on
Linux, OpenBSD, and macOS or can be cross-compiled to Windows executables
with MinGW.

For example, to build a hello world program:
```
  $ echo '(lambda (args) (print "Hello, world!"))' > hello.scm
  $ ol -o hello.c hello.scm
  $ gcc -o hello hello.c
  $ ./hello
  Hello, world!
```

Or simply:
```
  $ echo '(λ (args) (print "Hello, world!"))' \
     | ol -x c | gcc -x c -o hello - && ./hello
```

Parts of the compiled programs can be translated to C, instead of being
simply fasl-encoded, to increase speed. Using the --native flag compiles
most of the bytecode to C, and --usual-suspects compiles typically used
functions. To make programs run faster, one can use for example:

```
  $ ol -O2 -o test.c test.scm && gcc -O2 -o test test.c
```

## Updates and Documentation

For further documentation and updates, see:

  https://gitlab.com/owl-lisp/owl
