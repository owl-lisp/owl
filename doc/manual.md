---
title: "Owl Lisp v0.2 manual"
author: Aki Helin
date: 19.2.2018
geometry: "left=3cm,right=3cm,top=2cm,bottom=2cm"
output: pdf_document
---

# Owl Lisp

Owl Lisp is a simple programming language. This document describes 
the language, available libraries and implementation issues. Owl is
essentially R7RS version of Scheme with multithreading and no mutable 
data structures. This is a standalone document, so no prior knowledge 
of Scheme or similar languages is required. 


## Why another Lisp?

  * History, advanced compiler course vs toy languages

  * Lisp, different 

  * Forth etc
 
  * Standalone complex

  * Goals


## Status

Owl is currently useful for writing some relatively small programs. It 
is easy to get up and running in various environments due to very few 
requirements, and it can be used to make standalone programs easily in 
any environment in which it itself can run. However, lack of foreign 
function interfaces, poor error messages and lack of documentation 
likely make it difficult to use for many purposes. This document attempts 
to fix the last issue.


  
## Language reference

As with Scheme, the guiding principle of Owl is to strive for simplicity 
and elegance...



### Requirements

You should have make and gcc or clang installed. 


### Installation

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


### Files

   bin/ol      - the owl interpreter/compiler
   c/ovm.c     - the virtual machine / shared owl lisp runtime
   owl/*.scm   - implementation of owl repl and compiler
   bench/*.scm - some benchmarks
   fasl/*.fasl - bytecode images for bin/vm used during boot
   bin/vm      - plain VM used during boot
   c/ol.c      - combined VM and REPL heap image


### Usage

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
Linux, OpenBSD, OSX and can be crosscompiled to Windows executables with 
MinGW.

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

# Libraries

Libraries are named by lists of symbols. For example `(owl lazy)` is 
a library name. `ol` comes prelodaded with many libraries, some of which 
are loaded by default to REPL. If you want to use exported definitions from a 
builtin library `(owl lazy)`, you can do so by issuing `(import (owl lazy))`.

Notice that `import` is not a function. It is one of the special forms used 
by the REPL to handle management of visible definitions in your sessions. The 
syntax is the same as in imports within a library.

If you try to import a library which is not currently loaded, say `(my test)`, 
Owl would try to look for library definition from the file "my/test.scm". If 
it is available and contains definition of a libary called `(my test)`, it will 
be loaded and added to your repl.

You can get a listing of the currently loaded libraries with `,libraries` command.
Many of them are mainly needed for the implementation of the underlying system. 
Some of the likely useful ones are documented below.

