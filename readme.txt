Owl Lisp -- yet another pet lisp 
Copyright (c) 2008-2011 Aki Helin

OVERVIEW

   Owl Lisp is a simple purely functional Lisp system. It is mainly
   an implementation of the purely functional subset of R5RS Scheme,
   has some extra support for purely functional data structures (like a
   complete order for objects), and an implementation which can be used
   to run and compile programs fairly portably and efficiently without
   needing nonstandard support libraries.


REQUIREMENTS

You should have make and gcc installed. 


INSTALLATION

Short version:

   $ make install

Altervative version:

   $ make
   [...]
   $ cp bin/ol $HOME/bin/ol
   $ ol
   You see a prompt.
   > (+ 1 2)
   3
   > ^D
   bye bye _o/~

Ol is the standalone owl read-eval-print loop and compiler. It can be
used to rebuild owl, interpret files, evaluate terms interactively or
compile programs to fasl images or C files, which can then be compiled
with a C-compiler to standalone programs.


FILES

   c/ovm.c     - the virtual machine / shared owl lisp runtime
   lib/*.scm   - some libraries required to build owl
   bench/*.scm - some benchmarks
   bin/ol      - the owl interpreter


USAGE

Owl can be used either interactively, or interpret code from files,
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
  $ echo '(lambda (args) (print "Hello, world!"))' > hello.scm
  $ ol -o hello.c hello.scm
  $ gcc -o hello hello.c
  $ ./hello
  Hello, world!

Or simply:
  $ echo '(λ (args) (print "Hello, world!"))' \
     | ol -x c | gcc -x c -o hello - && ./hello

Parts of the compiled programs can be translated to C, instead of being 
simply fasl-encoded, to increase speed. Using the --native flag compiles 
most of the bytecode to C, and --usual-suspects compiles typically used 
functions. To make programs run faster, one can use for example:

  $ ol --native -o test.c test.scm && gcc -O2 -o test test.c


DOCUMENTATION

For further documentation, see:

  http://code.google.com/p/owl-lisp/wiki/OwlManual 

The source code repository is at:
   
   https://github.com/aoh/owl-lisp

