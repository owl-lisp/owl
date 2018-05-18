---
title: "Owl Lisp v0.2 manual"
author: Aki Helin
date: 2018-05-17
geometry: "left=3cm,right=3cm,top=2cm,bottom=2cm"
output: pdf_document
---

Owl Lisp is a simple purely functional programming language. This
document describes the language, its implementation and available
libraries. The language is essentially R7RS Scheme, apart from having
only immmutable data structures and relying on multithreading for
some core operations.

# History of Lisp

Lisp (or LISP) is short for LISt Processor. It was initially a
mathematical formalism intended for reasoning about computations. Lisp
was invented by John McCarthy at MIT in 1958. The initial description of
the language contained a definition of the semantics of the language in
terms of itself. This evaluation function was soon implemented on a
computer, turning Lisp from theory to practice.

Since then different dialects of Lisp have evolved into many different directions.
One of the main features connecting the dialects has been the syntax
   - or to be more precise, lack of it.
Most data in Lisp can be displayed textually in a simple format.
Instead of having a syntax for the programming language and one for representing this data,
   Lisp uses the same for both.
Lisp programs are just lists.

Common Lisp is one of the major current dialects.
It is used by several commercial applications.
Scheme is another modern version of Lisp.
It attempts to define a small core of constructs
   out of which many kinds of programming primitives can be built.  

Owl is essentially a small step from the multi-paradigm world of Scheme
   into a purely functional one.
Scheme programs are usually mainly functional
   with some mutations sprinkled sparingly where needed.

## Why Another Lisp?

These days compiler construction and parsing are usually taught towards
the end of computer science curriculum, if at all. This is probably
due to the complexity of modern programming languages and environments. 
Parsers and compilers, the typical building blocks of programming language
implementations, seem like dark magic only a few select pupils and
devout lone programmers are fit to approach.

This has not always been the case. In a few programming language families it
has been customary to start, not end, studies by building a small version
of the language you are studying. This approach favored
languages which had a small core of features, on top of which you could build the rest
of the system. Forth and Lisp are common examples of such languages.

The goal of Owl Lisp has not at any point been to become an ultimate Lisp and
take over the world. Ïn fact, this has been an anti-goal. The goal has
been to remain simple while incrementally growing only features required
to enable building the kinds of programs it is actually used for. While
this is a somewhat circular definition, it has worked surprisingly well.
Owl is shaped by minimalism and practical applications, not by what seem
like cool and powerful language features.


## Owl vs Scheme

Scheme is a modern lexically scoped multi-paradigm language. One of the
original goals was to also study the actor model of computation. The
actors were eventually removed, because in single threaded operation they
ended up being effectively equivalent with lambda-defined functions.

Owl takes a step back towards the actor model by allowing concurrent
execution of functions and passing messages between them. The operation
is mainly modeled after Erlang.

Another difference is in the multi-paradigm area. Owl does not try to
be able to support also imperative programming. All variable bindings
are made by lambdas, all bindings are permanent, and no data structure
can ever change. The core language is therefore closer to λ-calculus.


## Introduction

Languages such as Latin and English are called natural languages. They
have developed and evolve organically without strict rules or meanings.
It would be impossible to pin down all rules how they operate. There are
also artificial languages which do operate according to fixed rules.
The rules specify what can be considered to be a valid expression in the
language, and usually also what can be done to it without altering
the meaning. Such languages are called *formal languages*. Programming
languages belong to the latter category.

Lisp is a particular family of programming languages. A key feature of
programming languages is that you can write a program to compute anything.
Such programming languages are called *universal*. It is not difficult
to make a universal language - in fact it's quite hard not to!

The definition of a programming language can be thought to consist of two parts, *syntax* and
*semantics*. Since we typically want to write programs as text, we need
some rules to define how sequences of letters are to be interpreted as
something in the programming language. Once we are in the world of the
programming language, and not just reading a sequence of letters, we need
to attach some meaning and action to what we just read. This is the semantics
part.

The Lisp family of programming languages has a peculiar feature not typically
seen in programming languages: it is homoiconic. This means, that the syntax
of the programs is the same as the syntax used to represent data elsewhere.
This makes it extremely easy to write programs which themselves modify or
create programs.

Lisp programs can be developed either by typing the program into one or
more files and running, or by interactively working with a read-eval-print
loop (REPL). A Lisp REPL will repeatedly read one expression from the user,
evaluate expression and finally print out the textual representation of
the result.


## Simplified Core


## Common Data Types

...

## Macros

...

## Multithreading

...

## Modules


# Implementation

...

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

   bin/ol      - the owl interpreter/compiler
   c/ovm.c     - the virtual machine / shared owl lisp runtime
   owl/*.scm   - implementation of owl repl and compiler
   fasl/*.fasl - bytecode images for bin/vm used during boot
   bin/vm      - plain VM used during boot
   c/ol.c      - combined VM and REPL heap image


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

# Libraries

Libraries are named by lists of symbols. For example `(owl lazy)` is
a library name. `ol` comes preloaded with many libraries, some of which
are loaded by default to REPL. If you want to use exported definitions from a
builtin library `(owl lazy)`, you can do so by issuing `(import (owl lazy))`.

Notice that `import` is not a function. It is one of the special forms used
by the REPL to handle management of visible definitions in your sessions. The
syntax is the same as in imports within a library.

If you try to import a library which is not currently loaded, say `(my test)`,
Owl would try to look for library definition from the file "my/test.scm". If
it is available and contains definition of a library called `(my test)`, it will
be loaded and added to your REPL.

You can get a listing of the currently loaded libraries with `,libraries` command.
Many of them are mainly needed for the implementation of the underlying system.
Some of the likely useful ones are documented below.
