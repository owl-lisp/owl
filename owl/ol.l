;;;
;;; ol.scm: an Owl read-eval-print loop.
;;;

;;
;; Copyright (c) 2008-2011 Aki Helin
;; 
;; Permission is hereby granted, free of charge, to any person obtaining a 
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions
;; 
;; The above copyright notice and this permission notice shall be included 
;; in all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
;; DEALINGS IN THE SOFTWARE.
;;

;; todo: missing optimization: convert all evaluated closures to procs (doable but a bit of work..)
;; todo: having a constant-time (major) type dispatch opcode would help with generic routines
;; todo: there is no unit test for simultaneous async and sync mailing between threads
;; bug: captured threads are not shown properly in thread listing

;; todo: remove some manually added C-code primops and see how much slower the equivalents are in compiled bytecode
;; bug: ovm.c gc trigger off until the upper bound is enforced at compile time
;; todo: send repl errors to stderr (cat foo.scm | owl -c - | gcc -x c -o ..)
;; todo: add sources and binding meta-information to modules 
;; todo: accept name and linkage as arguments to fork, not via separate syscalls
;; todo: thread accepts should be the thread ids, not predicates
;; todo: inter-mcp communications (with current lib-fasl) 
;; todo: automatic constructor, binder and tester generation
;; todo: typed binds, typed ref and pattern matching
;; todo: inline with a partial evaluator (where to keep the sources and from which language level)
;; todo: False should not be also Empty
;; todo: rethink threads (tree structured computations seem much nicer)
;; todo: sane register allocator and closure converter
;; todo: port leaf prototype theorem prover later?


;; todo: owl is starting to need support for compilation of individual modules. requiring one file to have one library and map the libraries to paths would make this easy, but it feels stupid and wrong. there are two tempting options (having owl fork() processes to load requirements, mark them as loading in requirements, and block to receive the fasl when it is necessary) or allow compiling the modules to .fasl:s and have a Makefile to make just the required recompilation.
;; todo: extend the default loader to also handle fasls
;; todo: allow both $ ol -o foo.[fasl|c] bar.[ol|fasl]

,r "owl/primop.l"

;; todo: cut some of these out later
;; todo: convert forget-all-but to drop also primops after lib-primop is done

,forget-all-but (*vm-special-ops* *codes* wait *args* stdin stdout stderr set-ticker run lib-primop) ; wait needed in lib-parse atm

(define *loaded* '("owl/primop.l")) ;; avoid reloading it
(import lib-primop) ;; grab freshly defined primops 

;; common things using primops
(define-module lib-base

   (export raw?)

   (define (raw? obj) (eq? (fxband (type obj) #b100000000110) #b100000000110))

)

(import lib-base)


;; set a few flags which affect compilation or set static information
(define *owl-version* "0.1.2")
;(define *interactive* True) ;; causes file names to be printed when loading them

; The symbol _sans_cps acts as a quote to the CPS transformer. This 
; allows things like call/cc to be defined as library functions.

(define call/cc 
   ('_sans_cps (λ (c f) (f c (λ (r v) (c v))))))

(define call/cc2
   ('_sans_cps (λ (c f) (f c (λ (r a b) (c a b))))))

(define (i x) x)
(define (k x y) x)


;;; syscalls (interacting with the underlying thread controller implemented in lib/threads.scm and lib/mcp.scm)

,r "owl/syscall.l"

(import lib-syscall)


;;; rendering 

;; render unknown objects as <???>
(define (render self obj tl)
   (ilist 60 63 63 63 62 tl))

(define (halt n)
   (sys-prim 6 n n n))

(define (seccomp)
   (sys-prim 10 F F F))



(define-module lib-boolean

   (export boolean? render)

   (define (boolean? x) 
      (cond
         ((eq? x True) True)
         ((eq? x False) True)
         (else False)))

   (define render 
		(lambda (self obj tail)
			(cond
				((eq? obj True)  (ilist 84 114 117 101 tail))
				((eq? obj False) (ilist 70 97 108 115 101 tail))
				(else (render self obj tail))))))

(import lib-boolean)


;; todo: move these also to corresponding libraries

(define (char? x) (eq? (type x) 26))

(define max-object-size #xffff)


;;; Misc

(define (not x) 
   (if x False True))

(define o (λ f g (λ x (f (g x)))))

(define i (λ x x))

(define self i)

;;; Lists

;; pure list stuff not depending on math or other external things

,r "owl/list.l"

(import lib-list)

;;;
;;; Finite functions
;;;

,require "owl/ff.l"
(import lib-ff)


;;; integer stores, include in toplevel 

,r "owl/iff.l"


;;;
;;; Math
;;;

,r "owl/math.l"   ; basic arithmetic
(import lib-math) 

,r "owl/list-extra.l"   ; list ops requiring basic math (length ...)
(import lib-list-extra)

,r "owl/math-extra.l"   ; less basic math (factor, ...)
(import lib-math-extra)

,r "owl/lazy.l"   ; things computed as needed (remember, no caching, these are not ok for dp)
(import lib-lazy)


;;;
;;; Sorting
;;;

,r "owl/sort.l"

(import lib-sort)


;;;
;;; Strings
;;;

,r "owl/string.l"

(import lib-string)

(define (fopen path mode)
   (syscall 7 (c-string path) mode))

;; fixme: system-X do not belong here
(define (system-print str)
   (sys-prim 0 1 str (sizeb str)))

(define (system-println str)
   (system-print str)
   (system-print "
"))

(define (system-stderr str) ; <- str is a raw or pre-rendered string
   (sys-prim 0 2 str (sizeb str)))


;;;
;;; Vectors
;;;

,r "owl/vector.l"

(import lib-vector)


;;;
;;; Symbols
;;;


(define-module lib-symbol

   (export symbol? symbol->string render)

   (define (symbol? x) (eq? (type x) 38))

   (define (symbol->string x) (ref x 1))

   (define render
      (lambda (self obj tl)
         (if (symbol? obj)
            (self self (symbol->string obj) tl)
            (render self obj tl)))))

(import lib-symbol)


;;;
;;; Equality
;;;

(define (eq-fields a b eq pos)
   (cond
      ((eq? pos 0)
         True)
      ((eq (ref a pos) (ref b pos))
         (lets ((pos x (fx- pos 1)))
            (eq-fields a b eq pos)))
      (else False)))

(define (eq-bytes a b pos)
   (if (eq? (refb a pos) (refb b pos))
      (if (eq? pos 0)
         True
         (receive (fx- pos 1)
            (λ pos x (eq-bytes a b pos))))
      False))

;; fixme: ff:s should have a separate equality test too
;; fixme: byte vector paddings not here

;; raw brute force object equality
(define (equal? a b)
   (cond
      ((eq? a b)
         True)
      ((string? a)
         (and (string? b) (string-eq? a b)))
      ((symbol? a) False) ; would have been eq?, because they are interned
      ((pair? a)
         (if (pair? b)
            (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))
            False))
      (else
         (let ((sa (size a)))
            (cond
               ; a is immediate -> would have been eq?
               ((eq? sa 0) 
                  False)
               ; same size
               ((eq? sa (size b))
                  (let ((ta (type a)))
                     ; check equal types
                     (if (eq? ta (type b))
                        (if (eq? (fxband ta 2048) 0)
                           ; equal ntuples, check fields
                           (eq-fields a b equal? sa)
                           ; equal raw objects, check bytes
                           (lets
                              ((ea (sizeb a)) ; raw objects may have padding bytes, so recheck the sizes
                           (eb (sizeb b)))
                              (if (eq? ea eb)
                                 (if (eq? ea 0)
                                    True
                                    (eq-bytes a b (- ea 1)))
                                 False)))
                        False)))
               (else False))))))

;;;
;;; Random access lists
;;;

,r "owl/rlist.l"

(import lib-rlist)


;;;
;;; Generic versions of the universal common language words
;;;

,r "owl/generic.l"

(define ≡ equal?)

; once upon a time
;(define (equal? a b)
;   (cond
;      ((eq? a b) True)
;      ((pair? a)
;         (and (pair? b)
;            (equal? (car a) (car b))
;            (equal? (cdr a) (cdr b))))
;      (else False)))
         

;; todo: move string->integer elsewhere
;;; string base -> number | False
(define string->integer/base

   (define (byte->digit val base)
      (cond
         ((and (<= 48 val) (<= val 57))
            (let ((val (- val 48)))
               (if (< val base) val False)))
         ((and (<= 97 val) (<= val 122))
            (let ((val (+ (- val 97) 10)))
               (if (< val base) val False)))
         (else False)))

   (define (digits->number s pos n base)
      (cond
         ((= pos (string-length s))
            n)
         ((byte->digit (refb s pos) base) =>
            (λ (this)
               (digits->number s (+ pos 1) (+ (* n base) this) base)))
         (else False)))

   (λ (s base)
      (let ((len (string-length s)))
            (if (> len 0)
               (let ((first (refb s 0)))
                  (cond
                     ((eq? first 43)
                        (digits->number s 1 0 base))
                     ((eq? first 45)
                        (cond
                           ((digits->number s 1 0 base) =>
                              (λ (num) (- 0 num)))
                           (else False)))
                     (else
                        (digits->number s 0 0 base))))
               False))))

(define (string->integer str)
   (string->integer/base str 10))



   

,r "owl/intern.l"

(import lib-intern)

(import lib-unicode encode-point)


;;;
;;; The eof object
;;;

(define-module lib-eof

   (export eof? render)

   (define (eof? x) (eq? (type x) 34))

   ;; eof
   (define render
      (λ (self obj tl)
         (if (eof? obj)
            (ilist 69 111 102 tl)
            (render self obj tl)))))

(import lib-eof)


;;;
;;; Functions
;;;

(define-module lib-function

   (export function? bytecode? render)

   (define (function? x) (eq? #b110 (fxband (type x)  #b11111110)))

   ;                                                               .-> ignore padding byte count
   ;                            .-> raw data object              .-+
   (define (bytecode? x) (eq? #b100000000110 (fxband (type x) #b100011111110))) 
	;                             '------+
	;                                    '-> 8-bit type/padding info

   (define render 
      (λ (self obj tl)
         (if (function? obj)
            (ilist 35 60
               (self self
                  (interact 'meta (tuple 'get-name obj)) ;; ask for a name for this (remove later)
                  ;"function"  ;; just print as #<function>
                  (cons 62 tl)))
            (render self obj tl)))))

(import lib-function)


;;;
;;; Tuples
;;;

(define-module lib-tuple

   (export tuple? render
      list->tuple tuple->list
      read-tuple)

   (define (tuple? x) (eq? (type x) 22))

   (define (list->tuple lst)
      (let ((l (length lst)))
         (if (teq? l fix+)
            (listuple 2 l lst)
            (error "list does not fit a tuple: length " l))))

   (define (read-tuple tuple pos lst)
      (if (= pos 0)
         lst
         (read-tuple tuple (- pos 1)
            (cons (ref tuple pos) lst))))

   (define (tuple->list tuple)
      (read-tuple tuple (size tuple) null))

   (define render
      (λ (self obj tl)
         (if (tuple? obj)
            (ilist 35 40 
               (self self (ref obj 1)
                  (lfold
                     (λ (tl pos) (cons 32 (self self (ref obj pos) tl)))
                     (cons 41 tl)
                     (liota (size obj) -1 1))))
            (render self obj tl)))))

(import lib-tuple)


;;;
;;; IO
;;;

,r "owl/io.l"

(import lib-io)


;;;
;;; Common object serializetion 
;;;

;; old foldable api wrapper
(define (renderer o tl) 
	(render render o tl))


(define (verb obj) 
   (render render obj null))

(define (print-to obj to) 
   (mail to (render render obj '(10))))

(define (display-to obj to) 
   (mail to (render render obj '())))

(define (display x) 
   (display-to x stdout))

(define (print obj)
   (mail stdout 
      (render render obj '(10))))

; note, print* and show are both atomic
; for some reason a 2-arg show is still used although 
; it would make more sense just to make it a n-ary macro

(define (print* lst)
   (mail stdout
      (foldr renderer '(10) lst)))

(define-syntax output
   (syntax-rules ()
      ((output . stuff)
         (print* (list stuff)))))

(define (show a b)
   (mail stdout (render render a (render render b '(10)))))



;;;
;;; S-expression parsing
;;; 

,r "owl/parse.l"

(import lib-parse)

,r "owl/sexp.l"

(import lib-sexp)


;;;
;;; Environment
;;;

(define (ok? x) (eq? (ref x 1) 'ok))
(define (ok exp env) (tuple 'ok exp env))
(define (fail reason) (tuple 'fail reason))

,r "owl/env.l"

(import lib-env)


;;; Gensyms and sexp utils 

,r "owl/gensym.l"

(import lib-gensym)


;; does not belong here, but needed in macros for now 

(define (verbose-vm-error opcode a b)
   (cond
      ((eq? opcode 256)
         ; fixme, add but got ...
         (list 'function b 'expected a 'arguments))
      ((eq? opcode 52) (list "car: bad pair: " a))
      ((eq? opcode 53) (list "cdr: bad pair: " a))
      (else
         (list "error: " 'instruction opcode 'info (tuple a b)))))

;;;
;;; Macro expansion
;;;

,r "owl/macros.l"

(import lib-macros)


;;;
;;; Sexp -> AST translation
;;;

,r "owl/ast.l"

(import lib-ast)


;;;
;;; Computing fixed points
;;;

,r "owl/recursion.l"

(import lib-recursion)


;;;
;;; CPS
;;;

,r "owl/cps.l"

(import lib-cps)


;;; 
;;; Alpha conversion -- replace each formal with a unique symbol
;;; 

,r "owl/alpha-convert.l"

(import lib-alpha-convert)

; a value that can be created by an instruction

(define (small-value? val)
   (or
      (and (fixnum? val) (>= val -127) (< val 127))   
      (eq? val True)
      (eq? val False)
      (eq? val null)))


;;;
;;; Closure Conversion + Literal Conversion
;;;

,r "owl/closurize.l"

(import lib-closurize)


;;;
;;; Bytecode Assembler
;;;

,r "owl/assemble.l"

(import lib-assemble)


;;;
;;; Register transfer language, hic sunt hackones...
;;;

,r "owl/compile.l"

(import lib-compile)


;;;
;;; Code sharing
;;;

,r "owl/share-code.l"

(import lib-code-share share-code vm-special-ops->codes) ;; todo: cut vm-special-ops out of here (to assembly?)


;;;
;;; Master evaluator
;;;

(define error-tag "err")

; values are evaluated by a separate thread
; this way the errors do not affect the repl thread


(define (error? x)
   (and (tuple? x)
      (eq? (ref x 1) error-tag)))

(define (execute exp env)
   (ok (exp) env))



;;;
;;; The compiler
;;;

;; todo: add partial evaluation
;; todo: add type inference (Hindley-Milner for the primitive types, save and use result when inferable)
;; todo: move compiler code to a more appropriate place (like lib-compile, or lib-eval)

; (op exp env) -> #(ok exp' env') | #(fail info)
(define compiler-passes
   (list
      apply-env       ;; apply previous definitions 
      sexp->ast       ;; safe sane tupled structure
      fix-points      ;; make recursion explicit <3
      alpha-convert   ;; assign separate symbols to all bound values
      cps             ;; convert to continuation passing style
      build-closures  ;; turn lambdas into closures where necessary
      compile         ;; assemble to bytecode
      execute         ;; call the resulting code
      ))

; run the code in its own thread 
(define (evaluate-as exp env task)
   ; run the compiler chain in a new task
   (fork-linked task
      (λ ()
         (call/cc
            (λ exit
               (fold
                  (λ state next
                     (if (ok? state)
                        (begin
                           ;(show " - compiler at exp " (ref state 2))
                           (next (ref state 2) (ref state 3)))
                        (exit state)))
                  (ok exp env)
                  compiler-passes)))))
   ; grab the result
   (tuple-case (ref (accept-mail (λ (env) (eq? (ref env 1) task))) 2)
      ((finished result not used)
         result) ; <- is already ok/fail
      ((crashed opcode a b)
         (fail (verbose-vm-error opcode a b)))
      ((error cont reason info)
         ; note, these could easily be made resumable by storing cont
         (fail (list reason info)))
      ((breaked)
         (fail "breaked"))
      (else is foo
         (fail (list "Funny result for compiler " foo)))))

(define (evaluate exp env) (evaluate-as exp env 'repl-eval))

; fixme, make more power-efficient later, for example by 
; adding negative fixnums to sleep seconds and pick
; the minimum in ovm.

,r "owl/time.l"

;; fixme: should sleep one round to get a timing, and then use avg of the last one(s) to make an educated guess
(define (sleep ms)
   (lets ((end (+ ms (time-ms))))
      (let loop ()
         ;(print (syscall 18 1 1))
         (let ((now (time-ms)))
            (if (> now end)
               now
               (begin (interact sleeper-id 65535) (loop)))))))

; -> mcp gets <cont> 5 reason info

; (run <mcp-cont> thunk quantum) -> result

(define input-chunk-size  1024)
(define output-chunk-size 4096)

(define file-in 0)
(define file-out 1)

; read-file path|fd fail -> (exp ...) ∨ (fail reason)
(define (read-file src fail)
   (cond
      ((string? src)
         (let ((port (open-input-file src)))
            (if port
               (read-file port fail)
               (fail (list "unable to open " src)))))
      ((number? src)
         (read-exps-from src null fail))
      (else 
         (fail (list "bad source " src)))))

(define (name->func name)
   (some
      (λ (x) (if (eq? (ref x 1) name) (ref x 5) False))
      primops))

(define-syntax share-bindings
   (syntax-rules (defined)
      ((share-bindings) null)
      ((share-bindings this . rest)
         (cons
            (cons 'this
               (tuple 'defined (mkval this)))
            (share-bindings . rest)))))

(define (share-modules mods) 
   (for null mods
      (λ envl mod
         (append (ff->list mod) envl))))

,r "owl/arguments.l"
,r "owl/random.l"
,r "owl/cgen.l"

(import lib-args)

,r "owl/mcp.l"

(import lib-mcp)

,r "owl/dump.l"

(import lib-dump make-compiler dump-fasl)

(define compiler ; <- to compile things out of the currently running repl using the freshly loaded compiler
   (make-compiler *vm-special-ops*))

; path -> 'loaded | 'saved
(define (suspend path)
   (let ((maybe-world (syscall 16 True True)))
      (if (eq? maybe-world 'resumed)
         'loaded
         (begin
            (dump-fasl maybe-world path)
            'saved))))

,r "owl/checksum.l"
(import lib-checksum checksum)

;; todo: share the modules instead later
(define shared-misc
   (share-bindings
      run syscall error
      pair?  boolean?  fixnum?  eof?  symbol?  char?
      tuple?  string?  function?  equal? bytecode?
      not
      null?  null 
      o
      time
      time-ms
      halt
      seccomp
      call/cc
      call/cc2
      display print-to print print* show
      render verb
      system-println
      sleep
      list->tuple
      exit-thread
      fork
      fork-named
      fork-linked
      fork-server
      fork-linked-server
      exit-owl
      single-thread?
      set-ticker
      kill
      catch-thread
      release-thread
      suspend
      mail interact
      wait
      wait-mail accept-mail check-mail return-mails
      set-signal-action
      fopen
      byte-vector?
      string->symbol
      close-port flush-port
      read-file
      module-ref
      string->integer
      set-memory-limit 
      get-word-size
      get-memory-limit
      checksum
      ))

,r "owl/fasl.l"     ; encoding and decoding arbitrary objects as lists of bytes
,r "owl/checksum.l" ; checksums for (lazy) lists of numbers
,r "owl/queue.l"    ; double-ended lists
,r "owl/suffix.l"   ; suffix sorting
,r "owl/bisect.l"   ; binary searches 
,r "owl/test.l"     ; a simple algorithm equality/benchmark tester
,r "owl/regex.l"    ; regular expressions
,r "owl/sys.l"      ; more operating system interface
;,r "owl/sat.l"      ; naive SAT solving

;,r "owl/ppm.l"      ; support for reading ppm image files
;,r "owl/grale.l"    ; simple 8-bit graphics if grale available (not in use until grale can use internal programs)

;; included but not imported by default
(define shared-extra-libs
   (share-bindings
      lib-iff          ; (import lib-iff) if needed
      lib-args         ; ditto
      lib-parse        ; .
      ;lib-vt          ; .
      ;lib-system      ; 
      lib-rlist        ;
      lib-list         ;
      lib-unicode      ;
      lib-mcp
      lib-dump
      lib-checksum
      lib-queue
      lib-test
      ;lib-sat
      ;lib-grale
      ;lib-ppm
      ))

;; included and and imported on toplevel
(define shared-default-modules
   (share-modules
      (list
         lib-generic   ;; use generic functions by defult. must be first to not be overridden.
         lib-list
         lib-math
         lib-list-extra
         lib-math-extra
         lib-ff
         lib-string
         lib-vector
         lib-sort
         lib-bisect
         lib-random
         lib-primop
         lib-base
         lib-lazy   
         lib-fasl
         lib-suffix
         lib-cgen
         lib-io
         lib-regex
         lib-rlist
         lib-sys
         lib-syscall)))

(define shared-bindings
   (foldr append null 
      (list 
         shared-default-modules 
         shared-extra-libs
         shared-misc)))

;; initial primops and special forms 

(define initial-environment-sans-macros
   (let
      ((primitive
         (λ sym
            (cons sym
               (tuple 'defined
                  (tuple 'value (name->func sym)))))))
      (list->ff
         (append
            (list
               ;; special forms.
               (cons 'lambda  (tuple 'special 'lambda))
               (cons 'quote   (tuple 'special 'quote))
               (cons 'rlambda (tuple 'special 'rlambda)) 
               (cons 'receive (tuple 'special 'receive)) 
               (cons '_branch (tuple 'special '_branch)) 
               (cons '_define (tuple 'special '_define)) 
               (cons 'values   (tuple 'special 'values))

               (primitive 'cons)
               (primitive 'car)
               (primitive 'cdr)
               (primitive 'eq?)
               ;(primitive 'fx%)
               (primitive 'type)
               (primitive 'size)
               (primitive 'cast)
               (primitive 'fetch)
               (primitive 'ref)
               (primitive 'sys-prim)
               (primitive 'refb)
               (primitive 'pick)
               (primitive 'mk)
               (primitive 'mkr)
               (primitive 'sys)
               (primitive 'fxbor)
               (primitive 'fxbxor)
               (primitive 'fread)
               (primitive '_fopen)
               (primitive 'fclose)
               (primitive 'fsend)
               (primitive 'lraw)
               (primitive 'raw)
               (primitive '_connect)
               (primitive '_sopen)
               (primitive 'accept)
               (primitive 'mkt)
               (primitive 'bind)
               (primitive 'set)
               (primitive 'lesser?)
               (primitive 'call-native)
               ;(primitive 'halt)
               (primitive 'mkred)
               (primitive 'mkblack)
               (primitive 'ff-bind)
               (primitive 'ff-toggle)
               (primitive 'ffcar)
               (primitive 'ffcdr)
               (primitive 'red?)
               (primitive 'listuple)
               ;(primitive 'get)
               ;(primitive 'fupd)
               (primitive 'fxband)         
               (primitive 'fx+)
               (primitive 'fxqr)
               (primitive 'fx*)
               (primitive 'fx-)
               (primitive 'fx<<)
               (primitive 'fx>>)
               ;(primitive 'fxbdivmod)
               ;(primitive 'fxdivmod)
               (primitive 'ncons)
               (primitive 'ncar)
               (primitive 'ncdr)
               (primitive 'raw-mode)
               (primitive '_sleep)
               (primitive 'iomux)
               (primitive 'clock)
               (primitive 'time)
               (primitive 'sizeb)
               ;(primitive 'vm-set)
               (primitive 'blit)
               (primitive 'getev)
               (primitive 'fill-rect)

               ; graphics extension (comment out "start growling" sections in ovm.c if not needed)
               ; no, relocate the code back to blit.srv after posix is running. lila protocol ftw
               ;(primitive 'win-open) 
               ;(primitive 'win-put)
               ;(primitive 'win-update)
               ;(primitive 'win-line)
               ;(primitive 'win-circle)
               ;(primitive 'win-rectangle)
               ;(primitive 'win-arc)
               ;(primitive 'win-event)
               ;(primitive 'win-fill-rect)

               ; needed to define the rest of the macros
               ; fixme, could use macro-expand instead
               (cons 'define-syntax
                  (tuple 'macro
                     (make-transformer
                        '(define-syntax syntax-rules add quote)
                        '(
                           ((define-syntax keyword 
                              (syntax-rules literals (pattern template) ...))
                        ()
                        (quote syntax-operation add False 
                              (keyword literals (pattern ...) 
                              (template ...)))))))))
         shared-bindings))))

(define (define-macros env lst)
   (for env lst
      (λ env defn
         (tuple-case (macro-expand defn env)
            ((ok result env) env)
            (else (error "Failed to add initial macro " (car lst)))))))

(define initial-macros '(

   ; actually i'd prefer this vice versa
   ; protip: ab zlambda λ
   (define-syntax λ 
      (syntax-rules () 
         ((λ a) (lambda () a))
         ((λ (v ...) . body) (lambda (v ...) . body))
         ((λ v ... body) (lambda (v ...) body))))

   (define-syntax begin
      (syntax-rules (define define-syntax letrec)
         ((begin
            (define-syntax key1 rules1)
            (define-syntax key2 rules2) ... . rest)
      (letrec-syntax ((key1 rules1) (key2 rules2) ...)
            (begin . rest)))
         ((begin exp) exp)
         ((begin (define . a) (define . b) ... . rest)
            (begin 42 () (define . a) (define . b) ... . rest))
         ((begin 42 done (define (var . args) . body) . rest)
            (begin 42 done (define var (lambda args . body)) . rest))
         ((begin 42 done (define var exp1 exp2 . expn) . rest)
            (begin 42 done (define var (begin exp1 exp2 . expn)) . rest))
         ((begin 42 done (define var val) . rest)
            (begin 42 ((var val) . done) . rest))
         ((begin 42 done . exps)
            (begin 43 done () exps))
         ((begin 43 (a . b) c exps)
            (begin 43 b (a . c) exps))
         ((begin 43 () bindings exps)
            (letrec bindings (begin . exps)))
         ((begin first . rest)  
            ((lambda (free)
               (begin . rest))
               first))))

   (define-syntax quasiquote
      (syntax-rules (unquote quote unquote-splicing)
         ((quasiquote (unquote exp))
            exp)
         ((quasiquote ((unquote-splicing term) . tail))
            (append term (quasiquote tail)))
         ((quasiquote (op . args))
            (cons
               (quasiquote op)
               (quasiquote args)))
         ((quasiquote atom)
            (quote atom))
         ((quasiquote) '())))

   (define-syntax letrec
      (syntax-rules (rlambda)
         ((letrec ((?var ?val) ...) ?body) (rlambda (?var ...) (?val ...) ?body))
         ((letrec vars body ...) (letrec vars (begin body ...)))))

   (define-syntax let
         (syntax-rules ()
            ((let ((var val) ...) exp . rest) 
               ((lambda (var ...) exp . rest) val ...))
            ((let keyword ((var init) ...) exp . rest) 
               (letrec ((keyword (lambda (var ...) exp . rest))) (keyword init ...)))))

   ; Temporary hack: if inlines some predicates.

   (define-syntax if
      (syntax-rules 
         (not eq? and null? pair? teq? imm alloc raw
            fix+ fix- int+ int- pair rat comp)
         ((if test exp) (if test exp False))
         ((if (not test) then else) (if test else then))
         ((if (null? test) then else) (if (eq? test '()) then else))
         ((if (pair? test) then else) (if (teq? test (alloc 1)) then else))
         ((if (teq? q fix+) . c) (if (teq? q (imm    0)) . c))
         ((if (teq? q fix-) . c) (if (teq? q (imm   32)) . c))
         ((if (teq? q int+) . c) (if (teq? q (alloc  9)) . c))      ; num base type
         ((if (teq? q int-) . c) (if (teq? q (alloc 41)) . c))      ; num/1
         ((if (teq? q pair) . c) (if (teq? q (alloc  1)) . c))      
         ((if (teq? q rat) . c)  (if (teq? q (alloc 73)) . c))      ; num/2
         ((if (teq? q comp) . c)  (if (teq? q (alloc 105)) . c))   ; num/3
         ((if (teq? (a . b) c) then else) 
            (let ((foo (a . b)))
               (if (teq? foo c) then else)))
         ((if (teq? a (imm b)) then else) (_branch 1 a b then else))   
         ((if (teq? a (alloc b)) then else) (_branch 2 a b then else))
         ((if (teq? a (raw b)) then else) (_branch 3 a b then else))
         ((if (eq? a b) then else) (_branch 0 a b then else))            
         ((if (a . b) then else) (let ((x (a . b))) (if x then else)))
         ((if (teq? a b) then else) (teq? a b then else))
         ;((if (eq? a a) then else) then) ; <- could be functions calls and become non-eq?
         ((if False then else) else)
         ((if True then else) then)
         ((if test then else) (_branch 0 test False else then))))

   (define-syntax cond
      (syntax-rules (to else =>)
         ((cond) False)
         ((cond (else exp . rest))
            (begin exp . rest))
         ((cond (clause => exp) . rest) 
            (let ((fresh clause))
               (if fresh
                  (exp fresh)
                  (cond . rest))))
         ((cond (clause to exp) . rest) 
            (let ((fresh clause))
               (if fresh
                  (apply exp fresh)
                  (cond . rest))))
         ((cond (clause exp . rest-exps) . rest) 
            (if clause
               (begin exp . rest-exps)
               (cond . rest)))))

   (define-syntax case
      (syntax-rules (else has?)
         ((case (op . args) . clauses)
            (let ((fresh (op . args)))
               (case fresh . clauses)))
         ((case thing (else . body))
            (begin . body))
         ((case thing ((a) . body) . clauses)
            (if (eq? thing (quote a))
               (begin . body)
               (case thing . clauses)))
         ((case thing ((a . b) . body) . clauses)
            (if (has? (quote (a . b)) thing)
               (begin . body)
               (case thing . clauses)))))

   (define-syntax define
      (syntax-rules ()
         ((define (op . args) body)
            (define op
               (letrec ((op (lambda args body))) op)))
         ((define op val)
            (_define op val))
         ((define op a . b)
            (define op (begin a . b)))))

   ;; fixme, should use a print-limited variant for debugging

   (define-syntax define*
      (syntax-rules (show list)
         ((define* (op . args) . body)
            (define (op . args) 
               (show " * " (list (quote op) . args))
               .  body))
         ((define* name (lambda (arg ...) . body))
            (define* (name arg ...) . body))))

   (define-syntax lets
      (syntax-rules (<=)
         ((lets (((var ...) gen) . rest) . body)
            (receive gen (lambda (var ...) (lets rest . body))))
         ((lets ((var val) . rest-bindings) exp . rest-exps)
            ((lambda (var) (lets rest-bindings exp . rest-exps)) val))
         ((lets ((var ... (op . args)) . rest-bindings) exp . rest-exps)
            (receive (op . args)
               (lambda (var ...) 
                  (lets rest-bindings exp . rest-exps))))
         ((lets ((var ... node) . rest-bindings) exp . rest-exps)
            (bind node
               (lambda (var ...) 
                  (lets rest-bindings exp . rest-exps))))
         ((lets (((name ...) <= value) . rest) . code)
            (bind value
               (lambda (name ...)
                  (lets rest . code))))
         ((lets ()) exp)
         ((lets () exp . rest) (begin exp . rest))))

   ; i hate special characters, especially in such common operations.
   ; lets (let sequence) is way prettier and a bit more descriptive 

   (define-syntax let*
      (syntax-rules ()
         ((let* . stuff) (lets . stuff))))

   (define-syntax or
      (syntax-rules ()
         ((or) False)
         ((or (a . b) . c)
            (let ((x (a . b)))
               (or x . c)))
         ((or a . b)
            (if a a (or . b)))))

   (define-syntax and
      (syntax-rules ()
         ((and) True)
         ((and a) a)
         ((and a . b)
            (if a (and . b) False))))

   (define-syntax list
      (syntax-rules ()
         ((list) '())
         ((list a . b)
            (cons a (list . b)))))

   (define-syntax ilist
      (syntax-rules ()
         ((ilist a) a)
         ((ilist a . b)
            (cons a (ilist . b)))))

   (define-syntax tuple
      (syntax-rules ()
         ((tuple a . bs) ;; there are no such things as 0-tuples
            (mkt 2 a . bs))))

   ; replace this with typed destructuring compare later on 

   (define-syntax tuple-case
      (syntax-rules (else _ is)
         ((tuple-case (op . args) . rest)
            (let ((foo (op . args)))
               (tuple-case foo . rest)))
         ;;; bind if the first value (literal) matches first of pattern
         ((tuple-case 42 tuple type ((this . vars) . body) . others)
            (if (eq? type (quote this))
               (bind tuple
                  (lambda (ignore . vars) . body))
               (tuple-case 42 tuple type . others)))
         ;;; bind to anything
         ((tuple-case 42 tuple type ((_ . vars) . body) . rest)
            (bind tuple
               (lambda (ignore . vars) . body)))
         ;;; an else case needing the tuple
         ((tuple-case 42 tuple type (else is name . body))
            (let ((name tuple))
               (begin . body)))
         ;;; a normal else clause
         ((tuple-case 42 tuple type (else . body))
            (begin . body))
         ;;; throw an error if nothing matches
         ((tuple-case 42 tuple type)
            (div 1 0))
         ;;; get type and start walking
         ((tuple-case tuple case ...)
            (let ((type (ref tuple 1)))
               (tuple-case 42 tuple type case ...)))))

   (define-syntax type-case
      (syntax-rules 
         (else -> teq? imm alloc)
         
         ((type-case ob (else . more))
            (begin . more))
         ((type-case ob (else -> name . more))
            (let ((name ob)) . more))
         ((type-case (op . args) . rest)
            (let ((foo (op . args)))
               (type-case foo . rest)))
         ((type-case ob (type -> name . then) . more)
            (if (teq? ob type)
               (let ((name ob)) . then)
               (type-case ob . more)))
         ((type-case ob (pat . then) . more)
            (if (teq? ob pat)
               (begin . then)
               (type-case ob . more)))))

))



;;;
;;; REPL
;;;

,r "owl/repl.l"

(import lib-repl)

(define initial-environment
	(bind-toplevel
		(define-macros
			initial-environment-sans-macros
			initial-macros)))

;; todo: after there are a few more compiler options than one, start using -On mapped to predefined --compiler-flags foo=bar:baz=quux

(define command-line-rules
   (cl-rules
     `((help       "-h" "--help")
       (about      "-a" "--about")
       (version  "-v" "--version")
       (evaluate "-e" "--eval"     has-arg comment "evaluate expressions in the given string")
       (quiet    "-q" "--quiet"    comment "be quiet (default in non-interactive mode)")
       ;(memlimit "-m" "--max-heap" comment "maximum heap size (in mb)" cook ,string->integer)
       (run      "-r" "--run"      has-arg comment "run the last value of the given foo.scm with given arguments")
       (notes    "-n" "--notes"    comment "show notes from code (meaning \\n;; <label>:<text>\\n)")
       (load     "-l" "--load"     has-arg  comment "resume execution of a saved program state (fasl)")
       (output   "-o" "--output"   has-arg  comment "where to put compiler output (default auto)")
       ;(seccomp  "-s" "--seccomp"  comment "enter seccomp at startup if compiled with -DUSE_SECCOMP")
       (output-format  "-x" "--output-format"   has-arg comment "output format when compiling (default auto)")
       (native  False "--native"   comment "compile all applicable bytecode fragments to C")
       (usual-suspects  False "--usual-suspects"   comment "compile a few common functions to C")
       ;(linked  False "--most-linked" has-arg cook ,string->integer comment "compile most linked n% bytecode vectors to C")
       (no-threads False "--no-threads" comment "do not include threading and io to generated c-code")
       )))

(define brief-usage-text "Usage: ol [args] [file] ...")

(define error-usage-text "ol -h helps.")

;; repl-start, thread controller is now runnig and io can be 
;; performed. check the vm args what should be done and act 
;; accordingly.

; note, return value is not the owl return value. it comes
; from thread controller after all threads have finished.


(define (strip-zeros n)
   (cond
      ((= n 0) n)
      ((= 0 (rem n 10))
         (strip-zeros (div n 10)))
      (else n)))

(define (memory-limit-ok? n w)
   (cond
      ((< n 1) (print "Too little memory allowed.") False)
      ((and (= w 4) (> n 4096)) (print "This is a 32-bit executable, so you cannot use more than 4096Mb of memory.") False)
      ((and (= w 8) (> n 65536)) (print "65536 is as high as you can go.") False)
      (else True)))

(define (maybe-set-memory-limit args)
   (let ((limit (get args 'memlimit False)))
      (if limit
         (if (memory-limit-ok? limit (get-word-size))
            (set-memory-limit limit)
            (system-println "Bad memory limit")))))

(define (c-source-name path)
	(cond
		((/\.[a-z]+$/ path) ;; .scm, .lisp, .owl etc
			(s/\.[a-z]+$/.c/ path))
		(else
			(string-append path ".c"))))

(define (try thunk fail-val)
   ; run the compiler chain in a new task
   (let ((id (list 'thread)))
      (fork-linked-server id thunk)
      (tuple-case (ref (accept-mail (λ (env) (eq? (ref env 1) id))) 2)
         ((finished result not used)
            result)
         ((crashed opcode a b)
            (print-to (verbose-vm-error opcode a b) stderr)
            fail-val)
         ((error cont reason info)
            ; note, these could easily be made resumable by storing cont
            (mail stderr
               (foldr renderer '(10) (list "error: " reason info)))
            fail-val)
         (else is bad ;; should not happen
            (print-to (list "que? " bad) stderr)
            fail-val))))

(define (owl-run outcome args path)
   (if outcome
      (tuple-case outcome
         ((ok val env)
            ;; be silent when all is ok
            ;; exit with 127 and have error message go to stderr when the run crashes
            (try (λ () (val args)) 127))
         ((error reason env)
            (print-repl-error
               (list "ol: cannot run " path " as there there was an error during loading:" reason))
            2))
      1))

(define about-owl 
"Owl Lisp -- It's a lisp thing.
Copyright (c) 2008-2011 Aki Helin
Check out http://code.google.com/p/owl-lisp for more information.")





;;;
;;; MCP, master control program and the thread controller
;;;

; special keys in mcp state 

(import lib-mcp)

,r "owl/threads.l"

(import lib-threads)


;; pick usual suspects in a module to avoid bringing them to toplevel here
;; mainly to avoid accidentalaly introducing bringing generic functions here  

(define-module lib-usual-suspects
   (export usual-suspects)
   ; make sure the same bindings are visible that will be at the toplevel
   (import lib-generic)
   (import lib-suffix)
   (import lib-math)
   (import lib-random)
   (import lib-bisect)
   (import lib-io start-output-thread)
   (import lib-threads thread-controller)
   ; commonly needed functions 
   (define usual-suspects
      (list
            put get del ff-fold fupd
            - + * /
            div gcd ediv
            << < <= = >= > >> 
            equal? has? mem
            band bor bxor
            sort suffix-array 
            bisect bisect-range
            fold foldr for map reverse length zip append unfold
            lref lset iota
            vec-ref vec-len vec-fold vec-foldr
            print mail interact 
            ;lib-rlist lib-iff
            iter iterr
            take keep remove 
            start-output-thread thread-controller
            rnd seed->rands)))

(import lib-usual-suspects usual-suspects)


;; handles $ ol -c stuff
(define (repl-compile compiler env path opts)
   (try
      (λ ()
         ;; evaluate in a thread to catch error messages here
         (let ((outcome (if (equal? path "-") (repl-port env stdin) (repl-file env path))))
            (tuple-case outcome
               ((ok val env)
                  (if (function? val)
                     (begin
                        (compiler val 
                           ;; output path
                           (cond
                              ((get opts 'output F) => (λ (given) given)) ; requested with -o
                              ((equal? path "-") path) ; stdin → stdout
                              (else (c-source-name path)))
                           ;; inverse option on command line, add here if set
                           (if (get opts 'no-threads F)
                              opts
                              (put opts 'want-threads T))
                           ;; to be customizable via command line opts
                           (cond
                              ((get opts 'native F) val)
                              ((get opts 'usual-suspects F)
                                 usual-suspects)
                              (else False)))
                        0)
                     (begin
                        (show "The last value should be a function of one value (the command line arguments), but it is instead " val)
                        2)))
               ((error reason env)
                  (print-repl-error
                     (list "Cannot compile " path " because " reason))
                  2)
               (else
                  (print-repl-error "Weird eval outcome.")
                  3))))
      False))

;;; handling of --notes
,r "owl/notes.l"

(import lib-notes show-notes-of)

(import lib-dump load-fasl)

(define (try-load-state path args)
   (let ((val (load-fasl path False)))
      (if (function? val)
         (try (λ () (val (cons path args))) 127)
         (begin
            (show "failed to load dump from " path)
            1))))
  
;; -> vm exit with 0 on success, n>0 on error
(define (try-repl-string env str)
   (tuple-case (repl-string env str)
      ((ok val env)
         (print val)
         (exit-owl 0))
      ((error reason partial-env)
         (print-repl-error 
            (list "An error occurred when evaluating: " str ":" reason))
         (exit-owl 1))
      (else
         (print "Multifail")
         (exit-owl 2))))

;; todo: this should probly be wrapped in a separate try to catch them all
; ... → program rval going to exit-owl
(define (repl-start vm-args repl compiler env)
   (or
      (process-arguments (cdr vm-args) command-line-rules error-usage-text
         (λ (dict others)
            (let 
               ((env 
                  (if (fold (λ (is this) (or is (get dict this F))) F '(quiet evaluate run output output-format))
                     (del env '*owl-prompt*) 
                     (put env '*interactive* True))))
               (cond
                  ((get dict 'help False)
                     (print brief-usage-text)
                     (print-rules command-line-rules)
                     0)
                  ((get dict 'version False)
                     (show "owl lisp version " *owl-version*)
                     0)
                  ((get dict 'about False) (print about-owl) 0)
                  ((get dict 'notes False) (show-notes-of others) 0)
                  ((get dict 'load False) =>
                     (λ (path) (try-load-state path others)))
                  ((or (get dict 'output F) (get dict 'output-format F))
                     (if (< (length others) 2) ;; can take just one file or stdin
                        (repl-compile compiler env 
                           (if (null? others) "-" (car others)) dict)
                        (begin
                           (show "compile just one file for now please: " others)
                           1)))
                  ((get dict 'run False) =>
                     (λ (path)
                        (owl-run (try (λ () (repl-file env path)) False) (cons "ol" others) path)))
                  ((get dict 'evaluate False) => 
                     (λ (str)
                        (try-repl-string env str))) ;; fixme, no error reporting
                  ((null? others)
                     (repl-trampoline repl env))
                  (else
                     ;; load the given files
                     (define input
                        (foldr (λ (path tail) (ilist ',load path tail)) null others))
                     (tuple-case (repl env input)
                        ((ok val env) 0)
                        ((error reason partial-env)
                           (print-repl-error reason)
                           1)))))))
      2))



;;;
;;; The meta thread just collects information about functions (names etc)
;;;

; a test hack: collect also function sources for inlining 

; state = #(names ...)
(define (meta-storage state)
   (bind (wait-mail)
      (lambda (sender message)
         (tuple-case message
            ((set-name obj name)
               ; (show "meta: naming " name)
               (meta-storage
                  (set state 1
                     (put (ref state 1) obj name))))
            ((get-name obj)
               (let ((name (get (ref state 1) obj 'function)))
                  (mail sender name)
                  (meta-storage state)))
            ((set-source obj src)
               (meta-storage
                  (set state 2 
                     (put (ref state 2) obj src))))
            ((get-source obj)
               (let ((src (get (ref state 2) obj False)))
                  (mail sender src)
                  (meta-storage state)))
            (else
               (show "meta-storage: strange request: " message)
               (meta-storage state))))))

(define (collect-function-names env)
   (ff-fold
      (lambda (collected name value)
         (tuple-case value
            ((defined node)
               (tuple-case node
                  ((value val)
                     (if (function? val)
                        (put collected val name)
                        collected))
                  (else collected)))
            (else collected)))
      False env))

; *owl* points to owl root directory
; initally read from binary path (argv [0] )

(define (directory-of path)
   (runes->string
      (reverse
         (drop-while 
            (lambda (x) (not (eq? x 47)))
            (reverse
               (string->bytes path))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Dump a new repl image
;;;

;; call by repl to render result of evaluation and ask for more input
(define (default-prompt val)
   (print val)
   (display "> ")
   (flush-port stdout))
   
(define (heap-entry syms)
   ;(print " - heap entry got syms")
   (lets
      ((symbol-root (fold put-symbol False syms))
   (new-symbol-interner (initialize-symbol-interner symbol-root))
   (initial-names (collect-function-names initial-environment)))
      ;(print " - heap entry waiting for vm special ops")
      (λ (vm-special-ops)
         ;(show " - repl vm special op count " (length (ff->list vm-special-ops)))
         (let ((compiler (make-compiler vm-special-ops)))
            ;; still running in the boostrapping system
            ;; the next value after evaluation will be the new repl heap
            (λ (vm-args)
               ;; now we're running in the new repl 
               (thread-controller 
                  (list
                     (tuple 'init
                        (λ () 
                           (fork-server 'repl
                              (λ () 
                                 ;; get basic io running
                                 (start-base-threads)

                                 (mcp-on-break) ; install a global signal handler

                                 (fork-server 'intern new-symbol-interner)
                                 (fork-server 'meta 
                                    (λ () (meta-storage (tuple initial-names False))))
                                 (exit-owl 
                                    (repl-start vm-args repl compiler
                                       (fold 
                                          (λ (env defn)
                                             (put env (car defn) 
                                                (tuple 'defined (mkval (cdr defn)))))
                                          initial-environment
                                          (list
                                             (cons '*owl* (directory-of (car vm-args)))
                                             (cons '*args* vm-args)
                                             (cons 'dump compiler)                                 ; <- merge here and rename
                                             (cons '*owl-version* *owl-version*)
                                             (cons 'eval exported-eval)
                                             (cons 'stdin  (fd->id 0))
                                             (cons 'stdout (fd->id 1))
                                             (cons 'stderr (fd->id 2))
                                             (cons '*vm-special-ops* vm-special-ops)
                                             (cons '*codes* (vm-special-ops->codes vm-special-ops))
                                             (cons '*owl-prompt* default-prompt)
                                             )))))))))
                  null 
                  False))))))

;; todo: dumping with fasl option should only dump the fasl and only fasl


;;;
;;; Dump the new repl
;;;

;; note, one one could use the compiler of the currently running system, but using 
;; the rebuilt one here to make changes possible in 1 instead of 2 build cycles.
;; (this may be changed later)

(import lib-args)

(define command-line-rules
   (cl-rules
      `((output "-o" "--output" has-arg comment "output path")
        ;(format "-f" "--format" has-arg comment "output format (c or fasl)")
        (specialize "-s" "--specialize" has-arg comment "vm extensions (none, some, all)"))))

(define (choose-natives str all)
   (cond
      ((equal? str "none") null)
      ((equal? str "some") usual-suspects)
      ((equal? str "all") all)
      (else (show "Bad native selection: " str))))

(λ (args)
   (process-arguments (cdr args) command-line-rules "you lose"
      (λ (opts extra)
         (cond
            ((not (null? extra))
               (show "Unknown arguments: " extra)
               1)
            (else
               (compiler heap-entry "unused historical thingy"
                  (list->ff
                     `((output . ,(get opts 'output 'bug))
                       (want-symbols . True)
                       (want-native-ops . True)))
                  (choose-natives 
                     (get opts 'specialize "none")
                     heap-entry))
               0)))))
