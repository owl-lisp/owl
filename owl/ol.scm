;;;
;;; ol.scm: an Owl read-eval-print loop.
;;;

#| Copyright (c) 2012 Aki Helin
 |
 | Permission is hereby granted, free of charge, to any person obtaining a 
 | copy of this software and associated documentation files (the "Software"),
 | to deal in the Software without restriction, including without limitation
 | the rights to use, copy, modify, merge, publish, distribute, sublicense,
 | and/or sell copies of the Software, and to permit persons to whom the
 | Software is furnished to do so, subject to the following conditions
 | 
 | The above copyright notice and this permission notice shall be included 
 | in all copies or substantial portions of the Software.
 | 
 | THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 | IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 | FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
 | THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 | LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
 | FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
 | DEALINGS IN THE SOFTWARE.
 |#

(mail 'intern (tuple 'flush)) ;; ask intern to forget all symbols it knows

;; everything is rebuilt using primops which have to be loaded first

,r "owl/primop.scm" ;; <- to be (include "owl/primop.scm") soon

;; now forget almost everything to avoid heap leaks

,forget-all-but (*vm-special-ops* *libraries* *codes* wait *args* stdin stdout stderr set-ticker run )

;; list of dirs from which to try to load files included in libraries
(define *include-dirs* (list "."))

(define *loaded* '()) ;; used by old ,require

(define *libraries* (list (car *libraries*))) ;; keep just the just loaded primop library

(import (owl primop)) ;; grab freshly defined primops 

;; set a few flags which affect compilation or set static information
(define *owl-version* "0.1.7a")
;(define *interactive* True) ;; causes file names to be printed when loading them

; The symbol _sans_cps acts as a quote to the CPS transformer. This 
; allows things like call/cc to be defined as library functions.

(define call/cc 
   ('_sans_cps (λ (c f) (f c (λ (r v) (c v))))))

(define call-with-current-continuation call/cc)

(define call/cc2
   ('_sans_cps (λ (c f) (f c (λ (r a b) (c a b))))))

(define (i x) x)
(define (k x y) x)

;;; syscalls (interacting with the underlying thread controller implemented in lib/threads.scm and lib/mcp.scm)

;,r "owl/syscall.scm"

(import (owl syscall))

(import (owl defmac))

;;; repl exit codes

(define exit-seccomp-failed 2)     ;; --seccomp given but cannot do it


;;; rendering 

;; render unknown objects as <???>
(define (render self obj tl)
   (ilist 60 63 63 63 62 tl))

;; stop the vm *immediately* without flushing input or anything else with return value n
(define (halt n)
   (sys-prim 6 n n n))

;; throw an error if some familiar but unsupported Scheme functions are called
(define-module lib-unsupported
   (export set! set-car! set-cdr! string-set! vector-set!)

   (define-syntax set!
      (syntax-rules () 
         ((set! var val) (error "set! is not supported: " '(set! var val)))))
  
   (define (unsupported name)
      (error "Mutator not supported: " name))

   (define (set-car! pair val) (unsupported "set-car!"))
   (define (set-cdr! pair val) (unsupported "set-cdr!"))
   (define (vector-set! vec pos val) (unsupported "vector-set!"))
   (define (string-set! str pos val) (unsupported "string-set!"))

)

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

(import-old lib-boolean)


;; todo: move these also to corresponding libraries

(define max-object-size #xffff)


;;; Misc

(define (not x) 
   (if x False True))

(define o (λ f g (λ x (f (g x)))))

(define i (λ x x))

(define self i)

;;; Lists

;; pure list stuff not depending on math or other external things

,r "owl/list.scm"

(import-old lib-list)

;;;
;;; Finite functions
;;;

,require "owl/ff.scm"
(import-old lib-ff)


;;; integer stores, include in toplevel 

,r "owl/iff.scm"


;;;
;;; Math
;;;

,r "owl/math.scm"   ; basic arithmetic
(import-old lib-math) 

,r "owl/list-extra.scm"   ; list ops requiring basic math (length ...)
(import-old lib-list-extra)

,r "owl/math-extra.scm"   ; less basic math (factor, ...)
(import-old lib-math-extra)

,r "owl/lazy.scm"   ; things computed as needed (remember, no caching, these are not ok for dp)
(import-old lib-lazy)


;;;
;;; Sorting
;;;

,r "owl/sort.scm"

(import-old lib-sort)


;;;
;;; Strings
;;;

,r "owl/string.scm"

(import-old lib-string)

(define (number->string n base)
   (list->string (render-number n null base)))

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

,r "owl/vector.scm"

(import-old lib-vector)

;;;
;;; Functions
;;;

(define-module lib-function

   (export function? procedure? bytecode? render)

   ;; raw bytecode vector, 1-level (proc) or 2-level (clos) function
   (define (function? x) (eq? #b110 (fxband (type x)  #b11111110)))

   ;; something executable? being a function or a finite function
   (define (procedure? obj) 
      (or (function? obj) (ff? obj) (not obj)))

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
                  (let ((name (interact 'intern (tuple 'get-name obj))))
                     (or name "function"))
                  (cons 62 tl)))
            (render self obj tl)))))

(import-old lib-function)


;;;
;;; Better error handler
;;;

;; reverse tuple & enlist
#|
(define (function->list func)
   (if (bytecode? func)
      (list func)
      (map (λ (x) (ref func x)) (iota (size func) -1 0))))

;; this isn't accurate enough
;; needed: a way to know which value in a function is the continuation
;; one solution, though not a simple one, would be to disassemble the code and figure out what is called at the end
;; another option, write a version of the vm in owl and use that?
;; third option: use a value browser and let the user look into the current continuation
;; fourth option: just print the internals of the continuation for a hint of location
(define (sub-cont cont)
   (cond
      ((not cont) False)
      ((bytecode? cont) False)
      ((not (function? cont)) False)
      (else (first function? (function->list cont) False))))

(define (cont-chain cont)
   (if (function? cont)
      (cons cont (cont-chain (sub-cont cont)))
      null))

(define error-syscall error)

(define (error msg info)
   (call/cc
      (λ (cont)
         (error-syscall (cons msg info) 
           (foldr (λ (ob tl) (if (null? tl) (list ob) (ilist ob " -> " tl))) null (cdr (cont-chain cont)))))))
|#

;;;
;;; Symbols
;;;


,r "owl/symbol.scm"

(import-old lib-symbol)


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


(define eqv? equal?)
;;;
;;; Random access lists
;;;

,r "owl/rlist.scm"

(import-old lib-rlist)


;;;
;;; Generic versions of the universal common language words
;;;

,r "owl/generic.scm"

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
            (ilist 40 84 117 112 108 101 32
               (self self (ref obj 1)
                  (lfold
                     (λ (tl pos) (cons 32 (self self (ref obj pos) tl)))
                     (cons 41 tl)
                     (liota (size obj) -1 1))))
            (render self obj tl)))))

(import-old lib-tuple)

   

,r "owl/intern.scm"

(import-old lib-intern)

(import-old lib-unicode encode-point)


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

(import-old lib-eof)




;;;
;;; IO
;;;

,r "owl/io.scm"

(import-old lib-io)


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
   (mail stdout (foldr renderer '(10) lst)))

(define (print*-to lst to)
   (mail to (foldr renderer '(10) lst)))

(define-syntax output
   (syntax-rules ()
      ((output . stuff)
         (print* (list stuff)))))

(define (show a b)
   (mail stdout (render render a (render render b '(10)))))



;;;
;;; S-expression parsing
;;; 

,r "owl/parse.scm"

(import-old lib-parse)

,r "owl/sexp.scm"

(import-old lib-sexp)


;;;
;;; Small utils
;;;

(define (string->number str fail)
   (list->number (string->list str) fail))

;;;
;;; Environment
;;;

(define (ok? x) (eq? (ref x 1) 'ok))
(define (ok exp env) (tuple 'ok exp env))
(define (fail reason) (tuple 'fail reason))

(define-module lib-scheme-compat
   (export 
      member memq memv 
      assoc assv assq
      apply rationalize)

   ;; scheme member functions don't follow the argument conventions of other functions 
   (define (member x lst)
      (cond
         ((null? lst) False)
         ((equal? x (car lst)) lst)
         (else (member x (cdr lst)))))

   (define memv member)

   (define (memq x lst)
      (cond
         ((null? lst) False)
         ((eq? x (car lst)) lst)
         (else (memq x (cdr lst)))))

   (define (assq k l)
      (cond
         ((null? l) #f)
         ((eq? (caar l) k) (car l))
         (else (assq k (cdr l)))))
   
   (define (assv k l)
      (cond
         ((null? l) #f)
         ((equal? (caar l) k) (car l))
         (else (assv k (cdr l)))))

   (define assoc assv)

   ;; a silly non-primitive apply
   (define (apply func l)
      (if (null? l)
         (func)
         (lets ((a l l)) (if (null? l) (func a)
         (lets ((b l l)) (if (null? l) (func a b)
         (lets ((c l l)) (if (null? l) (func a b c)
         (lets ((d l l)) (if (null? l) (func a b c d)
         (lets ((e l l)) (if (null? l) (func a b c d e)
         (lets ((f l l)) (if (null? l) (func a b c d e f)
            (error "apply: too many arguments: " (ilist a b c d e f l))))))))))))))))

   ;; owl doesn't have inexact numbers, so any argument
   ;; coming in will always be rational differing by 0
   (define (rationalize n max-delta) n)

)

(import-old lib-scheme-compat)


,r "owl/env.scm"

(import-old lib-env)


;;; Gensyms and sexp utils 

,r "owl/gensym.scm"

(import-old lib-gensym)


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

,r "owl/macros.scm"

(import-old lib-macros)


;;;
;;; Sexp -> AST translation
;;;

,r "owl/ast.scm"

(import-old lib-ast)


;;;
;;; Computing fixed points
;;;

,r "owl/recursion.scm"

(import-old lib-recursion)


;;;
;;; CPS
;;;

,r "owl/cps.scm"

(import-old lib-cps)


;;; 
;;; Alpha conversion -- replace each formal with a unique symbol
;;; 

,r "owl/alpha-convert.scm"

(import-old lib-alpha-convert)

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

,r "owl/closurize.scm"

(import-old lib-closurize)


;;;
;;; Bytecode Assembler
;;;

,r "owl/assemble.scm"

(import-old lib-assemble)


;;;
;;; Register transfer language, hic sunt hackones...
;;;

,r "owl/compile.scm"

(import-old lib-compile)


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

,r "owl/time.scm"

;; fixme: should sleep one round to get a timing, and then use avg of the last one(s) to make an educated guess
(define (sleep ms)
   (lets ((end (+ ms (time-ms))))
      (let loop ()
         ;(print (syscall 18 1 1))
         (let ((now (time-ms)))
            (if (> now end)
               now
               (begin (interact sleeper-id 50) (loop)))))))

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

,r "owl/arguments.scm"
,r "owl/random.scm"
,r "owl/cgen.scm"

(import-old lib-args)

,r "owl/mcp.scm"

(import-old lib-mcp)

,r "owl/dump.scm"

(import-old lib-dump make-compiler dump-fasl)

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

,r "owl/checksum.scm"
(import-old lib-checksum checksum)


;;;
;;; Entering seccomp 
;;;

;; a temporary O(n) way to get some space in the heap

;; fixme: allow a faster way to allocate memory
;; n-megs → _
(define (ensure-free-heap-space megs)
   (if (> megs 0)
      (lets
         ((my-word-size (get-word-size)) ;; word size in bytes in the current binary (4 or 8)
          (blocksize 65536)              ;; want this many bytes per node in list
          (pairsize (* my-word-size 3))  ;; size of cons cell, being [header] [car-field] [cdr-field]
          (bytes                         ;; want n bytes after vector header and pair node for each block
            (map (λ (x) 0) 
               (iota 0 1 
                  (- blocksize (+ pairsize my-word-size)))))
          (n-blocks  
            (ceil (/ (* megs (* 1024 1024)) blocksize))))
         ;; make a big data structure
         (map
            (λ (node)
               ;; make a freshly allocated byte vector at each node
               (list->byte-vector bytes))
            (iota 0 1 n-blocks))
         ;; leave it as garbage
         True)))

;; enter seccomp with at least n-megs of free space in heap, or stop the world (including all other threads and io)
(define (seccomp n-megs)
   ;; grow some heap space work working, which is usually necessary given that we can't 
   ;; get any more memory after entering seccomp
   (if (and n-megs (> n-megs 0))
      (ensure-free-heap-space n-megs))
   (or (sys-prim 10 F F F)
      (begin
         (system-stderr "Failed to enter seccomp sandbox. 
You must be on a newish Linux and have seccomp support enabled in kernel.
")
         (halt exit-seccomp-failed))))


;; Scheme compatibility library 
(define-module lib-char 
   (export char? char->integer integer->char)
   (define char? number?)
   (define char->integer self)
   (define integer->char self)
)

;; profiling doesn't yet have a good home. merge to lib-internals or lib-debug later?
;; run thunk and show n most called functions. no timings yet.
(define (profile thunk n)
   (lets
      ((skip (start-profiling))
       (skip (set-ticker 0))
       (res (thunk))
       (stats (stop-profiling))
       (most-used
         (take
            (sort
               (λ (a b) (> (car a) (car b)))
               (ff-fold
                  (λ (out func n)
                     (cons (cons n func) out))
                  null stats))
            n)))
      (for-each (λ (p) (print*-to (list (car p) ":" (cdr p)) stdout)) most-used) ;; <- could use stderr later
      res))

;; implementation features, used by cond-expand
(define *features*
   (cons 
      (string->symbol (string-append "owl-lisp-" *owl-version*))
      '(owl-lisp r7rs exact-closed ratios exact-complex full-unicode immutable)))
      ;;          ^
      ;;          '-- to be a fairly large subset of at least, so adding this


;; todo: share the modules instead later
(define shared-misc
   (share-bindings
      run syscall error
      pair?  boolean?  fixnum?  eof?  symbol?  
      tuple?  string?  function? procedure? equal? eqv? bytecode?
      not
      null?  null 
      o
      time
      time-ms
      halt
      seccomp
      call/cc
      call/cc2
      call-with-current-continuation
      display print-to print print* show
      render verb
      system-println
      sleep
      list->tuple
      exit-thread
      number->string
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
      string->number
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
      string->sexp
      profile
      *features*
      *include-dirs*
      *libraries*      ;; all currently loaded libraries
      ))

,r "owl/fasl.scm"     ; encoding and decoding arbitrary objects as lists of bytes
,r "owl/checksum.scm" ; checksums for (lazy) lists of numbers
,r "owl/queue.scm"    ; double-ended lists
,r "owl/suffix.scm"   ; suffix sorting
,r "owl/bisect.scm"   ; binary searches 
,r "owl/test.scm"     ; a simple algorithm equality/benchmark tester
,r "owl/regex.scm"    ; regular expressions
,r "owl/sys.scm"      ; more operating system interface
;,r "owl/sat.scm"      ; naive SAT solving

;,r "owl/ppm.scm"      ; support for reading ppm image files
;,r "owl/grale.scm"    ; simple 8-bit graphics if grale available (not in use until grale can use internal programs)

;; included but not imported by default
(define shared-extra-libs
   (share-bindings
      lib-iff          ; (import-old lib-iff) if needed
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
         lib-lazy   
         lib-fasl
         lib-suffix
         lib-cgen
         lib-io
         lib-regex
         lib-rlist
         lib-sys
         lib-symbol
         lib-unsupported
         lib-char
         lib-scheme-compat
         )))

(define shared-bindings
   (foldr append null 
      (list 
         shared-default-modules 
         shared-extra-libs
         shared-misc)))

;; initial primops and special forms 

;; ,--- figure out how to be able to put this to a (owl core) library at the very beginning
;; v    or at least just drop everything other than this at the beginning
(define initial-environment-sans-macros
   (let
      ((primitive
         (λ (sym)
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
               (primitive 'mkred)
               (primitive 'mkblack)
               (primitive 'ff-bind)
               (primitive 'ff-toggle)
               (primitive 'ffcar)
               (primitive 'ffcdr)
               (primitive 'red?)
               (primitive 'listuple)
               (primitive 'fxband)         
               (primitive 'fx+)
               (primitive 'fxqr)
               (primitive 'fx*)
               (primitive 'fx-)
               (primitive 'fx<<)
               (primitive 'fx>>)
               (primitive 'ncons)
               (primitive 'ncar)
               (primitive 'ncdr)
               (primitive 'raw-mode)
               (primitive '_sleep)
               (primitive 'iomux)
               (primitive 'clock)
               (primitive 'time)
               (primitive 'sizeb)
               (primitive 'blit)
               (primitive 'getev)
               (primitive 'fill-rect)

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


(import-old lib-scheme-compat) ;; used in macros

,r "owl/repl.scm"

(import-old lib-repl)

(define initial-environment
   (bind-toplevel
      (library-import initial-environment-sans-macros
         '((owl primop)   ;; primop wrappers
           (owl syscall)  ;; calling mcp
           (owl defmac)   ;; standard toplevel macros
           )
         (λ (reason) (error "bootstrap import error: " reason))
         (λ (env exp) (error "bootstrap import requires repl: " exp)))))


;; todo: after there are a few more compiler options than one, start using -On mapped to predefined --compiler-flags foo=bar:baz=quux

(define command-line-rules
   (cl-rules
     `((help     "-h" "--help")
       (about    "-a" "--about")
       (version  "-v" "--version")
       (evaluate "-e" "--eval"     has-arg comment "evaluate expressions in the given string")
       (quiet    "-q" "--quiet"    comment "be quiet (default in non-interactive mode)")
       (run      "-r" "--run"      has-arg comment "run the last value of the given foo.scm with given arguments" terminal)
       (notes    "-n" "--notes"    comment "show notes from code (meaning \\n;; <label>:<text>\\n)")
       (load     "-l" "--load"     has-arg  comment "resume execution of a saved program state (fasl)")
       (output   "-o" "--output"   has-arg  comment "where to put compiler output (default auto)")
       (seccomp  "-S" "--seccomp"  comment "enter seccomp at startup or exit if it failed")
       (seccomp-heap     "-H" "--heap"     cook ,string->integer default "5"
         comment "allocate n megabytes of memory at startup if using seccomp")
       (output-format  "-x" "--output-format"   has-arg comment "output format when compiling (default auto)")
       (optimize "-O" "--optimize" cook ,string->integer comment "optimization level in C-compiltion (0-2)")
       (profile  "-p" "--profile" comment "Count calls when combined with --run (testing)")
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
      ((m/\.[a-z]+$/ path) ;; .scm, .lisp, .owl etc
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

(define (owl-run outcome args path profile?)
   (if outcome
      (tuple-case outcome
         ((ok val env)
            ;; be silent when all is ok
            ;; exit with 127 and have error message go to stderr when the run crashes
            (if profile?
               (try (λ () (profile (λ () (val args)) 30)) 127)
               (try (λ () (val args)) 127)))
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

(import-old lib-mcp)

,r "owl/threads.scm"

(import-old lib-threads)


;; pick usual suspects in a module to avoid bringing them to toplevel here
;; mainly to avoid accidentalaly introducing bringing generic functions here  

(define-module lib-usual-suspects
   (export usual-suspects)
   ; make sure the same bindings are visible that will be at the toplevel
   (import-old lib-generic)
   (import-old lib-suffix)
   (import-old lib-math)
   (import-old lib-random)
   (import-old lib-bisect)
   (import-old lib-io start-output-thread)
   (import-old lib-threads thread-controller)
   (import-old lib-sexp)
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
            ;print 
            mail interact 
            ;lib-rlist lib-iff
            iter iterr
            take keep remove 
            start-output-thread thread-controller
            ;sexp-parser 
            rand seed->rands)))

(import-old lib-usual-suspects usual-suspects)


;; handles $ ol -c stuff
(define (repl-compile compiler env path opts)
   (try
      (λ ()
         ;; evaluate in a thread to catch error messages here
         (let ((outcome (if (equal? path "-") (repl-port env stdin) (repl-file env path))))
            (tuple-case outcome
               ((ok val env)
                  (if (or 1 (function? val)) ;; <- testing, could allow individual module compilation also
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
                           (let ((opt (abs (get opts 'optimize 0))))
                              (cond
                                 ((>= opt 2) val) ;; compile everything to native extended primops for -O2
                                 ((= opt 1) usual-suspects) ;; compile some if -O1
                                 (else False)))) ;; otherwise use bytecode and plain vm
                           0)
                     (begin
                        (show "The last value should be a function of one value (the command line arguments), but it is instead " val)
                        2)))
               ((error reason env)
                  (print-repl-error
                     (list "Cannot compile" path "because " reason))
                  2)
               (else
                  (print-repl-error "Weird eval outcome.")
                  3))))
      False))

;;; handling of --notes
,r "owl/notes.scm"

(import-old lib-notes show-notes-of)

(import-old lib-dump load-fasl)

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
                     (put env '*interactive* True)))
                (seccomp?
                  (if (get dict 'seccomp False)
                     (let ((megs (get dict 'seccomp-heap 'bug)))
                        (seccomp megs) ;; <- process exits unless this succeeds 
                        True)
                     False)))
               (cond
                  ((get dict 'help False)
                     (print brief-usage-text)
                     (print-rules command-line-rules)
                     0)
                  ((get dict 'version False)
                     (show "Owl Lisp " *owl-version*)
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
                        (owl-run (try (λ () (repl-file env path)) False) (cons "ol" others) path
                           (get dict 'profile False))))
                  ((get dict 'evaluate False) => 
                     (λ (str)
                        (try-repl-string env str))) ;; fixme, no error reporting
                  ((null? others)
                     (repl-trampoline repl 
                        (module-set env '*seccomp* seccomp?)))
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
      (λ (sender message)
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

;; env → (ff of entry-object → symbol)
(define (collect-function-names env)
   (ff-fold
      (λ (collected name value)
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
   
(define (heap-entry symbol-list)
   (λ (codes) ;; all my codes are belong to codes
      (lets
         ((initial-names (ref (interact 'intern null) 4)) ;; names from current interner
          (interner-thunk (initialize-interner symbol-list codes initial-names)))
         (λ (vm-special-ops)
            (let ((compiler (make-compiler vm-special-ops)))
               ;; still running in the boostrapping system
               ;; the next value after evaluation will be the new repl heap
               (λ (vm-args)
                  ;; now we're running in the new repl 
                  (thread-controller thread-controller
                     (list
                        (tuple 'init
                           (λ () 
                              (fork-server 'repl
                                 (λ () 
                                    ;; get basic io running
                                    (start-base-threads)

                                    ;; breaks go to MCP
                                    (mcp-on-break)

                                    ;; repl needs symbol etc interning, which is handled by this thread
                                    (fork-server 'intern interner-thunk)

                                    ;; this thread will be removed later once 'intern does also the same
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
                                                ;(cons '*codes* (vm-special-ops->codes vm-special-ops))
                                                (cons '*owl-prompt* default-prompt)
                                                )))))))))
                     null 
                     False)))))))

;; todo: dumping with fasl option should only dump the fasl and only fasl


;;;
;;; Dump the new repl
;;;

;; note, one one could use the compiler of the currently running system, but using 
;; the rebuilt one here to make changes possible in 1 instead of 2 build cycles.
;; (this may be changed later)

(import-old lib-args)

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
                       (want-codes . True)
                       (want-native-ops . True)))
                  (choose-natives 
                     (get opts 'specialize "none")
                     heap-entry))
               0)))))
