;;;
;;; ol.scm: an Owl read-eval-print loop.
;;;

#| Copyright (c) 2012-2015 Aki Helin
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

;; check that (owl defmac) is indeed from last generation

(define build-start (time-ms))

; (import (owl defmac))

(mail 'intern (tuple 'flush)) ;; ask intern to forget all symbols it knows

; forget all other libraries to have them be reloaded and rebuilt

(define *libraries*
   (keep 
      (λ (lib) 
         (equal? (car lib) '(owl core)))
      *libraries*))

(import (owl defmac)) ;; reload default macros needed for defining libraries etc

;; forget everhything except these and core values (later list also them explicitly)
,forget-all-but (*vm-special-ops* *libraries* *codes* wait *args* stdin stdout stderr set-ticker run build-start)


;;;
;;; Time for a new REPL
;;;

;; this should later be just a sequence of imports followed by a fasl dump

(import (owl core))     ;; get special forms, primops and define-syntax

(import (owl defmac))   ;; get define, define-library, import, ... from the just loaded (owl defmac)

(define *interactive* #false) ;; be verbose 

(define *include-dirs* (list ".")) ;; now we can (import <libname>) and have them be autoloaded to current repl

(define *owl-names* #empty)

(import (owl syscall))

(import (owl primop))

(define *loaded* '())   ;; can be removed soon, was used by old ,load and ,require


;; shared parameters, librarize later or remove if possible

(define *owl-version* "0.1.9")
(define exit-seccomp-failed 2)    ;; --seccomp given but cannot do it
(define max-object-size #xffff)

(define owl-ohai "You see a prompt.")
(define owl-ohai-seccomp "You see a prompt. You feel restricted.")

(import (owl boolean))

(import (owl list))

(import (owl ff))

(import (only (owl iff)))

(import (owl math))

(import (owl list-extra))

(import (owl sort))

(import (owl math-extra))

(import (owl lazy))

(import (only (owl unicode) encode-point))

(import (owl string))

(import (owl vector))

(import (owl symbol))

(import (owl tuple))

(import (owl function))

(import (owl equal))

(import (owl rlist))

(import (owl render))
 
(import (only (owl queue))) ; just load it

(import (owl intern))

(import (owl io))

(import (owl parse))

(import (owl regex))

(import (owl sexp))

(define (ok? x) (eq? (ref x 1) 'ok))
(define (ok exp env) (tuple 'ok exp env))
(define (fail reason) (tuple 'fail reason))

(import (scheme cxr))

(import (scheme base))

(import (scheme case-lambda))

(import (owl env))

(import (owl gensym))

(import (owl bisect))

(import (owl macro))

(import (owl ast))

(import (owl fixedpoint))

(import (owl cps))

(import (owl alpha))

(import (owl thread))

(import (owl assemble))

(import (owl closure))

(import (owl compile))

(import (owl suffix))

(import (owl digest))

(define error-tag "err")

(define (error? x)
   (and (tuple? x)
      (eq? (ref x 1) error-tag)))

(import (owl time))

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
      (λ (envl mod)
         (append (ff->list mod) envl))))

;,load "owl/arguments.scm"
;,load "owl/random.scm"

(import (owl random))

(import (owl args))

(import (owl cgen))

(import (only (owl dump) make-compiler dump-fasl load-fasl))


(define compiler ; <- to compile things out of the currently running repl using the freshly loaded compiler
   (make-compiler *vm-special-ops*))

; path -> 'loaded | 'saved
(define (suspend path)
   (let ((maybe-world (syscall 16 #true #true)))
      (if (eq? maybe-world 'resumed)
         'loaded
         (begin
            (dump-fasl maybe-world path)
            'saved))))

(import (owl checksum))

(import (owl sys))

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
         #true)))

;; enter seccomp with at least n-megs of free space in heap, or stop the world (including all other threads and io)
(define (seccomp n-megs)
   ;; grow some heap space work working, which is usually necessary given that we can't 
   ;; get any more memory after entering seccomp
   (if (and n-megs (> n-megs 0))
      (ensure-free-heap-space n-megs))
   (or (sys-prim 10 #false #false #false)
      (begin
         (system-stderr "Failed to enter seccomp sandbox. 
You must be on a newish Linux and have seccomp support enabled in kernel.
")
         (halt exit-seccomp-failed))))


(define-library (owl char)
   (export char? char->integer integer->char)
   (import
      (owl defmac)
      (owl math))
   (begin
      (define char? number?)
      (define char->integer self)
      (define integer->char self)))

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
      (for-each (λ (p) (print*-to stdout  (list (car p) ":" (cdr p)))) most-used) ;; <- could use stderr later
      res))

;; implementation features, used by cond-expand
(define *features*
   (cons 
      (string->symbol (string-append "owl-lisp-" *owl-version*))
      '(owl-lisp r7rs exact-closed ratios exact-complex full-unicode immutable)))
      ;;          ^
      ;;          '-- to be a fairly large subset of at least, so adding this

(import (owl eval))

(import (owl base))

(import (owl server))

;; push it to libraries for sharing, replacing the old one
(define *libraries* 
   (cons 
      (cons '(owl core) *owl-core*)
      (keep (λ (x) (not (equal? (car x) '(owl core)))) *libraries*)))

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
      apply
      call/cc
      call-with-current-continuation
      display print-to print print* 
      render 
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
      byte-vector?
      string->symbol
      close-port flush-port
      read-file
      set-memory-limit 
      get-word-size
      get-memory-limit
      checksum
      string->sexp
      profile
      read read-ll
      *features*
      *include-dirs*
      *libraries*      ;; all currently loaded libraries
      ))


;,load "owl/test.scm"     ; a simple algorithm equality/benchmark tester
;,load "owl/sys.scm"      ; more operating system interface

(define shared-bindings shared-misc)

(define initial-environment-sans-macros
   (fold 
      (λ (env pair) (env-put-raw env (car pair) (cdr pair)))
      *owl-core*
      shared-bindings))
     
;; owl core needed before eval

;; toplevel can be defined later

(define initial-environment
   (bind-toplevel
      (library-import initial-environment-sans-macros
         '((owl base))
         (λ (reason) (error "bootstrap import error: " reason))
         (λ (env exp) (error "bootstrap import requires repl: " exp)))))

;; todo: after there are a few more compiler options than one, start using -On mapped to predefined --compiler-flags foo=bar:baz=quux

(define command-line-rules
   (cl-rules
     `((help     "-h" "--help")
       (about    "-a" "--about")
       (version  "-v" "--version")
       (evaluate "-e" "--eval"     has-arg comment "evaluate given expression and print result")
       (test     "-t" "--test"     has-arg comment "evaluate given expression exit with 0 unless the result is #false")
       (quiet    "-q" "--quiet"    comment "be quiet (default in non-interactive mode)")
       (run      "-r" "--run"      has-arg comment "run the last value of the given foo.scm with given arguments" terminal)
       (load     "-l" "--load"     has-arg  comment "resume execution of a saved program state (fasl)")
       (output   "-o" "--output"   has-arg  comment "where to put compiler output (default auto)")
       (seccomp  "-S" "--seccomp"  comment "enter seccomp at startup or exit if it failed")
       (seccomp-heap     "-H" "--heap"     cook ,string->number default "5"
         comment "allocate n megabytes of memory at startup if using seccomp")
       (output-format  "-x" "--output-format"   has-arg comment "output format when compiling (default auto)")
       (optimize "-O" "--optimize" cook ,string->number comment "optimization level in C-compiltion (0-2)")
       ;(profile  "-p" "--profile" comment "Count calls when combined with --run (testing)")
       ;(debug    "-d" "--debug" comment "Define *debug* at toplevel verbose compilation")
       ;(linked  #false "--most-linked" has-arg cook ,string->integer comment "compile most linked n% bytecode vectors to C")
       (no-threads #false "--no-threads" comment "do not include threading and io to generated c-code")
       )))

(define brief-usage-text "Usage: ol [args] [file] ...")

(define error-usage-text "ol -h helps.")

;; repl-start, thread controller is now runnig and io can be 
;; performed. check the vm args what should be done and act 
;; accordingly.

; note, return value is not the owl return value. it comes
; from thread controller after all threads have finished.

(define (memory-limit-ok? n w)
   (cond
      ((< n 1) (print "Too little memory allowed.") #false)
      ((and (= w 4) (> n 4096)) (print "This is a 32-bit executable, so you cannot use more than 4096Mb of memory.") #false)
      ((and (= w 8) (> n 65536)) (print "65536 is as high as you can go.") #false)
      (else #true)))

(define (maybe-set-memory-limit args)
   (let ((limit (get args 'memlimit #false)))
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
            (print-to stderr (verbose-vm-error empty opcode a b))
            fail-val)
         ((error cont reason info)
            ; note, these could easily be made resumable by storing cont
            (print-to stderr
               (list->string
                  (foldr render '(10) (list "error: " reason info))))
            fail-val)
         (else is bad ;; should not happen
            (print-to stderr (list "que? " bad))
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
               (list "ol: cannot run" path "because there was an error during loading:" reason))
            2))
      1))

(define about-owl 
"Owl Lisp -- a functional scheme for world domination
Copyright (c) 2015 Aki Helin
Check out https://github.com/aoh/owl-lisp for more information.")





;;;
;;; MCP, master control program and the thread controller
;;;

; special keys in mcp state 


;; pick usual suspects in a module to avoid bringing them to toplevel here
;; mainly to avoid accidentalaly introducing bringing generic functions here  

(define-library (owl usuals)

   (export usual-suspects)
   ; make sure the same bindings are visible that will be at the toplevel

   (import
      (owl defmac)
      (owl suffix)
      (owl math)
      (owl random)
      (owl bisect)
      (owl thread)
      (owl list)
      (owl list-extra)
      (owl syscall)
      (owl vector)
      (owl sort)
      (owl equal)
      (owl ff)
      (owl sexp))

   (begin
      ; commonly needed functions 
      (define usual-suspects
         (list
               put get del ff-fold fupd
               - + * /
               div gcd ediv
               << < <= = >= > >> 
               equal? has? mem
               band bor bxor
               sort
               ; suffix-array bisect
               fold foldr for map reverse length zip append unfold
               lref lset iota
               ;vec-ref vec-len vec-fold vec-foldr
               ;print 
               mail interact 
               take keep remove 
               thread-controller
               ;sexp-parser 
               rand seed->rands
               ))))

(import (owl usuals))


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
                              ((get opts 'output #false) => (λ (given) given)) ; requested with -o
                              ((equal? path "-") path) ; stdin → stdout
                              (else (c-source-name path)))
                           ;; inverse option on command line, add here if set
                           (if (get opts 'no-threads #false)
                              opts
                              (put opts 'want-threads #true))
                           ;; to be customizable via command line opts
                           (let ((opt (abs (get opts 'optimize 0))))
                              (cond
                                 ((>= opt 2) val) ;; compile everything to native extended primops for -O2
                                 ((= opt 1) usual-suspects) ;; compile some if -O1
                                 (else #false)))) ;; otherwise use bytecode and plain vm
                           0)
                     (begin
                        (print "The last value should be a function of one value (the command line arguments), but it is instead " val)
                        2)))
               ((error reason env)
                  (print-repl-error
                     (list "Cannot compile" path "because " reason))
                  2)
               (else
                  (print-repl-error "Weird eval outcome.")
                  3))))
      #false))


(define (try-load-state path args)
   (let ((val (load-fasl path #false)))
      (if (function? val)
         (try (λ () (val (cons path args))) 127)
         (begin
            (print "failed to load dump from " path)
            1))))
  
;; -> vm exit with 0 on success, n>0 on error
(define (try-repl-string env str)
   (tuple-case (repl-string env str)
      ((ok val env)
         (exit-owl
            (if (print val) 0 127)))
      ((error reason partial-env)
         (print-repl-error 
            (list "An error occurred while evaluating:" str reason))
         (exit-owl 1))
      (else
         (exit-owl 2))))

;; exit with 0 if value is non-false, 1 if it's false, 127 if error
(define (try-test-string env str)
   (tuple-case (repl-string env str)
      ((ok val env)
         (exit-owl (if val 0 1)))
      ((error reason partial-env)
         (print-repl-error 
            (list "An error occurred while evaluating:" str reason))
         (exit-owl 127))
      (else
         (exit-owl 127))))

;; say hi if interactive mode and fail if cannot do so (the rest are done using 
;; repl-prompt. this should too, actually)
(define (greeting env seccomp?)
   (if (env-get env '*interactive* #f)
      (or
         (and
            (print (if seccomp? owl-ohai-seccomp owl-ohai))
            (display "> "))
         (halt 127))))

;; todo: this should probly be wrapped in a separate try to catch them all
; ... → program rval going to exit-owl
(define (repl-start vm-args repl compiler env)
   (or
      (process-arguments (cdr vm-args) command-line-rules error-usage-text
         (λ (dict others)
            (lets 
               ((env ;; be quiet automatically if any of these are set
                  (if (fold (λ (is this) (or is (get dict this #false))) #false '(quiet test evaluate run output output-format))
                     (env-set env '*interactive* #false)
                     (env-set env '*interactive* #true)))
                (env ;; maybe set debug causing (owl eval) to print intermediate steps
                  (if (getf dict 'debug)
                     (env-set env '*debug* #true)
                     env))
                (seccomp?
                  (if (get dict 'seccomp #false)
                     (let ((megs (get dict 'seccomp-heap 'bug)))
                        (seccomp megs) ;; <- process exits unless this succeeds 
                        #true)
                     #false)))
               (cond
                  ((getf dict 'help)
                     (print brief-usage-text)
                     (print-rules command-line-rules)
                     0)
                  ((getf dict 'version)
                     (print "Owl Lisp " *owl-version*)
                     0)
                  ((getf dict 'about) (print about-owl) 0)
                  ((getf dict 'load) =>
                     (λ (path) (try-load-state path others)))
                  ((or (getf dict 'output) (getf dict 'output-format))
                     (if (< (length others) 2) ;; can take just one file or stdin
                        (repl-compile compiler env 
                           (if (null? others) "-" (car others)) dict)
                        (begin
                           (print "compile just one file for now please: " others)
                           1)))
                  ((getf dict 'run) =>
                     (λ (path)
                        (owl-run (try (λ () (repl-file env path)) #false) (cons "ol" others) path
                           (get dict 'profile #false))))
                  ((getf dict 'evaluate) => 
                     (λ (str)
                        (try-repl-string env str))) ;; fixme, no error reporting
                  ((getf dict 'test) => 
                     (λ (str)
                        (try-test-string env str)))
                  ((null? others)
                     (greeting env seccomp?)
                     (repl-trampoline repl 
                        (env-set env '*seccomp* seccomp?)))
                  (else
                     ;; load the given files
                     (define input
                        (foldr (λ (path tail) (ilist ',load path tail)) null others))
                     (tuple-case (repl env input)
                        ((ok val env)  
                           0)
                        ((error reason partial-env)
                           (print-repl-error reason)
                           1)))))))
      2))



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

(define (heap-entry symbol-list)
   (λ (codes) ;; all my codes are belong to codes
      (lets
         ((initial-names *owl-names*)
          (interner-thunk (initialize-interner symbol-list codes)))
         (λ (vm-special-ops)
            (let ((compiler (make-compiler vm-special-ops)))
               ;; still running in the boostrapping system
               ;; the next value after evaluation will be the new repl heap
               (λ (vm-args)
                  ;; now we're running in the new repl 
                  (start-thread-controller
                     (list
                        (tuple 'init
                           (λ () 
                              (fork-server 'repl
                                 (λ () 
                                    ;; get basic io running
                                    (start-base-threads)

                                    ;; repl needs symbol etc interning, which is handled by this thread
                                    (fork-server 'intern interner-thunk)

                                    ;; set a signal handler which stop evaluation instead of owl 
                                    ;; if a repl eval thread is running
                                    (set-signal-action repl-signal-handler)

                                    (exit-owl 
                                       (repl-start vm-args repl compiler
                                          (fold 
                                             (λ (env defn)
                                                (env-set env (car defn) (cdr defn)))
                                             initial-environment
                                             (list
                                                (cons '*owl* (directory-of (car vm-args)))
                                                (cons '*args* vm-args)
                                                (cons 'dump compiler)
                                                (cons '*owl-version* *owl-version*)
                                                ;(cons '*owl-metadata* *owl-metadata*)
                                                (cons '*owl-names* initial-names)
                                                (cons 'eval exported-eval)
                                                (cons 'render render) ;; can be removed when all rendering is done via libraries
                                                (cons '*vm-special-ops* vm-special-ops)
                                                ;(cons '*codes* (vm-special-ops->codes vm-special-ops))
                                                )))))))))
                     null)))))))

;; todo: dumping with fasl option should only dump the fasl and only fasl


;;;
;;; Dump the new repl
;;;

;; note, one one could use the compiler of the currently running system, but using 
;; the rebuilt one here to make changes possible in 1 instead of 2 build cycles.
;; (this may be changed later)

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
      (else (print "Bad native selection: " str))))

;;;
;;; Step 3 - profit
;;;

(print "Code loaded at " (- (time-ms) build-start) "ms.")

(λ (args)
   (process-arguments (cdr args) command-line-rules "you lose"
      (λ (opts extra)
         (cond
            ((not (null? extra))
               (print "Unknown arguments: " extra)
               1)
            (else
               (compiler heap-entry "unused historical thingy"
                  (list->ff
                     `((output . ,(get opts 'output 'bug))
                       (want-symbols . #true)
                       (want-codes . #true)
                       (want-native-ops . #true)))
                  (choose-natives 
                     (get opts 'specialize "none")
                     heap-entry))
               (print "Output written at " (- (time-ms) build-start) "ms.")
               0)))))

