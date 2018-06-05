;;;
;;; ol.scm: an Owl read-eval-print loop.
;;;

#| Copyright (c) 2012-2018 Aki Helin
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

(mail 'intern (tuple 'flush)) ;; ask symbol interner to forget all symbols it knows

(define *libraries* '()) ;; clear loaded libraries

(import (owl defmac)) ;; reload default macros needed for defining libraries etc

;; forget everhything except these and core values (later list also them explicitly)
,forget-all-but (quote *libraries* _branch _define rlambda)

;; --------------------------------------------------------------------------------

(import (owl defmac))   ;; get define, define-library, import, ... from the just loaded (owl defmac)

(define *interactive* #false) ;; be verbose
(define *include-dirs* '(".")) ;; now we can (import <libname>) and have them be autoloaded to current repl
(define *owl-names* #empty)
(define *owl-version* "0.1.16")

(import
   (owl intern)
   (owl env)
   (owl ast)
   (owl thread)
   (owl args)
   (only (owl dump) make-compiler load-fasl)
   (only (owl primop) bind mkt)
   (owl eval)
   (owl repl)
   (owl base)
   (owl variable))

;; implementation features, used by cond-expand
(define *features*
   (cons
      (string->symbol (string-append "owl-lisp-" *owl-version*))
      '(owl-lisp r7rs exact-closed ratios exact-complex full-unicode immutable)))

(define initial-environment
   (bind-toplevel
      (env-fold env-put-raw
         *owl-core*
         (cdr (assoc '(owl base) *libraries*)))))

(define (path->string path)
   (let ((data (file->vector path)))
      (if data
         (bytes->string (vector->list data))
         #false)))

(define command-line-rules
   (cl-rules
     `((help     "-h" "--help")
       (about    "-a" "--about")
       (version  "-v" "--version")
       (evaluate "-e" "--eval"     has-arg comment "evaluate given expression and print result")
       (test     "-t" "--test"     has-arg comment "evaluate given expression exit with 0 unless the result is #false")
       (quiet    "-q" "--quiet"    comment "be quiet (default in non-interactive mode)")
       (run      "-r" "--run"      has-arg comment "run the last value of the given foo.scm with given arguments" terminal)
       (load     "-l" "--load"     has-arg comment "resume execution of a saved program state saved with suspend")
       (output   "-o" "--output"   has-arg comment "where to put compiler output (default auto)")
       (output-format  "-x" "--output-format"   has-arg comment "output format when compiling (default auto)")
       (optimize "-O" "--optimize" cook ,string->number comment "optimization level in C-compilation (0-2)")
       (custom-runtime "-R" "--runtime"
          cook ,path->string
          comment "use a custom runtime in C compilation")
       ;(interactive "-i" "--interactive" comment "use builtin interactive line editor")
       ;(debug    "-d" "--debug" comment "Define *debug* at toplevel verbose compilation")
       ;(linked  #false "--most-linked" has-arg cook ,string->integer comment "compile most linked n% bytecode vectors to C")
       (bare #false "--bare" comment "output the bare fasl-encoded result"))))

(define brief-usage-text "Usage: ol [args] [file] ...")

(define error-usage-text "ol -h helps.")

(define (c-source-name path)
   (cond
      ((m/\.[a-z]+$/ path) ;; .scm, .lisp, .owl etc
         (s/\.[a-z]+$/.c/ path))
      (else
         (string-append path ".c"))))

(define (owl-run outcome args path)
   (if outcome
      (tuple-case outcome
         ((ok val env)
            ;; be silent when all is ok
            ;; exit with 126 and have error message go to stderr when the run crashes
            (try (λ () (val args)) 126))
         ((error reason env)
            (print-repl-error
               (list "ol: cannot run" path "because there was an error during loading:" reason))
            2))
      1))

(define about-owl
"Owl Lisp -- a functional scheme
Copyright (c) Aki Helin
Check out https://gitlab.com/owl-lisp/owl for more information.")


(define usual-suspects
   (list
         put get del ff-fold fupd
         - + * /
         quotient gcd ediv
         << < <= = >= > >>
         equal? memq member
         band bor bxor
         sort
         ; suffix-array bisect
         fold foldr map reverse length zip append unfold
         list-ref lset iota
         ;vec-ref vec-len vec-fold vec-foldr
         ;print
         mail interact
         take filter remove
         thread-controller
         uncons lfold lmap
         rand seed->rands
         ))

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
                              ((get opts 'output #false) => self) ; requested with -o
                              ((equal? path "-") path) ; stdin → stdout
                              (else (c-source-name path)))
                           ;; inverse option on command line, add here if set
                           (if (get opts 'bare #false)
                              (put opts 'no-utf8-decode #true)
                              (put opts 'want-threads #true))
                           ;; to be customizable via command line opts
                           (let ((opt (abs (get opts 'optimize 0))))
                              (cond
                                 ((>= opt 2) val) ;; compile everything to native extended primops for -O2
                                 ((= opt 1) usual-suspects) ;; compile some if -O1
                                 (else #false))) ;; otherwise use bytecode and plain vm
                           (getf opts 'custom-runtime))
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
         (try (λ () (val (cons path args))) 126)
         (begin
            (print "failed to load dump from " path)
            1))))

;; -> vm exit with 0 on success, n>0 on error
(define (try-repl-string env str)
   (tuple-case (repl-string env str)
      ((ok val env)
         (exit-owl
            (if (print val) 0 126)))
      ((error reason partial-env)
         (print-repl-error
            (list "An error occurred while evaluating:" str reason))
         (exit-owl 1))
      (else
         (exit-owl 2))))

;; exit with 0 if value is non-false, 1 if it's false, 126 if error
(define (try-test-string env str)
   (tuple-case (repl-string env str)
      ((ok val env)
         (exit-owl (if val 0 1)))
      ((error reason partial-env)
         (print-repl-error
            (list "An error occurred while evaluating:" str reason))
         (exit-owl 126))
      (else
         (exit-owl 126))))

(define owl-ohai "You see a prompt.")

;; say hi if interactive mode and fail if cannot do so (the rest are done using
;; repl-prompt. this should too, actually)
(define (greeting env)
   (if (env-get env '*interactive* #f)
      (or
         (and
            (print owl-ohai)
            (display "> "))
         (halt 126))))

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
                     env)))
               (cond
                  ((getf dict 'help)
                     (print brief-usage-text)
                     (print-rules command-line-rules)
                     0)
                  ((getf dict 'version)
                     (print "Owl Lisp " *owl-version*)
                     0)
                  ((getf dict 'about) (print about-owl) 0)
                  ((getf dict 'load) => (C try-load-state others))
                  ((or (getf dict 'output) (getf dict 'output-format))
                     (if (< (length others) 2) ;; can take just one file or stdin
                        (repl-compile compiler env
                           (if (null? others) "-" (car others)) dict)
                        (begin
                           (print "compile just one file for now please: " others)
                           1)))
                  ((getf dict 'run) =>
                     (λ (path)
                        (owl-run (try (λ () (repl-file env path)) #false) (cons "ol" others) path)))
                  ((getf dict 'evaluate) => (H try-repl-string env)) ;; FIXME: no error reporting
                  ((getf dict 'test) => (H try-test-string env))
                  ((null? others)
                     (greeting env)
                     (repl-trampoline repl
                        (-> env
                           ;(env-set '*line-editor* (getf dict 'interactive))
                           )))
                  (else
                     ;; load the given files
                     (define input
                        (foldr (λ (path tail) (ilist ',load path tail)) null others))
                     (tuple-case (repl (env-set env '*interactive* #false) input)
                        ((ok val env)
                           0)
                        ((error reason partial-env)
                           (print-repl-error reason)
                           1)))))))
      2))

(define (directory-of path)
   (runes->string
      (reverse
         (or
            (memq #\/ (reverse (string->runes path)))
            null))))

(define compiler ; <- to compile things out of the currently running repl using the freshly loaded compiler
   (make-compiler #empty))

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
                              (thread 'repl
                                 (let ((state (make-variable '*state* #empty)))
                                    ;; get basic io running
                                    (start-base-threads)

                                    ;; store initial state values
                                    (state 'call
                                       (λ (st)
                                          (-> st
                                             (put 'command-line-arguments vm-args)
                                             (put 'features *features*)
                                             )))

                                    ;; repl needs symbol etc interning, which is handled by this thread
                                    (thunk->thread 'intern interner-thunk)

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
                                                (cons '*features* *features*)
                                                (cons '*include-dirs* *include-dirs*) ;; todo: add command line flag
                                                (cons '*libraries* *libraries*)
                                                (cons 'dump compiler)
                                                (cons '*owl-version* *owl-version*)
                                                (cons '*owl-names* initial-names)
                                                (cons 'eval exported-eval)
                                                (cons 'render render)
                                                (cons '*vm-special-ops* vm-special-ops)
                                                (cons '*state* state))))))))))
                     null)))))))


(define command-line-rules
   (cl-rules
      `((output "-o" "--output" has-arg comment "output path")
        (specialize "-s" "--specialize" has-arg comment "vm extensions (none, some, all)"))))

(define (choose-natives str all)
   (cond
      ((equal? str "none") null)
      ((equal? str "some") usual-suspects)
      ((equal? str "all") all)
      (else (print "Bad native selection: " str))))

(import (owl sys))
(print-to stderr "writes: " (>> (get-heap-bytes-written) 20) "MWords")
(print-to stderr "max live: " (>> (get-heap-max-live) 10) "KB")

(λ (args)
   (process-arguments (cdr args) command-line-rules "you lose"
      (λ (opts extra)
         (cond
            ((null? extra)
               (compiler heap-entry "unused historical thingy"
                  (list->ff
                     `((output . ,(get opts 'output 'bug))
                       (want-symbols . #true)
                       (want-codes . #true)
                       (want-native-ops . #true)))
                  (choose-natives
                     (get opts 'specialize "none")
                     heap-entry))
               0)
            (else
               (print "Unknown arguments: " extra)
               1)))))
