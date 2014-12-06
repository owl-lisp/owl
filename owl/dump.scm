;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Heap dumper (for ovm) <- to be renamed to lib-compile later, as this is starting to become more like a compiler entry point
;;;

(define-library (owl dump)

   (export 
      make-compiler    ; ((make-compiler extra-insts) entry path opts native) 
      dump-fasl 
      load-fasl)

   (import 
      (owl defmac)
      (owl fasl)
      (owl list)
      (owl sort)
      (owl ff)
      (owl symbol)
      (owl vector)
      (owl equal)
      (owl function)
      (owl list-extra)
      (owl string)
      (owl io)
      (owl math)
      (owl render)
      (owl lazy)
      (owl cgen)
      (only (owl syscall) error mail exit-owl)
      (only (owl env) signal-halt signal-tag)
      (only (owl unicode) utf8-decode)
      (only (owl thread) start-thread-controller)
      (only (owl queue) qnull))

   (begin
      ;;; 
      ;;; Symbols must be properly interned in a repl.
      ;;; 

      (define (symbols-of node)

         (define tag (list 'syms))

         (define (walk trail node)
            (cond
               ((immediate? node) trail)
               ((get trail node #false) trail)
               ((symbol? node) 
                  (let ((trail (put trail node 1)))
                     (put trail tag 
                        (cons node (get trail tag null)))))
               ((raw? node) trail)
               (else
                  (fold walk 
                     (put trail node #true)
                     (tuple->list node)))))
         (define trail
            (walk (put empty tag null) node))

         (get 
            (walk (put empty tag null) node)
            tag null))

      ;; FIXME - fails with variable fixnum size and usual vectors
      (define (file->string path)
         (bytes->string
            (vec-iter
               (let ((vec (file->vector path)))
                  (if vec 
                     vec
                     (error "Unable to load: " path))))))

      ;; todo: compress the rts source in heap
      ;; todo: include rts source into lib-ccomp instead of keeping it in a separate file 
      (define rts-source 
         (file->string "c/ovm.c"))

      ; str -> str' | #false
      (define (utf8-decode-string str)
         (let ((cps (utf8-decode (lfoldr cons '() (str-iterr str)))))
            (if cps
               (list->string cps)
               (begin
                  ;; don't use normal IO, since it may not yet be running.
                  (system-println "warning: bad UTF-8 in command line argument")
                  ;; return the string although it has broken data. this allows 
                  ;; paths with broken (or intentional) funny encodings to be 
                  ;; passed as command line arguments.
                  str))))

      ;; walk over raw string using primops and look for bytes > 127
      (define (high-point? str pos)
         (if (lesser? (refb str pos) 128)
            (lets ((pos underflow? (fx- pos 1)))
               (if underflow?
                  #false
                  (high-point? str pos)))
            #true))
           
      ;; utf-8 decode if necessary (avoids some constant overhead, which is useful if there are 2 quadriollion args)
      (define (maybe-utf8-decode str)
         (let ((n (sizeb str))) ;; <- command line args are raw blocks so primitive lenb is ok
            (cond
               ((eq? n 0) str)
               ((high-point? str (- n 1)) 
                  (utf8-decode-string str))
               (else str))))

      (define (with-decoded-args prog)
         (λ (vm-args) 
            (prog 
               (map utf8-decode-string vm-args))))

      ; notice that decoding brings bignum math as a dependency to all dumped heaps

      (define (width x)
         (cond
            ((< x 10) 2)
            ((< x 100) 3)
            (else 4)))

      (define (render-byte-array bytes pos)
         (cond
            ((> pos 75)
               (cons 10
                  (render-byte-array bytes 0)))
            ((null? (cdr bytes))
               (render (car bytes) null))
            (else
               (let ((this (car bytes)))
                  (render this 
                     (cons 44
                        (render-byte-array (cdr bytes) (+ pos (width this)))))))))

      ;; fixme: no way to synchronize io and/or get check success of writing. should add at some point..
      (define (dump-data data path)
         (let ((port (open-output-file path)))
            (if port
               (let loop ((data data))
                  (cond
                     ((null? data) 
                        (close-port port)
                        #true)
                     ((pair? data)
                        (if (write-byte-vector port (car data))
                           (loop (cdr data))
                           #false))
                     (else 
                        (loop (data)))))
               #false)))

      (define (dump-fasl obj path)
         (dump-data (fasl-encode-stream obj (lambda (x) x)) path))
      
      ;; fixme: sould be (load-fasl <path> <fail>)
      (define (load-fasl path fval)
         (let ((port (open-input-file path)))
            (if port
               (let ((val (fasl-decode (port->byte-stream port) fval)))
                  (close-port port)
                  val)
               fval)))

      (define (render-native-ops nops)
         (runes->string
            (foldr render null
               (ff-fold
                  (λ (tl func info)
                     (lets ((opcode new-func c-code info))
                        ;; render code if there (shared users do not have it)
                        (if c-code 
                           ;; all of these end to an implicit goto apply
                           (ilist "      case " opcode ":" c-code "break; /* " func " */
   " tl)
                           tl)))
                  null nops))))
            

      ; nodes = ((func . #(opcode warpper src)) ...)

      ; obj → (ff of #[bytecode] → #(native-opcode native-using-bytecode c-fragment|#false))
      (define (choose-native-ops entry extras)
         (let ((all (objects-below entry)))
            (if (null? all)
               (begin
                  ;(print " - no native instructions selected")
                  (list->ff all))
               (let loop ((code 0) (obs all) (out null)) ;; <- can start at 0 after cleaning up the old code
                  (cond
                     ((null? obs)
                        (list->ff out))
                     ((= code 65536)
                        ;; would need a larger wrappers, but will not likely be necessary
                        ;; could be added as (+ (<< 1 6) 0) -> read 4 bytes
                        (error "too many native opcodes." 
                           "report this as an issue if this happens for a real program."))
                     ((compile-to-c (car obs) extras) =>
                        (λ (src)
                           (lets 
                              ((wrapper (raw (list 0 (>> code 8) (band code 255)) type-bytecode #false)))
                              (loop (+ code 1) (cdr obs)
                                 (cons (cons (car obs) (tuple code wrapper src)) out)))))
                     (else
                        (loop code (cdr obs) out)))))))

      ; obj -> fixnum|#false
      (define (extended-opcode obj)
         (if (and (bytecode? obj) (eq? 0 (refb obj 0)))
            (+ (<< (refb obj 1) 8) (refb obj 2))
            #false))

      (define (show-func val)
         (cons 'bytecode
            (map (λ (p) (refb val p)) (iota 0 1 (sizeb val)))))

      ; native-ops → (obj → obj')
      ;; fixme: rewrite...
      (define (make-native-cook native-ops extras)
         (λ (obj)
            (cond
               ;; if chosen to be a macro instruction in the new vm, replace with new bytecode calling it
               ((get native-ops obj #false) =>
                  (λ (vals) 
                     ; write a reference to the wrapper function instead of the original bytecode
                     (ref vals 2)))
               ;; if this is a macro instruction in the current system, convert back to vanilla bytecode, or the 
               ;; target machine won't understand this
               ((extended-opcode obj) =>
                  (λ (opcode)
                     ;(print " * mapping superinstruction back to to bytecode: " opcode)
                     (or (get extras opcode #false)
                        (error "could not find bytecode for opcode " opcode))))
               (else obj))))

      ;; make a ff of opcode → original-bytecode. for example the repl 
      ;; needs to know what the plain bytecode of each compiled version is in 
      ;; order to for example build a new vm with possibly other set of native ops.

      (define (clone-code bc extras) ;; clone to not be eq? with the ones being compiled
         (cond
            ((extended-opcode bc) =>
               ; the opcodes must be described with vanilla bytecode 
               ; this does not belong here...
               (λ (opcode)
                  (let ((original (get extras opcode #false)))
                     (if original 
                        (clone-code original extras)
                        (error "bug: no original code found for superinstruction " opcode)))))
            (else
               (let ((bytes (map (λ (p) (refb bc p)) (iota 0 1 (sizeb bc)))))
                  (if (eq? (cadr bytes) 0)
                     (error "bug: vm speciazation instruction probably referencing code from current vm: " bytes))
                  (raw bytes type-bytecode #false))))) ; <- reallocate it

      (define (original-sources native-ops extras)
         (ff-fold
            (λ (sources bytecode info)
               (lets ((opcode wrapper c-code info))
                  (put sources opcode 
                     (clone-code bytecode extras))))
            empty native-ops))


      ;;;
      ;;; Choosing frequently referenced code vectors
      ;;;

      (define (code-refs seen obj)
         (cond
            ((immediate? obj) (values seen empty))
            ((bytecode? obj)
               (values seen (put empty obj 1)))
            ((get seen obj #false) =>
               (λ (here) (values seen here)))
            (else
               (let loop ((seen seen) (lst (tuple->list obj)) (here empty))
                  (if (null? lst)
                     (values (put seen obj here) here)
                     (lets ((seen this (code-refs seen (car lst))))
                        (loop seen (cdr lst)
                           (ff-union this here +))))))))

      ; ob → ((nrefs . ob) ..) 
      (define (all-code-refs ob)
         (lets ((refs this (code-refs empty ob)))
            (ff-fold (λ (out x n) (cons (cons n x) out)) null this)))

      ;; _ → ((bytecode . bytecode) ...)
      (define (codes-of ob) 
         (lets ((refs this (code-refs empty ob)))
            (ff-fold (λ (out x n) (cons (cons x x) out)) null this)))

      ;; ob percent → (codevec ...)
      (define (most-linked-code ob perc)
         (print "Picking most shared code vectors:")
         (lets 
            ((all (all-code-refs ob))
             (sorted (sort (λ (a b) (> (car a) (car b))) all))
             (_ (print " - total code vectors " (length sorted)))
             (topick (floor (* (/ perc 100) (length sorted)))))
            (print " - taking " topick)
            (map cdr (take sorted topick))))

      ;; todo: move with-threading to lib-threads and import from there
      (define (with-threading ob)
         (λ (args)
            (start-thread-controller
               (list
                  (tuple 'root
                     (λ ()
                        (start-base-threads)    ;; get basic io running
                        (exit-owl (ob args))))) ;; exit thread scheduler with exit value of this thread (if it doesn't crash)
               (list 
                  (cons signal-tag signal-halt)
                  (cons 'root qnull)))))   ;; the init thread usually needs a mailbox

      (define (cook-format str)
         (cond
            ((equal? str "c") 'c)
            ((equal? str "fasl") 'fasl)
            (else #false)))

      ; → c | fasl (| s)
      (define (choose-output-format opts maybe-path)
         (lets ((path (get opts 'output maybe-path)))
            (if (string? path)
               (cook-format (s/^.*\.([a-z]+)$/\1/ path))
               #false)))


      ; obj → (ff of #[bytecode] → #(native-opcode native-using-bytecode c-fragment))
      ; dump entry object to path, or stdout if path is "-"

      (define (make-compiler extras)
         (λ (entry path opts native) ; <- this is the usual compile-owl 
            (lets
               ((path (get opts 'output "-")) ; <- path argument deprecated
                (format 
                  ;; use given format (if valid) or choose using output file suffix
                  (or (cook-format (get opts 'output-format #false))
                     (choose-output-format opts path)))

                ;(_ (print " - output format " format))
                (entry ;; start threading if requested (note how this affects the other args)
                  (if (get opts 'want-threads #false) 
                     (with-threading entry)
                     entry)) ; <- continue adding this next
               
                (entry ;; pass symbols to entry if requested (repls need this)
                  (if (get opts 'want-symbols #false) 
                     (entry (symbols-of entry))
                     entry))
                  
                (entry ;; pass code vectors to entry if requested (repls need this)
                  (if (get opts 'want-codes #false) 
                     (entry (codes-of entry))
                     entry))
                  
                ;; fixme: allow a compiler arg to convert this point fully to native to get also the thread scheduler compiled
                ;(extra-native ;; choose 10% of most frequently linked code unless compiling a fasl image
                ;  (cond
                ;     ((eq? format 'fasl) null) ; fasl -> no natives
                ;     ((eq? entry native) null)  ; everything native anyway
                ;     (else (most-linked-code entry 10)))) ; pick most linked 10% (testing)

                (native-ops ;; choose which bytecode vectors to add as extended vm instructions
                  (choose-native-ops (if (get opts 'native #false) entry native) extras))

                (entry ;; possibly tell the entry function about extended opcodes 
                  (if (get opts 'want-native-ops #false)
                     ;; entry is actually ((ff of extended-opcode → vanilla-bytecode) → entry)
                     (entry (original-sources native-ops extras))
                     entry))

                (entry ;; possibly add code to utf-8 decode command line arguments
                  (if (get opts 'no-utf8-decode #false)
                     (begin
                        (print " - not decoding command line arguments")
                        entry)
                     (with-decoded-args entry)))

                (native-cook ;; make a function to possibly rewrite bytecode during save (usually to native code wrappers)
                   (make-native-cook native-ops extras))
      
                (bytes ;; encode the resulting object for saving in some form
                  (fasl-encode-cooked entry native-cook))

                (port ;; where to save the result
                  (if (equal? path "-")
                     stdout
                     (open-output-file path))))
               (cond
                  ((not port)
                     (print "Could not open path for writing")
                     #false)
                  ((not format)
                     (print "I do not know how to write that.")
                     (print "Use -o file.c, -o file.fasl, or defined format with -x c or -x fasl")
                     #false)
                  ((eq? format 'fasl) ;; just save the fasl dump
                     (write-bytes port bytes)
                     (close-port port)
                     #true)
                  ((eq? format 'c) 
                     (write-bytes port ;; output fasl-encoded heap as an array
                        (append
                           (string->bytes "unsigned char heap[] = {")
                           (render-byte-array bytes 24)))
                     ;; dump also a fasl if requested
                     (write-bytes port (string->bytes "};\n "))
                     ;; dump ovm.c and replace /* AUTOGENERATED INSTRUCTIONS */ with new native ops (if any)
                     (write-bytes port 
                        (string->bytes 
                           (str-replace rts-source
                              "/* AUTOGENERATED INSTRUCTIONS */"
                              (render-native-ops native-ops))))
                     
                     ;; done, now just gcc -O2 -o foo <path>
                     (close-port port))))))

))


