;;
;; count and signal worst case memory use in bytecode
;;

; most bytecode fragments always need a small finite space to run. thus
; to make gc checks much sparser, and generated C code more compact, the 
; vm only checks at each function application site that threre are n 
; (likely 4)Kb of memory available, and it is up to the function to check 
; that there is more if necessarily. this is always done with the first 
; instruction after arity check when necessary.

(define-module lib-memuse

   (export count-allocs)
;
;   (define (count-allocs insts)
;      ;; bug: count-allocs: not here yet
;      insts)
;
;   (define (count insts fail)
;      (let loop ((insts insts))
;         (tuple-case insts
;            ((ret a) 0)
;            ((move a b more) (loop more))
;            ((prim op args to more)
;               (cond
;                  ;; fixme: handle mk differently, this was supposed to be a temp hack
;                  ((> op #xff)
;                     (output-code op
;                        (+ 1 (+ (length args) (loop more)))))
;                  ((variable-input-arity? op)
;                     (cons op
;                        (cons (length args)
;                           (append (map reg args)
;                              (cons (reg to)
;                                 (assemble more fail))))))
;                  ((fixnum? to)
;                     (cons op
;                        (append (map reg args)
;                           (cons to
;                              (assemble more fail)))))
;                  ((list? to)
;                     (if (has? multiple-return-variable-primops op)    
;                        (cons op
;                           (append (map reg args)      
;                              ; <- nargs implicit, FIXME check nargs opcode too
;                              (append (map reg to)
;                                 (assemble more fail))))
;                        (cons op
;                           (append (map reg args)
;                              (cons (length to)          ; <- prefix with output arity
;                                 (append (map reg to)
;                                    (assemble more fail)))))))
;                  (else
;                     (error "bad case of primop in assemble: " op))))
;            ;; fixme: closures should have just one RTL node instead of separate ones for clos-proc and clos-code
;            ((clos-proc lpos offset env to more)
;               ;; make a 2-level closure
;               (if (= lpos 1)
;                  (cons (inst->op 'clos1)
;                     (cons (+ 2 (length env))      
;                        ;; size of object (hdr code e0 ... en) 
;                        (cons offset
;                           (append (map reg env)
;                              (cons (reg to)
;                                 (assemble more fail))))))
;                  (cons (inst->op 'clos)
;                     (cons (+ 2 (length env))      
;                        ;; size of object (hdr code e0 ... en) 
;                        (cons (reg lpos)
;                           (cons offset
;                              (append (map reg env)
;                                 (cons (reg to)
;                                    (assemble more fail)))))))))
;            ((clos-code lpos offset env to more)      ;; make a 1-level closure
;               (if (= lpos 1)
;                  (cons (inst->op 'cloc1)
;                     (cons (+ 2 (length env))   
;                        ;; size of object (hdr code e0 ... en) 
;                        (cons offset
;                           (append (map reg env)
;                              (cons (reg to)
;                                 (assemble more fail))))))
;                  (cons (inst->op 'cloc)
;                     (cons (+ 2 (length env))   
;                        ;; size of object (hdr code e0 ... en) 
;                        (cons (reg lpos)
;                           (cons offset
;                              (append (map reg env)
;                                 (cons (reg to)
;                                    (assemble more fail)))))))))
;            ((ld val to cont)
;               (cond
;                  ;; todo: add implicit load values to free bits of the instruction
;                  ((eq? val null)
;                     (ilist (inst->op 'ldn) (reg to)
;                        (assemble cont fail)))
;                  ((fixnum? val)
;                     (let ((code (assemble cont fail)))
;                        (if (or (> val 126) (< val -126)) ; would be a bug
;                           (error "ld: big value: " val))
;                        (ilist (inst->op 'ld) 
;                           (if (< val 0) (+ 256 val) val)
;                           (reg to) code)))
;                  ((eq? val #false)
;                     (ilist (inst->op 'ldf) (reg to)
;                        (assemble cont fail)))
;                  ((eq? val #true)
;                     (ilist (inst->op 'ldt) (reg to)
;                        (assemble cont fail)))
;                  (else
;                     (error "cannot assemble a load for " val))))
;            ((refi from offset to more)
;               (ilist 
;                  (inst->op 'refi) (reg from) offset (reg to) 
;                  (assemble more fail)))
;            ((goto op nargs)
;               (list (inst->op 'goto) (reg op) nargs))
;            ((goto-code op n)
;               (list (inst->op 'goto-code) (reg op)))
;            ((goto-proc op n)
;               (list (inst->op 'goto-proc) (reg op)))
;            ((goto-clos op n)
;               (list (inst->op 'goto-clos) (reg op)))
;            ;; todo: all jumps could have parameterized lengths (0 = 1-byte, n>0 = 2-byte, being the max code length)
;            ((jeq a b then else)
;               (lets
;                  ((then (assemble then fail))
;                   (else (assemble else fail))
;                   (len (length else)))
;                  (cond
;                     ((< len #xffff)
;                        (ilist (inst->op 'jlq) (reg a) (reg b)
;                           (band len #xff) (>> len 8) (append else then)))
;                     (else
;                        (error "need a bigger jump instruction: length is " len)))))
;            ;; todo: jit, jat and jrt should have a shared RTL node
;            ((jit a type then else)
;               (lets
;                  ((then (assemble then fail))
;                   (else (assemble else fail))
;                   (len (length else)))
;                  (cond
;                     ((< len #xffff)
;                        (ilist (inst->op 'jit2) (reg a) type (band len #xff) 
;                           (>> len 8) (append else then)))
;                     (else
;                        (error "need a bigger jit instruction: length is " len)))))
;            ((jat a type then else)
;               (lets
;                  ((then (assemble then fail))
;                   (else (assemble else fail))
;                   (len (length else)))
;                  (cond
;                     ((< len #xffff)
;                        (ilist (inst->op 'jat2) (reg a) type (band len #xff) 
;                           (>> len 8) (append else then)))
;                     ;; if (jat a type2 ...), make (jatq a type jmp jatq  ...)
;                     (else
;                        (error "need a bigger jat instruction: length is " len)))))
;            ((jrt a type then else)
;               (lets
;                  ((then (assemble then fail))
;                   (else (assemble else fail))
;                   (len (length else)))
;                  (cond
;                     ((< len #xff) ;; todo: jrt has only a short branch option
;                        (ilist (inst->op 'jrt) (reg a) type len (append else then)))
;                     (else
;                        (error "need a bigger jrt instruction: length is " len)))))
;            (else
;               (print "assemble: what is " code)
;               (fail #false)))))
;
;   ;; insts 
;   (define (count-allocs insts)
;      (call/cc
;         (λ (ret)
;            (let ((lim
;               (count insts 
;                  (λ (reason)
;                     (mail stderr (foldr render '(10) (cons "Failed to compute allocation limits: " reason)))
;                     insts))))
;               (if (< lim 4096) 
;                  insts
;                  (begin
;                     ;; fixme: add an alloc-instruction (primop + assembler + lib-cgen) and drop gc trigger to 4Kb
;                     (mail stderr (foldr render '(10) (list "Large allocator: " lim)))
;                     insts))))))
;

   ;; active: instead of this, export the primop table from somewhere and add memory use info there (as functions of corresponding rtl node?)
   ; this library could stay here, but it would become a simple code tree fold using the primop table function on each node

   (define (count-allocs insts) insts))

