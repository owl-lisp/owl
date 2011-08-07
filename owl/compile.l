;;;
;;; Compile AST to a code instruction tree suitable for assembly
;;;

;; todo: register dance prior to making a call really belongs to a separate register retargeting pass

,r "owl/ast.l"
,r "owl/assemble.l"
,r "owl/closurize.l"

(define-module lib-compile

   (export compile
      nary-primop nullary-primop unary-primop binary-primop trinary-primop
      primops prim-opcodes
      )

   (import lib-ast)
   (import lib-lazy)
   (import lib-assemble)
   (import lib-closurize uncompiled-closure?)

	(define try-n-perms 1000)	 ;; how many load permutations to try before evicting more registers

   (define (small-value? val)
      (or
         (and (fixnum? val) (>= val -127) (< val 127))
         (eq? val True)
         (eq? val False)
         (eq? val null)))

   (define (ok exp env) (tuple 'ok exp env))
   (define (fail reason) (tuple 'fail reason))

   ; regs = (node ...), biggest id first
   ; node = #(var sym id)
   ;      = #(val value id)
   ;      = #(env regs id) 
   ;      = #(lit regs id)

   ; [r0 = MCP] [r1 = Clos] [r2 = Env] [r3 = a0, often cont] [r4] ... [rn]

   (define a0 3) ;;; number of first argument register (may change) 

   (define (next-free-register regs)
      (if (null? regs)
         a0
         (+ (ref (car regs) 3) 1)))

   (define (load-small-value regs val cont)
      (let ((reg (next-free-register regs)))
         (tuple 'ld val reg
            (cont
               (cons (tuple 'val val reg) regs)
               reg))))

   ; get index of thing at (future) tuple 
   ; lst = (l0 l1 ... ln) -> #(header <code/proc> l0 ... ln)
   (define (index-of thing lst pos)
      (cond
         ((null? lst) False)
         ((eq? (car lst) thing) pos)
         (else (index-of thing (cdr lst) (+ pos 1)))))

   (define (find-any regs sym type subtype)
      (if (null? regs)
         False
         (let ((this (car regs)))
            (cond
               ((and (eq? type (ref this 1))
                  (eq? (ref this 2) sym))
                  (ref this 3))
               ((eq? subtype (ref this 1))
                  (or
                     (find-any (cdr regs) sym type subtype)
                     (let 
                        ((sub 
                           (index-of sym (ref this 2) 2)))   
                        ;; FIXME, 2 will not be correct for shared envs
                        (if sub
                           (cons (ref this 3) sub)
                           False))))
               (else
                  (find-any (cdr regs) sym type subtype))))))

   (define (find-literals env)
      (if (null? env)
         (error "No literals found: " env)
         (tuple-case (car env)
            ((lit vals id)
               id)
            (else
               (find-literals (cdr env))))))

   ;; find a register or an env address containing the thing
   (define (find-variable regs var)
      (find-any regs var 'var 'env))

   ;; find a register or address to literals where it can be found
   (define (find-value regs var)
      (find-any regs var 'val 'lit))

   ; #(iref a1 a2 r more)
   ; #(ref a r more)

   (define (rtl-value regs val cont)
      (let ((position (find-value regs val)))
         (cond
            ((fixnum? position)
               (cont regs position))
            ((small-value? val)
               (load-small-value regs val
                  (lambda (regs pos)
                     (cont regs pos))))
            ((not position)
               (error "rtl-value: cannot make a load for a " val))
            ((fixnum? (cdr position))
               (let ((this (next-free-register regs)))
                  (tuple 'refi (car position) (cdr position) this
                     (cont (cons (tuple 'val val this) regs) this))))
            (else
               (error "tried to use old chain load in " val)))))

   (define (rtl-variable regs sym cont)
      (let ((position (find-variable regs sym)))
         (cond
            ((fixnum? position)
               (cont regs position))
            ((not position)
               (error "rtl-variable: cannot find the variable " sym))
            ((fixnum? (cdr position))
               (let ((this (next-free-register regs)))
                  (tuple 'refi (car position) (cdr position) this
                     (cont (cons (tuple 'var sym this) regs) this))))
            (else
               (error "no chain load: " position)))))


   (define (rtl-close regs lit-offset env lit cont)
      (let ((this (next-free-register regs)))
         (cond
            ((null? env)
               ;; no need to close, just refer the executable procedure
               (tuple 'refi (find-literals regs) lit-offset this
                  (cont
                     (cons (tuple 'val (list 'a-closure) this) regs)
                     this)))
            ((null? lit)
               ;; the function will be of the form 
               ;; #(closure-header <code> e0 ... en)
               (tuple 'clos-code (find-literals regs) lit-offset env this
                  (cont
                     (cons (tuple 'val (list 'a-closure) this) regs)
                     this)))
            (else
               ;; the function will be of the form 
               ;; #(clos-header #(proc-header <code> l0 .. ln) e0 .. em)
               (tuple 'clos-proc (find-literals regs) lit-offset env this
                  (cont
                     (cons (tuple 'val (list 'a-closure) this) regs)
                     this))))))

   (define (env->loadable env)
      (map
         (lambda (x)
            (if (symbol? x)
               (tuple 'var x)
               (error "Cannot yet load this env node: " env)))
         env))

   (define (create-alias regs name position)
      (let ((this (car regs)))
         (if (eq? (ref this 3) position)
            (cons (tuple 'var name position) regs)
            (cons this
               (create-alias (cdr regs) name position)))))

   (define (create-aliases regs names positions)
      (fold
         (lambda (regs alias)
            (create-alias regs (car alias) (cdr alias)))
         regs
         (zip cons names positions)))

   (define (rtl-arguments one?)

      (define (one regs a cont)
         (tuple-case a
            ((value val)
               (rtl-value regs val cont))
            ((var sym)
               (rtl-variable regs sym cont))
            ((make-closure lpos env lit)
               (many regs (env->loadable env) null
                  (lambda (regs envp)
                     (rtl-close regs lpos envp lit cont))))
            (else
               (error "rtl-simple: unknown thing: " a))))

      (define (many regs args places cont)
         (if (null? args)
            (cont regs (reverse places))
            (one regs (car args)
               (lambda (regs pos)
                  (many regs (cdr args) (cons pos places) cont)))))
      (if one?
         one
         (lambda (regs args cont)
            (many regs args null cont))))


   (define rtl-simple (rtl-arguments True))

   (define rtl-args (rtl-arguments False))

   ; -> [reg] + regs'
   (define (rtl-bind regs formals)
      (let loop ((regs regs) (formals formals) (taken null))
         (if (null? formals)
            (tuple (reverse taken) regs)
            (let ((this (next-free-register regs)))
               (loop
                  (cons (tuple 'var (car formals) this) regs)
                  (cdr formals)
                  (cons this taken))))))

;; fixme: mkt chugs the type to the instruction
   (define (rtl-primitive regs op formals args cont)
      (if (eq? op 23) ; generalize this later. mkt is not a safe instruction!
         (if (null? args)
            (error "rtl-primitive: no type for mkt" args)
            (begin
               (rtl-primitive regs 
                  (+ (<< op 8) (band (value-of (car args)) #xff))
                  formals (cdr args) cont)))
         (rtl-args regs args
            (lambda (regs args)
               ;; args = input registers
               (cond
                  ;; a run-of-the-mill a0 .. an → rval -primop
                  ((and (= (length formals) 1) (not (special-bind-primop? op)))
                     (let ((this (next-free-register regs)))
                        (tuple 'prim op args this
                           (cont
                              (cons
                                 (tuple 'var (car formals) this)
                                 regs)))))
                  (else
                     ; bind or ff-bind, or arithmetic
                     (bind (rtl-bind regs formals)
                        (lambda (selected regs)
                           (tuple 'prim op args selected
                              (cont regs))))))))))


   (define (rtl-make-moves sequence tail)
      (foldr
         (lambda (move rest)
            (if (eq? (car move) (cdr move))
               rest
               (tuple 'move (car move) (cdr move) rest)))
         tail sequence))

   (define (rtl-moves-ok? moves)
      (cond
         ((null? moves) True)
         ((getq (cdr moves) (cdar moves))
            False)
         (else
            (rtl-moves-ok? (cdr moves)))))

   ;;; (from ...) -> ((from . to) ...)
   (define (rtl-add-targets args)
      (zip cons args
         (iota a0 1 (+ (length args) a0))))

   (define (rtl-safe-registers n call)
      (let loop
         ((hp (+ (length call) (+ a0 1)))
          (safe null)
          (n n))
         (cond
            ((= n 0)
               (reverse safe))
            ((has? call hp)
               (loop (+ hp 1) safe n))
            (else
               (loop (+ hp 1) (cons hp safe) (- n 1))))))

   ;;; -> replace the to-save registers in call 
   (define (apply-saves to-save safes call)
      (let ((new (zip cons to-save safes)))
         (map
            (lambda (reg)
               (let ((node (getq new reg)))
                  (if node (cdr node) reg)))
            call)))


   (define (rtl-check-moves perms n)
      (call/cc
         (lambda (ret)
            (lfor 0 perms
               (lambda (nth perm)
                  (cond
                     ((rtl-moves-ok? perm) (ret perm))
                     ((eq? nth try-n-perms) (ret False))
                     (else (+ nth 1)))))
               False)))

   ;;; find the first set of saves that works
   (define (rtl-try-saves saves free call rest)
      (lets
         ((call-prime (apply-saves saves free call))
          (call-prime (rtl-add-targets call-prime))
          (call-prime 
            (remove 
               (lambda (move) (eq? (car move) (cdr move)))
               call-prime))
          (call-prime (sort (lambda (a b) (< (car a) (car b))) call-prime))
          (perms (permutations call-prime))
          (ok-moves (rtl-check-moves perms 1)))
         (if ok-moves
            (rtl-make-moves
               (append (zip cons saves free) ok-moves)
               rest)
            False)))

   (define (rtl-make-jump call free rest)
      (call/cc
         (lambda (ret)
            (or
               (lfor False (subsets call)
                  (lambda (foo subset) 
                     (cond
                        ((rtl-try-saves subset free call rest)
                           => (lambda (call) (ret call)))
                        (else False))))
               ; has never happened in practice
               (error "failed to compile call: " call)))))

   (define (rtl-jump rator rands free inst)
      (let ((nargs (length rands)))
         (cond
            ;; cont is usually at 2, and usually there is 
            ;; 1 return value -> special instruction
            ((and (eq? rator a0) (= nargs 1))
               (tuple 'ret (car rands)))
            ;;; rator is itself in rands, and does not need rescuing
            ((has? rands rator)
               (rtl-make-jump rands free
                  (if inst
                     (tuple inst (index-of rator rands a0) nargs)
                     (tuple 'goto
                        (index-of rator rands a0)
                        nargs))))
            ;;; rator is above rands, again no need to rescue
            ((> rator (+ 2 nargs))
               (rtl-make-jump rands free
                  (if inst
                     (tuple inst rator nargs)
                     (tuple 'goto rator (length rands)))))
            (else
               (tuple 'move rator (car free)
                  (rtl-jump (car free) rands (cdr free) inst))))))

   (define (fn-type obj)
      (let ((t (type obj)))
         (cond
            ((eq? 2054 (fxband #b100011111111 t)) ;; raw bytecode
               (tuple 'code (refb obj 0)))
            ((eq? t 262)
               (tuple 'proc (refb (ref obj 1) 0)))
            ((eq? t 518)
               (tuple 'clos (refb (ref (ref obj 1) 1) 0)))
            (else
               (tuple 'bad-fn 0)))))

   (define bad-arity "Bad arity: ")

   (define (rtl-pick-call rator nargs)
      (tuple-case rator
         ((value rator)
            (tuple-case (fn-type rator)
               ((code n)
                  ;; todo: could check further whether the code consists one just one macroinstruction, 
                  ;; and if so, make a direct (call-instruction-16 <hi> <lo>)
                  (if (= n nargs) 'goto-code 
                     (error bad-arity (list rator 'wanted (- n 1) 'got (- nargs 1)))))
               ((proc n)
                  (if (= n nargs) 'goto-proc 
                     (error bad-arity 
                        (list rator 'wanted (- n 1) 'got (- nargs 1)))))
               ((clos n)
                  (if (= n nargs) 'goto-clos 
                     (error bad-arity 
                        (list rator 'wanted (- n 1) 'got (- nargs 1)))))
               (else is foo
                  (error "Bad operator: " rator))))
         (else False)))

   (define (rtl-call regs rator rands)
      ; rator is here possibly #(value #<func>) and much of the call can be inlined
      ; change the flag if can check call here
      (rtl-args regs (cons rator rands)
         (lambda (regs all)
            (let ((free (rtl-safe-registers (length all) all)))
               (rtl-jump (car all) (cdr all) free 
                  (rtl-pick-call rator (length rands)))))))

   (define (value-pred pred)
      (lambda (val)
         (tuple-case val
            ((value val)
               (pred val))
            (else False))))

   (define null-value? (value-pred null?))
   (define false-value? (value-pred (lambda (x) (eq? x False))))
   (define zero-value? (value-pred (lambda (x) (eq? x 0))))

   (define (simple-first a b cont)
      (cond
         ((null-value? b)  (cont b a))
         ((false-value? b) (cont b a))
         ((zero-value? b)  (cont b a))
         (else
            (cont a b))))

   (define (extract-value node)
      (tuple-case node
         ((value val) val)
         (else False)))

	;; fixme: ??? O(n) search for opcode->primop. what the...
   (define (opcode->primop op)
      (let 
         ((node
            (some 
               (lambda (x) (if (eq? (ref x 2) op) x False))
               primops)))
         (if node node (error "Unknown primop: " op))))

   (define (opcode-arity-ok? op n)
      (bind (opcode->primop op)
         (lambda (name op in out fn)
            (cond
               ((eq? in n) True)
               ((eq? in 'any) True)
               (else False)))))

   (define (rtl-control regs exp)
      ;(show " - " exp)
      (tuple-case exp
         ((branch kind a b then else)
            (cond
               ((eq? kind 0)      ; branch on equality (jump if equal)
                  (simple-first a b
                     ;;; move simple to a, if any
                     (lambda (a b)
                        (cond
;; todo: convert jump-if-<val> rtl nodes to a single shared rtl node to avoid having to deal with them as separate instructions
      
                           ((null-value? a) ; jump-if-null (optimization)
                              (rtl-simple regs b (lambda (regs bp)
                                 (let 
                                    ((then (rtl-control regs then))
                                     (else (rtl-control regs else)))
                                    (tuple 'jn bp then else)))))
                           ((false-value? a) ; jump-if-false 
                              (rtl-simple regs b (lambda (regs bp)
                                 (let 
                                    ((then (rtl-control regs then))
                                     (else (rtl-control regs else)))
                                    (tuple 'jf bp then else)))))
                           ;; jz not yet understood everywhere
                           ;((zero-value? a) ; jump-if-zero 
                           ;   (rtl-simple regs b (lambda (regs bp)
                           ;      (let 
                           ;         ((then (rtl-control regs then))
                           ;          (else (rtl-control regs else)))
                           ;         (tuple 'jz bp then else)))))
                           (else
                              (rtl-simple regs a (lambda (regs ap)
                                 (rtl-simple regs b (lambda (regs bp)
                                    (let
                                       ((then (rtl-control regs then))
                                        (else (rtl-control regs else)))
                                       (tuple 'jeq ap bp then else)))))))))))
               ((eq? kind 1)      ; branch on type of immediate object
                  (let ((b (extract-value b)))
                     (if (and (fixnum? b) (>= b 0) (< b 256))
                        (rtl-simple regs a 
                           (lambda (regs ap)
                              (let 
                                 ((then (rtl-control regs then))
                                  (else (rtl-control regs else)))
                                 (tuple 'jit ap b then else))))
                        (error "rtl-control: bad immediate branch type: " b))))
               ((eq? kind 2)      ; branch on type of allocated object
                  (let ((b (extract-value b)))
                     (if (and (fixnum? b) (>= b 0) (< b 256))
                        (rtl-simple regs a 
                           (lambda (regs ap)
                              (let 
                                 ((then (rtl-control regs then))
                                  (else (rtl-control regs else)))
                                 ; peephole optimize (jat <x> <type-a> <then> (jat <x> <type-b> .. ..))
                                 (tuple 'jat ap b then else))))
                        (error "rtl-control: bad alloc branch type: " b))))
               ((eq? kind 3) ; branch on type of raw object
                  (let ((b (extract-value b)))
                     (if (and (fixnum? b) (>= b 0) (< b 256))
                        (rtl-simple regs a 
                           (lambda (regs ap)
                              (let 
                                 ((then (rtl-control regs then))
                                  (else (rtl-control regs else)))
                                 (tuple 'jrt ap b then else))))
                        (error "rtl-control: bad raw branch type: " b))))
               ((eq? kind 4)   ; (branch-4 name type (lambda (f0 .. fn) B) Else)
                  ; FIXME check object size here (via meta)
                  (let ((b (extract-value b)))
                     (if (and (fixnum? b) (>= b 0) (< b 257))
                        (rtl-simple regs a 
                           (lambda (regs ap)
                              (tuple-case then
                                 ((lambda formals body)
                                    (bind (rtl-bind regs formals)
                                       (lambda (selected then-regs)
                                          (let 
                                             ((then-body (rtl-control then-regs body))
                                              (else (rtl-control regs else)))
                                             (tuple 'jab ap b 
                                                (tuple 'lambda selected then-body)
                                                else)))))
                                 (else
                                    (error "rtl-control: bad jab then branch: " then)))))
                        (error "rtl-control: bad alloc binding branch type: " b))))
               ((eq? kind 3)
                  ; a verbose type-directed binding dispatch instruction, probable base for a real 
                  ; algebraic data types. (branch 3 obj type then else)
                  (tuple-case (extract-value b)
                     ((immediate type)
                        ; same as kind 1 above
                        (if (and (fixnum? type) (>= type 0) (< type 257))
                           (rtl-simple regs a 
                              (lambda (regs ap)
                                 (let 
                                    ((then (rtl-control regs then))
                                     (else (rtl-control regs else)))
                                    (tuple 'jit ap type then else))))
                           (error "rtl-control: bad immediate branch type: " type)))
                     ((alloc type size)
                        ; same as kind 2 above
                        (if (and (fixnum? type) (>= type 0) (< type 257))
                           (rtl-simple regs a 
                              (lambda (regs ap)
                                 (let 
                                    ((then (rtl-control regs then))
                                     (else (rtl-control regs else)))
                                    (tuple 'jat ap type then else))))
                           (error "rtl-control: bad immediate branch type: " b)))
                     ((literal val)
                        ; same as (branch 0 val a then else)
                        (rtl-control regs (tuple 'branch 0 a (mkval val) then else)))
                     (else is type
                        (error "rtl-control: type branch: bad type: " type))))
               (else
                  (show "rtl-control: unknown branch type: " kind))))
         ((call rator rands)
            (let ((op (and (eq? (ref rator 1) 'value) (primop-of (ref rator 2)))))
               (if op
                  (tuple-case (car rands)
                     ((lambda formals body)
                        (if (opcode-arity-ok? op (length (cdr rands)))
                           (rtl-primitive regs op formals (cdr rands)
                              (lambda (regs) (rtl-control regs body)))
                           (error "Bad number of arguments for primitive: " 
                              (list 'op op 'got (length (cdr rands)) 'at rands))))
                     (else
                        (error "bad primitive args: " rands)))
                  (tuple-case rator
                     ((lambda formals body)
                        (rtl-args regs rands
                           (lambda (regs args)
                              ;;; note that this is an alias thing...
                              (if (= (length formals) (length args))
                                 (rtl-control
                                    (create-aliases regs formals args)
                                    body)
                                 (error "Bad argument count in lambda call: " (list 'args args 'formals formals))))))
                     (else
                        (rtl-call regs rator rands))))))
         (else
            (show "rtl-control: wtf: " exp))))

   (define (formals->regs formals pos)
      (if (null? formals)
         null
         (cons (tuple 'var (car formals) pos)
            (formals->regs (cdr formals) (+ pos 1)))))

   ; r0 = clos, r1 = lit, r2 = a0 ...

   (define (entry-regs clos literals formals)
      (append
         (reverse (formals->regs formals a0))
         (if (null? clos)
            (list
               (tuple 'env null 2)        ; <- really just empty
               (tuple 'lit literals 1))   ; <- may be empty
            (list
               (tuple 'lit literals 2)    ; <- may be empty
               (tuple 'env clos 1)))))

   ;;; closure -> executable procedure (closed from elsewhere if not independent)

   (define (rtl-literal env rtl thing)
      (if (uncompiled-closure? thing)
         (rtl env (cdr thing))
         (values env thing)))

   ; code .. → code' ...
   (define (rtl-literals env rtl-procedure lits)
      ;;; convert all uncompiled closures to procedures
      (if (null? lits)
         (values env lits)
         (lets
            ((env this (rtl-literal  env rtl-procedure (car lits)))
             (env tail (rtl-literals env rtl-procedure (cdr lits))))
            (values env (cons this tail)))))

   (define (list->proc lst)
      (listuple 32 (length lst) lst))

   ;;; proc = #(procedure-header <code-ptr> l0 ... ln)
   ; env node → env' owl-func
   (define (rtl-procedure env node)
      (tuple-case node
         ;;; #(procedure #(code arity exec) l0 ... ln)
         ((closure formals body clos literals)
            (lets
               ((env lits (rtl-literals env rtl-procedure literals))
                (env exec
                  (assemble-code env
                     (tuple 'code
                        (length formals)
                        (rtl-control (entry-regs clos literals formals) body)))))
               (values env
                  (if (null? lits)
                     exec ; #<bytecode> 
                     (list->proc (cons exec lits)))))) ; #[TPROC #<bytecode> <val0> ... <valn]
         (else
            (error "rtl-procedure: bad input: " node))))


   ; env exp → env' exp'
   (define (rtl-exp env exp)
      (tuple-case exp
         ((closure formals body clos literals)
            (if (null? clos)
               (rtl-procedure env exp)
               (error "rtl-exp: free variables in entry closure: " clos)))
         (else
            (values env False))))

   (define (compile exp env)
      ;(show " - compiling " exp)
      ;; todo: add and update the shared code tree and possible other necessary things in env (or pass and modify the real env?)
      (lets
         ((toplevel (module-ref env '*toplevel* 42))
          (codes (module-ref env '*codes* False))
          (codes exp (rtl-exp codes exp)))
         (ok exp (module-set env '*codes* codes))))

)
