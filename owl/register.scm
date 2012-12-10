;;;
;;; Register allocation
;;;

; Earlier compilation steps produce RTL that has an unbounded set 
; of registers. Register allocation handles limiting them to the 
; number available in VM, while also trying to retarget operations 
; to more sensible registers.

(define-library (owl register)

   (export 
      allocate-registers 
      n-registers)

   (import
      (owl defmac)
      (owl ff)
      (owl math)
      (owl list-extra)
      (owl primop)
      (owl io)
      (owl equal)
      (only (owl syscall) error)
      (owl list)
      (scheme misc))

   (begin
      ;; fixme: temp register limit
      (define highest-register 95) ;; atm lower than NR in ovm.c
      (define n-registers (+ highest-register 1)) 

      ; reg-touch U r -> mark as live -> make sure it has a value 
      ; (must be in some register)

      (define (reg-touch usages reg)
         (let ((val (get usages reg #false)))
            (if val
               usages
               (put usages reg null))))

      ; set value to (reg) -> would be cool to have it in reg

      (define (reg-root usages reg)
         (put usages reg (list reg)))

      ; return a list of registers from uses (where the value has been moved to), or 
      ; some list of low registers if this one is outside the available ones
      (define (use-list uses reg)
         (let ((opts (keep (λ (x) (< x highest-register)) (get uses reg null))))
            (cond
               ((< reg highest-register)
                  opts)
               ((null? opts)
                  (iota 4 1 highest-register))
               (else opts))))

      ; union on each register target
      (define (merge-usages a b)
         (ff-fold
            (λ (a reg places)
               (if places
                  (put a reg
                     (union (use-list a reg) places))
                  a))
            a b))

      ; FIXME, switch op and target to just (old . new)

      (define (bad? to target op)
         (or (eq? to target) (not (eq? to (op to)))))

      ; try to rename the register and exit via fail if the values are disturbed 

      (define (rtl-rename code op target fail)
         (tuple-case code
            ((ret a)
               (tuple 'ret (op a)))
            ((move a b more)
               (cond
                  ((eq? b target)
                     (if (eq? b (op a))
                        (rtl-rename more op target fail)
                        (fail)))
                  ((eq? a target)
                     (fail))
                  ((not (eq? b (op b)))
                     (fail))
                  (else
                     (let ((a (op a)))
                        (if (eq? a b)
                           (rtl-rename more op target fail)
                           (tuple 'move a b (rtl-rename more op target fail)))))))
            ((prim opcode args to more)
               (if (fixnum? to)
                  (if (bad? to target op) 
                     (fail)
                     (tuple 'prim opcode
                        (map op args)
                        to (rtl-rename more op target fail)))
                  (if (bad? to target op)
                     (fail)
                     (tuple 'prim opcode (map op args) to (rtl-rename more op target fail)))))
            ((clos-proc lp off env to more)
               (if (bad? to target op) 
                  (fail)
                  (tuple 'clos-proc (op lp) off (map op env) to (rtl-rename more op target fail))))
            ((clos-code lp off env to more)
               (if (bad? to target op)
                  (fail)
                  (tuple 'clos-code (op lp) off (map op env) to (rtl-rename more op target fail))))
            ((ld val to cont)
               (if (bad? to target op)
                  (fail)
                  (tuple 'ld val to (rtl-rename cont op target fail))))
            ((refi from offset to more)
               (if (bad? to target op)
                  (fail)
                  (tuple 'refi (op from) offset to (rtl-rename more op target fail))))
            ((goto fn nargs)
               (tuple 'goto (op fn) nargs))
            ((goto-code fn nargs)
               (tuple 'goto-code (op fn) nargs))
            ((goto-proc fn nargs)
               (tuple 'goto-proc (op fn) nargs))
            ((goto-clos fn nargs)
               (tuple 'goto-clos (op fn) nargs))
            ((jeq a b then else)
               (tuple 'jeq (op a) (op b) (rtl-rename then op target fail) (rtl-rename else op target fail)))
            ((jn a then else)
               (tuple 'jn (op a) (rtl-rename then op target fail) (rtl-rename else op target fail)))
            ((jz a then else)
               (tuple 'jz (op a) (rtl-rename then op target fail) (rtl-rename else op target fail)))
            ((jf a then else)
               (tuple 'jf (op a) (rtl-rename then op target fail) (rtl-rename else op target fail)))
            (else
               (error "rtl-rename: what is this: " code))))


      ;; try to remap the register to each known good alternative 
      ;; finish with (cont the-register code)

      (define (retarget-first code old news uses cont)
         (if (or (null? news) (and (has? news old) (< old highest-register)))
            (cont old code)         ; no remapping happened
            (let ((new (car news)))
               (if (or (eq? old new) (get uses new #false))
                  (retarget-first code old (cdr news) uses cont)
                  (let ((new-code 
                     (call/cc
                        (λ (drop)
                           (rtl-rename code 
                              (λ (reg) (if (eq? reg old) new reg))
                              new
                              (lambda () (drop #false)))))))
                     (if new-code
                        (cont new new-code) ; remapping success
                        (retarget-first code old (cdr news) uses cont)))))))

      (define (rtl-retard-jump proc op a b then else)
         (lets 
            ((then then-uses (proc then))
             (else else-uses (proc else))
             (uses (merge-usages then-uses else-uses))
             (uses (reg-touch uses a)))
            (case op
               ((jeq)
                  (values (tuple op a b then else) (reg-touch uses b)))
               (else
                  (values (tuple op a then else) uses)))))

      (define (rtl-retard-closure rtl-retard code)
         (lets
            ((clos-type lpos offset env to more code)
             (more uses (rtl-retard more))
             (good (use-list uses to))
             (uses (del uses to))
             (pass (λ () (values (tuple clos-type lpos offset env to more) (fold reg-touch uses (cons lpos env))))))
            (retarget-first more to good uses
               (λ (to-new more-new)
                  (if (eq? to to-new)
                     (pass)
                     (rtl-retard (tuple clos-type lpos offset env to-new more-new)))))))

      ; retarget register saves to registers where they are moved 
      ; where possible (register retargeting level 1)

      (define (rtl-retard code)
         (tuple-case code
            ((ret a)
               ;; needs R3=cont and Ra
               (if (> a highest-register)
                  ;; needs to be relocated lower, so return here a wish to put is somewhere lower
                  (values code
                     (put (reg-root empty 3) a (iota 4 1 highest-register))) ; please move me anywhere lower
                  (values code
                     (reg-touch (reg-root empty 3) a))))

            ((move a b more)
               (cond
                  ((eq? a b)
                     (rtl-retard more)) ; drop useless instruction
                  ((> b highest-register)
                     (error "out of registers in move: " b))
                  (else
                     (lets 
                        ((more uses (rtl-retard more))
                         (uses (del uses b))
                         (targets (use-list uses a)))
                        (if (has? targets b)
                           ; moved to a useful target
                           (values (tuple 'move a b more) uses)
                           ; leave a wish that the value at a could already be in b
                           (values (tuple 'move a b more) (put uses a (cons b targets))))))))

            ((prim op args to more)
               (lets 
                  ((more uses (rtl-retard more))
                   (pass 
                     (λ () (values
                        (tuple 'prim op args to more)
                        (fold reg-touch (del uses to) args)))))
                  (cond
                     ((fixnum? to)
                        ; retarget the sole argument if possible
                        (let ((good (use-list uses to)))
                           (retarget-first more to good uses
                              (lambda (to-new more-new)
                                 (if (eq? to to-new)
                                    (pass)
                                    (rtl-retard
                                       (tuple 'prim op args to-new more-new)))))))
                     ;; fixme: no register retargeting for multiple-return-value primops
                     (else
                        '(call/cc
                           (lambda (ret)
                              (fold
                                 (lambda (pass ato)
                                    (let ((good (use-list uses ato)))
                                       (retarget-first more ato good uses
                                          (lambda (ato-new more-new)
                                             (if (eq? ato ato-new)
                                                pass
                                                (ret
                                                   (rtl-retard
                                                      (tuple 'prim op args
                                                         (map (lambda (to) (if (eq? to ato) ato-new to)) to)
                                                         more-new))))))))
                                 pass to)))
                        (pass)))))

            ((ld val to cont)
               (lets 
                  ((cont uses (rtl-retard cont))
                   (good (use-list uses to))
                   (good (if (> to highest-register) (append good (iota 0 1 highest-register)) good))
                   (pass (λ () (values (tuple 'ld val to cont) (del uses to)))))
                  (retarget-first cont to good uses
                     (λ (to-new cont-new)
                        (if (eq? to to-new)
                           (pass)
                           (rtl-retard (tuple 'ld val to-new cont-new)))))))

            ((clos-proc lpos offset env to more)  
               (rtl-retard-closure rtl-retard code))

            ((clos-code lpos offset env to more) 
               (rtl-retard-closure rtl-retard code))

            ((refi from offset to more)
               (lets 
                  ((more uses (rtl-retard more))
                   (uses (reg-touch uses from))
                   (good (use-list uses to))
                   (uses (del uses to)) 
                   (pass 
                     (λ () (values (tuple 'refi from offset to more)
                        (reg-touch uses from)))))
                  (retarget-first more to good uses
                     (λ (to-new more-new)
                        (if (eq? to to-new)
                           (pass)
                           (rtl-retard
                              (tuple 'refi from offset to-new more-new)))))))
            ((goto op nargs)
               (values code (fold reg-root empty (cons op (iota 3 1 (+ 4 nargs))))))
            ((goto-code op nargs)
               (values code (fold reg-root empty (cons op (iota 3 1 (+ 4 nargs))))))
            ((goto-proc op nargs)
               (values code (fold reg-root empty (cons op (iota 3 1 (+ 4 nargs))))))
            ((goto-clos op nargs)
               (values code (fold reg-root empty (cons op (iota 3 1 (+ 4 nargs))))))
            ((jeq a b then else)
               (rtl-retard-jump rtl-retard 'jeq a b     then else))
            ((jn a then else)
               (rtl-retard-jump rtl-retard 'jn a empty  then else)) ; fp
            ((jf a then else)
               (rtl-retard-jump rtl-retard 'jf a empty  then else)) ; fp
            ((jz a then else)
               (rtl-retard-jump rtl-retard 'jz a empty  then else)) ; fp
            ((jab a type then else)
               (rtl-retard-jump rtl-retard 'jab a type then else))
            (else
               (error "rtl-retard: unknown code: " code))))

      (define (allocate-registers rtl)
         (lets ((rtl usages (rtl-retard rtl)))
            rtl))
         
   ))
