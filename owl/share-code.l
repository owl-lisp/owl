;;;
;;; Code sharing
;;;

; replace code fragments built for the term by equal, possibly native, 
; ones which are already available.

; note: this must be done *BEFORE* running the code, because search and 
; replace afterwards can break key order of red-black trees.

;  - for each incoming term
;     + walk the value down up to age of previous *toplevel* 
;     + compute a ff of obj -> obj' after replacing to preserve sharing 
;        * DP terminating to obj -> obj on older objects and immediates
;     + when hitting code vectors that are in the toplevel store
;        * just store obj -> replaced
;     + later also store all new encountered code vectors under some key having the code db
;     + later update the toplevel code db after each run

,r "owl/env.l"

(define-module lib-code-share

	(export 
      share-code            ;; compiler step, 
      ;no-codes              ;; value
      ;add-code              ;; (add-code <codes> <bytecode>) → codes' bytecode'
      vm-special-ops->codes ;; intended for (define *codes* (vm-special-ops->codes *vm-special-ops*))
      )

	(import lib-env module-ref module-set)

	(define (ok exp env) (tuple 'ok exp env))
	(define (fail reason) (tuple 'fail reason))

   
   ;; fixme: raw? and immediate? re-defined here
   (define (raw? obj) (eq? (fxband (type obj) #b100000000110) #b100000000110))
   (define (immediate? obj) (eq? 0 (size obj)))

   ; walk obj down to fields younger than given marker and 
   ; construct an equal object having all for which there are 
   ; equal values for in codes replaced by their values.


   ; collect objects for a search and replace. remember that the order must be preserved, 
   ; which is why they are first collected to an ordered collection as a ff.

   ; mark obj → ff of subobjs
   (define (objs-below-upto mark obj)
      ;(print "collecting closure")
      (define (collect seen ob)
         ;(print "    + next ob")
         ;(show "    + looking at " (list 'ob ob 'size (size ob)))
         (cond
            ((immediate? ob) seen)
            ((lesser? ob mark) 
               ;(print "     o below mark")
               seen)
            ((raw? ob)
               ;(print "     o raw")
               (put seen ob ob))
            ((get seen ob False) 
               ;(print "     o already seen")
               seen)
            (else
               ;(show " - getting contents, type is " (type ob))
               (let ((seen (put seen ob ob)))
                  (fold collect seen 
                     (map (λ (pos) (ref ob pos))
                        (iota (size ob) -1 0)))))))
      (collect F obj))

   (define (code-vector? obj)
      (and (raw? obj) (function? obj)))


   ;; when making a custom vm, there is a map of fixnum → bytecode at *vm-special-ops*
   ;; for restoring special instructions back to normal bytecode when dumping. here we 
   ;; need a map from bytecode → bytecode' which preserves equality and increases 
   ;; sharing

   ;; fixme: export primop wrapper generation from somewhere else
   (define (primop-wrapper bcode pop)
      (let ((arity (refb bcode 0))) ;; bytecode is #[arity ...]
         (cond
            ((<= pop #xffff)
               ;; fits in two instructions → #[arity 0 hi lo]
               (raw (list arity 0 (>> pop 8) (band pop 255)) 0 False))
            (else
               (error "primop-wrapper: too high: " pop)))))

   (define (code->list bcode)
      (map (λ (p) (refb bcode p))
         (iota 0 1 (sizeb bcode))))

   ;; use a list for now, switch to a tree later

   (define no-codes False)

   ;; codes = null | bytecode | (bytecode . shared) | #(bytecode value )

   ; bcode bcode → False | True | null (less | equal | greater)

   (define is-less False)
   (define is-equal True)
   (define is-greater null)

   (define (compare-bytes a b pos end)
      (if (eq? pos end) 
         is-equal
         (let ((ab (refb a pos)) (bb (refb b pos)))
            (cond
               ((eq? ab bb) (compare-bytes a b (+ pos 1) end))
               ((lesser? ab bb) is-less)
               (else is-greater)))))

   ;; shorter is less, otherwase lexical comparison from start
   (define (compare-code a b)
      (lets 
         ((as (sizeb a))
          (bs (sizeb b)))
         (cond
            ((eq? as bs) (compare-bytes a b 0 as))
            ((lesser? as bs) is-less)
            (else is-greater))))

   ;; fixme: should occasionally balance the tree

   ;; codes bcode value → codes'
   (define (insert-code codes bcode value)
      (if codes
         (ff-bind codes
            (λ (l k v r)
               (let ((res (compare-code k bcode)))
                  (cond
                     ((eq? res is-equal)
                        (mkblack l bcode value r))
                     ((eq? res is-less)
                        (mkblack (insert-code l bcode value) k v r))
                     (else
                        (mkblack l k v (insert-code r bcode value)))))))
         (mkblack False bcode value False)))
   
    ;; codes bcode → bcode(')
    (define (lookup-code codes bcode)
      (if codes
         (ff-bind codes
            (λ (l k v r)
               (let ((res (compare-code k bcode)))
                  (cond
                     ((eq? res is-equal) v)
                     ((eq? res is-less) (lookup-code l bcode))
                     (else (lookup-code r bcode))))))
         bcode))

   ;; fixme: O(n) code search!
   (define (vm-special-ops->codes pops)
      (ff-fold
         (λ (codes pop bcode)
            (insert-code codes bcode
               (primop-wrapper bcode pop)))
         no-codes pops))

   ;; codes = False | black node #[lesser bytecode shared greater]

   (define (replace-codes-upto mark codes obj)
      (lets 
         ((subs (objs-below-upto mark obj))
          (delta 
            (ff-fold
               (λ (delta obj _)
                  (cond
                     ((code-vector? obj)
                        ;(show " - code vector, checking out " (code->list obj))
                        (put delta obj (lookup-code codes obj)))
                     ((raw? obj) delta)
                     ((immediate? obj) delta)
                     ((symbol? obj) delta)
                     ((lesser? obj mark) delta)
                     (else
                        (lets 
                           ((fields (map (λ (p) (ref obj p)) (iota 1 1 (+ (size obj) 1))))
                            (news (map (λ (f) (get delta f f)) fields))
                            (both (zip cons fields news)))
                           (if (all (λ (x) (eq? (car x) (cdr x))) both)
                              delta ; nothing changed 
                              (put delta obj
                                 (listuple (>> (type obj) 3) (size obj) news)))))))
               False 
               subs)))
         (let ((new (get delta obj obj)))
            ;; notify if something has been changed (for the better)
            ;(if (not (eq? new obj))
            ;   (print " *** \\o\\ I'M ON A HORSE /o/ ***"))
            (values codes new))))

	(define (share-code exp env)
      (lets 
         ((toplevel (module-ref env '*toplevel* 42))
          (codes (module-ref env '*codes* no-codes)) ;; added when dumping repl
          (codes val (replace-codes-upto toplevel codes exp)))
         (ok val (module-set env '*codes* codes))))
               

)
