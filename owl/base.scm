(define-library (owl base)
   (export
      ;; core
      begin if lets call/cc define let letrec
      λ cond lets/cc and or not define* o
      tuple

      ;; lists
      cons car cdr
      caar
      cadr
      cdar
      cddr
      null
      fold map length iota
      list ilist
      for-each
      fold-map foldr-map

      ;; lazy lists
      pair lfold lmap delay uncons ltake liota lappend

      ;; ffs
      put get ff-fold fupd getf
      list->ff

      ;; math
      + - * = < > <= >= /
      << >> band bor bxor 

      ;; io
      show print print*
      stdout stdin stderr

      ;; strings
      string->list string-length str-fold 
      string=?

      ;; rendering
      render
     
      ;; random
      rand seed->rands

      ;; sorting
      sort

      ;; vector
      list->vector vector->list
      vector? byte-vector?

      ;; misc
      string->integer
      symbol->string
      error mail interact

      ;; comparison
      equal? eq? eqv?
      )

   (import
      (owl list)
      (owl list-extra)
      (owl ff)
      (owl io)
      (owl lazy)
      (owl string)
      (scheme misc)
      (owl symbol)
      (owl sort)
      (owl vector)
      (owl equal)
      (owl random)
      (owl defmac)
      (owl render)
      (owl syscall)
      (owl math)))
