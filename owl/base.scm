;; the standard toplevel library, being populated lazily

(define-library (owl base)
   (export
      ;; core
      begin if lets call/cc define let letrec

      ;; lists
      cons car cdr 
      caar
      cadr 
      cadr
      cddr
      fold map length

      ;; ffs
      put get ff-fold fupd

      ;; math
      + - * = < > <= >= /

      ;; io
      show print 

      ;; rendering
      render

      ;; essential syscalls
      error
      )
   (import
      (owl list)
      (owl list-extra)
      (owl ff)
      (owl defmac)
      (owl primop)
      (owl syscall)
      (owl io)
      (owl render)
      (owl math)))

