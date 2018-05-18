(define-library (owl char)

   (export
      char?
      char=?
      char<?
      char>?
      char<=?
      char>=?
      char->integer
      integer->char)

   (import
      (owl defmac)
      (owl math))

   (begin

      (define char? fixnum?)

      (define char=? =)
      (define char<? <)
      (define char>? >)
      (define char<=? <=)
      (define char>=? >=)

      (define char->integer self)
      (define integer->char self)
))
