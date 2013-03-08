(defpackage yacc-is-dead
  (:use #:cl #:lazy)
  (:export #:parser #:token #:eps #:con #:alt #:rep #:red
           #:*empty* #:*epsilon*
           #:parse #:parse-partial
           #:choice #:~ #:*+ #:==>
           #:recognizesp #:compact)
  (:export #:make-new-parser ;; DEFSYNTAX-related stuff
           #:*parser*
           #:parse-rule
           #:defsyntax))

(in-package #:yacc-is-dead)
