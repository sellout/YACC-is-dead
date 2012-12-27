(defpackage yacc-is-dead
  (:use #:cl #:lazy)
  (:export #:parser #:token #:eps #:con #:alt #:rep #:red
           #:*empty* #:*epsilon*
           #:parse #:parse-partial
           #:choice #:~ #:*+ #:==>
           #:recognizesp #:compact))
 
(in-package #:yacc-is-dead)
