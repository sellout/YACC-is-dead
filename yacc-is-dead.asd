(defpackage yacc-is-dead-system
  (:use #:cl #:asdf))

(in-package #:yacc-is-dead-system)

(defsystem lazy
  :components ((:file "lazy")))

(defsystem yacc-is-dead
  :depends-on (lazy alexandria)
  :serial t
  :components ((:file "package")
               (:file "yacc-is-dead")
               (:file "defsyntax"))
  :in-order-to ((test-op (load-op yacc-is-dead-tests)))
  :perform (test-op :after (op c)
                    (funcall (intern "RUN!" :yacc-is-dead-tests)
                             (intern "YACC-IS-DEAD-TESTS" :yacc-is-dead-tests))))

(defmethod operation-done-p ((op test-op) (c (eql (find-system :yacc-is-dead))))
  (values nil))

(defsystem yacc-is-dead-tests
  :depends-on (yacc-is-dead lazy fiveam)
  :components ((:file "tests-package")
               (:file "api-tests" :depends-on ("tests-package"))
               (:file "scala-tests" :depends-on ("tests-package"))
               (:file "racket-tests" :depends-on ("tests-package"))))
