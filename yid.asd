(defpackage yid-system
  (:use #:cl #:asdf))

(in-package #:yid-system)

(defsystem lazy
  :components ((:file "lazy")))

(defsystem yid
  :depends-on (lazy alexandria)
  :components ((:file "yid"))
  :in-order-to ((test-op (load-op yid-tests)))
  :perform (test-op :after (op c)
                    (funcall (intern "RUN!" :yid-tests)
                             (intern "YID-TESTS" :yid-tests))))

(defmethod operation-done-p ((op test-op) (c (eql (find-system :yid))))
  (values nil))

(defsystem yid-tests
  :depends-on (yid lazy fiveam)
  :components ((:file "tests-package")
               (:file "api-tests" :depends-on ("tests-package"))
               (:file "scala-tests" :depends-on ("tests-package"))
               (:file "racket-tests" :depends-on ("tests-package"))))
