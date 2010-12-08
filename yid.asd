(defpackage yid-system
  (:use #:cl #:asdf))

(in-package #:yid-system)

(defsystem lazy
  :components ((:file "lazy")))

(defsystem yid
  :depends-on (lazy)
  :components ((:file "yid"))
  :in-order-to ((test-op (load-op yid-tests))))

(defmethod operation-done-p ((op test-op) (c (eql (find-system :yid))))
  (values nil))

(defsystem yid-tests
  :depends-on (yid lazy)
  :components ((:file "yid-tests")))
