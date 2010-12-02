(defpackage yid
  (:use #:cl)
  (:export #:parser #:parse
           #:eq-t
           #:red
           #:choice #:concatenation #:replication))

(in-package #:yid)

;;; Some helper functions to make the translation from Scala easier, until I
;;; think about how to do things more Lispily

(defun is-empty-p (stream)
  (not (peek-char 'character stream nil)))

(defun stream-cons (item stream)
  (make-concatenated-stream (make-string-input-stream item)
                            stream))

(defun make-empty-stream ()
  (make-string-input-stream ""))

;;; Ok, here's the real stuff

(defclass change-cell ()
  ((changedp :initform nil :accessor changedp)
   (seen :initform () :accessor seen)))

(defmethod or-with ((object change-cell) changed)
  (or (slot-value object 'changed)
      (setf (slot-value object 'changed) changed)))

(defclass concatenation ()
  ((x :initarg :x :reader x)
   (y :initarg :y :reader y)))

(defclass parser ()
  ((parse-null :initform '())
   (is-empty :initform nil)
   (is-nullable :initform nil)
   (initializedp :initform nil :accessor initializedp)
   (cache :initform (make-hash-table :test #'equal) :reader cache)))

(defclass eq-t (parser)
  ((value :initarg :value)
   (parse-null :initform ())
   (is-empty :initform nil)
   (is-nullable :initform nil)))

(defclass emp (parser)
  ((parse-null :initform ())
   (is-empty :initform t)
   (is-nullable :initform nil)))

(defclass eps (parser)
  ((generator :initarg :generator :reader generator)
   (is-empty :initform nil)
   (is-nullable :initform t)))

(defclass epsilon (eps)
  ()
  (:default-initargs :generator (stream-cons "" (make-empty-stream))))

(defclass con (parser)
  ((first :initarg :first)
   (second :initarg :second)))

(defclass alt (parser)
  ((choice1 :initarg :choice1 :reader choice1)
   (choice2 :initarg :choice2 :reader choice2)))

(defclass rep (parser)
  ((parser :initarg :parser)
   (parse-null :initform '(nil))
   (is-empty :initform nil)
   (is-nullable :initform t)))

(defclass red (parser)
  ((parser :initarg :parser)
   (f :initarg :f)))

(defun parse-null (parser)
  (if (is-empty parser)
      '()
      (slot-value parser 'parse-null)))

(defun is-nullable (parser)
  (if (is-empty parser)
      nil
      (slot-value parser 'is-nullable)))

(defun is-empty (parser)
  (initialize-parser parser)
  (slot-value parser 'is-empty))

;;; NOTE: These SETF functions behave differently from most. Rather than
;;;       returning the value they were passed, they return whether or not the
;;;       value changed.

(defun (setf parse-null) (value parser)
  (when (not (equal (slot-value parser 'parse-null) value))
    (setf (slot-value parser 'parse-null) value)
    t))

(defun (setf is-empty) (value parser)
  (when (not (eq (slot-value parser 'is-empty) value))
    (setf (slot-value parser 'is-empty) value)
    t))

(defun (setf is-nullable) (value parser)
  (when (not (eq (slot-value parser 'is-nullable) value))
    (setf (slot-value parser 'is-nullable) value)
    t))

(defun initialize-parser (parser)
  (when (not (initializedp parser))
    (setf (initializedp parser) t)
    (loop
       with change = (make-instance 'change-cell)
       do (update-child-based-attributes parser change)
       while (changedp change))))

(defgeneric derive (parser value)
  (:method :around ((parser parser) value)
    (cond ((is-empty parser) (make-instance 'emp))
          ((gethash value (cache parser)) (gethash value (cache parser)))
          (t (setf (gethash value (cache parser)) (call-next-method)))))
  (:method ((parser eq-t) value)
    (if (equal (slot-value parser 'value) value)
        (make-instance 'eps :generator (stream-cons value (make-empty-stream)))
        (make-instance 'emp)))
  (:method ((parser emp) value)
    (declare (ignore value))
    parser)
  (:method ((parser eps) value)
    (declare (ignore value))
    (make-instance 'emp))
  (:method ((parser con) value)
    (if (is-nullable (first parser))
        (make-instance
         'alt
         :choice1 (make-instance 'con
                                 :first (derive (first parser) value)
                                 :second (second parser))
         :choice2 (make-instance 'con
                                 :first (make-instance
                                         'eps
                                         :generator (parse (first parser)
                                                           (make-empty-stream)))
                                 :second (derive (second parser)
                                                 value)))
        (make-instance 'con
                       :first (derive (first parser) value)
                       :second (second parser))))
  (:method ((parser alt) value)
    (cond ((is-empty (choice1 parser)) (derive (choice2 parser) value))
          ((is-empty (choice2 parser)) (derive (choice1 parser) value))
          (t (make-instance 'alt
                            :choice1 (derive (choice1 parser) value)
                            :choice2 (derive (choice2 parser) value)))))
  (:method ((parser rep) value)
    (make-instance 'red
                   :parser (make-instance 'con
                                          :first (derive (slot-value parser
                                                                     'parser)
                                                         value)
                                          :second parser)
                   :f (lambda (alist) (cons (x alist) (y alist)))))
  (:method ((parser red) value)
    (make-instance 'red
                   :parser (derive (slot-value parser 'parser) value)
                   :f (slot-value parser 'f))))

(defgeneric parse-full (parser &optional input-stream)
  (:method ((parser parser) &optional (input-stream *standard-input*))
    (handler-case
        (parse-full (derive parser (read-char input-stream)) input-stream)
      (end-of-file () (make-string-input-stream (parse-null parser)))))
  (:method ((parser red) &optional (input-stream *standard-input*))
    (let ((a (parse-full (slot-value parser 'parser) input-stream)))
      (funcall (slot-value parser 'f) a))))

(defgeneric parse (parser &optional input-stream)
  (:method ((parser parser) &optional (input-stream *standard-input*))
    (handler-case
        (combine-even (parse (derive parser (read-char input-stream))
                             input-stream)
                      (stream-cons (parse-full parser (make-empty-stream))
                                   input-stream))
      (end-of-file () (make-string-input-stream (parse-null parser)))))
  (:method ((parser eq-t) &optional (input-stream *standard-input*))
    (if (equal (peek-char 'character input-stream nil)
               (slot-value parser 'value))
        (make-concatenated-stream input-stream (make-empty-stream))
        (make-empty-stream)))
  (:method ((parser emp) &optional (input-stream *standard-input*))
    (declare (ignore input-stream))
    (make-empty-stream))
  (:method ((parser eps) &optional (input-stream *standard-input*))
    (values (generator parser) input-stream))
  (:method ((parser red) &optional (input-stream *standard-input*))
    (multiple-value-bind (a rest)
        (parse (slot-value parser 'parser) input-stream)
      (values (funcall (slot-value parser 'f) a) rest))))

(defgeneric update-child-based-attributes (parser change)
  (:method ((parser parser) change)
    (declare (ignore change))
    (values))
  (:method ((parser eps) change)
    (or-with change (setf (parse-null parser) (generator parser))))
  (:method ((parser con) change)
    (when (not (find parser (seen change)))
      (push parser (seen change))
      (update-child-based-attributes (first parser) change)
      (update-child-based-attributes (second parser) change)
      (setf (initializedp parser) t))
    (or-with change
             (setf (parse-null parser)
                   (make-instance 'concatenation
                                  :x (parse-null (first parser))
                                  :y (parse-null (second parser)))))
    (or-with change
             (setf (is-empty parser) (or (is-empty (first parser))
                                         (is-empty (second parser)))))
    (or-with change
             (setf (is-nullable parser) (and (not (is-empty parser))
                                             (is-nullable (first parser))
                                             (is-nullable (second parser))))))
  (:method ((parser alt) change)
    (when (not (find parser (seen change)))
      (push parser (seen change))
      (update-child-based-attributes (choice1 parser) change)
      (update-child-based-attributes (choice2 parser) change)
      (setf (initializedp parser) t))
    (or-with change
             (setf (parse-null parser) (append (parse-null (choice1 parser))
                                               (parse-null (choice2 parser)))))
    (or-with change (setf (is-empty parser) (and (is-empty (choice1 parser))
                                                 (is-empty (choice2 parser)))))
    (or-with change
             (setf (is-nullable parser)
                   (and (not (is-empty parser))
                        (or (is-nullable (choice1 parser))
                            (is-nullable (choice2 parser)))))))  
  (:method ((parser rep) change)
    (when (not (find parser (seen change)))
      (push parser (seen change))
      (update-child-based-attributes (slot-value parser 'parser) change)
      (setf (initializedp parser) t)))
  (:method ((parser red) change)
    (when (not (find parser (seen change)))
      (push parser (seen change))
      (update-child-based-attributes (slot-value parser 'parser) change)
      (setf (initializedp parser) t))
    (or-with change
             (setf (parse-null parser)
                   (mapcar (slot-value parser 'f)
                           (parse-null (slot-value parser 'parser)))))
    (or-with change
             (setf (is-empty parser) (is-empty (slot-value parser 'parser))))
    (or-with change
             (setf (is-nullable parser)
                   (is-nullable (slot-value parser 'parser))))))

(defun choice (parser1 parser2)
  (make-instance 'alt :choice1 parser1 :choice2 parser2))

(defun concatenation (parser1 parser2)
  (make-instance 'con :first parser1 :second parser2))

(defun replication (parser)
  (make-instance 'rep :parser parser))

(defun combine-even (s1 s2)
  (cond ((not (is-empty-p s1)) (stream-cons (read-char s1) (combine-odd s1 s2)))
        ((not (is-empty-p s2)) (stream-cons (read-char s2) (combine-odd s1 s2)))
        (t (make-empty-stream))))

(defun combine-odd (s1 s2)
  (cond ((not (is-empty-p s2)) (stream-cons (read-char s2) (combine-odd s1 s2)))
        ((not (is-empty-p s1)) (stream-cons (read-char s1) (combine-odd s1 s2)))
        (t (make-empty-stream))))
