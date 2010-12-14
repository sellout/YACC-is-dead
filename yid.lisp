(defpackage yid
  (:use #:cl #:lazy)
  (:export #:parser #:eq-t #:eps #:con #:alt #:rep #:red
           #:*empty* #:*epsilon*
           #:parse-full #:parse
           #:choice #:~ #:*+ #:==>))

(in-package #:yid)

(defclass change-cell ()
  ((changedp :initform nil :accessor changedp)
   (seen :initform () :accessor seen)))

(defmethod or-with ((object change-cell) changed)
  (or (slot-value object 'changedp)
      (setf (slot-value object 'changedp) changed)))

(defclass parser ()
  ((parse-null :initform '())
   (is-empty :initform nil :initarg :emptyp)
   (is-nullable :initform nil :initarg :nullablep)
   (initializedp :initform nil :accessor initializedp)
   (cache :initform (make-hash-table :test #'equal) :reader cache)))

(defclass eq-t (parser)
  ((value :initarg :value)
   (parse-null :initform '())
   (is-empty :initform nil)
   (is-nullable :initform nil)))

(defmethod print-object ((c eq-t) stream)
  (print-unreadable-object (c stream :type t :identity t)
    (prin1 (slot-value c 'value) stream)))

(defun eq-t (value)
  (delay (make-instance 'eq-t :value value)))

(defvar *empty* (make-instance 'parser :emptyp t :nullablep nil))

(defclass eps (parser)
  ((generator :initarg :generator :reader generator)
   (is-empty :initform nil)
   (is-nullable :initform t)))

(defun eps (generator)
  (delay (make-instance 'eps :generator generator)))

(defvar *epsilon* (eps (cons-stream '() '())))

(defclass con (parser)
  ((first :initarg :first :reader first*)
   (second :initarg :second :reader second*)))

(defmacro con (first second)
  `(delay (make-instance 'con :first ,first :second ,second)))

(defclass alt (parser)
  ((choice1 :initarg :choice1 :reader choice1)
   (choice2 :initarg :choice2 :reader choice2)))

(defmacro alt (choice1 choice2)
  `(delay (make-instance 'alt :choice1 ,choice1 :choice2 ,choice2)))

(defclass rep (parser)
  ((parser :initarg :parser)
   (parse-null :initform '(nil))
   (is-empty :initform nil)
   (is-nullable :initform t)))

(defmacro rep (parser)
  `(delay (make-instance 'rep :parser ,parser)))

(defclass red (parser)
  ((parser :initarg :parser)
   (f :initarg :f)))

(defmacro red (parser f)
  `(delay (make-instance 'red :parser ,parser :f ,f)))

(defmethod parse-null ((parser parser))
  (if (is-empty parser)
      '()
      (slot-value parser 'parse-null)))

(defmethod is-nullable ((parser parser))
  (if (is-empty parser)
      nil
      (slot-value parser 'is-nullable)))

(defmethod is-empty ((parser parser))
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
  (when (not (eq (not (slot-value parser 'is-empty)) (not value)))
    (setf (slot-value parser 'is-empty) value)
    t))

(defun (setf is-nullable) (value parser)
  (when (not (eq (not (slot-value parser 'is-nullable)) (not value)))
    (setf (slot-value parser 'is-nullable) value)
    t))

(defun initialize-parser (parser)
  (when (not (initializedp parser))
    (setf (initializedp parser) t)
    (loop
       for change = (make-instance 'change-cell)
       do (update-child-based-attributes parser change)
       while (changedp change))))

(defgeneric derive (parser value)
  (:method :around ((parser parser) value)
    (cond ((is-empty parser) *empty*)
          ((gethash value (cache parser)) (gethash value (cache parser)))
          (t (setf (gethash value (cache parser)) (call-next-method)))))
  (:method ((parser eq-t) value)
    (if (equal (slot-value parser 'value) value)
        (eps (cons-stream value '()))
        *empty*))
  (:method ((parser (eql *empty*)) value)
    (declare (ignore value))
    (error "Cannot derive the empty parser"))
  (:method ((parser eps) value)
    (declare (ignore value))
    *empty*)
  (:method ((parser con) value)
    (if (is-nullable (first* parser))
        (alt (con (derive (first* parser) value)
                  (second* parser))
             (con (eps (map-stream #'first (parse (first* parser) '())))
                  (derive (second* parser) value)))
        (con (derive (first* parser) value) (second* parser))))
  (:method ((parser alt) value)
    (cond ((is-empty (choice1 parser)) (derive (choice2 parser) value))
          ((is-empty (choice2 parser)) (derive (choice1 parser) value))
          (t (alt (derive (choice1 parser) value)
                  (derive (choice2 parser) value)))))
  (:method ((parser rep) value)
    (con (derive (slot-value parser 'parser) value)
         parser))
  (:method ((parser red) value)
    (red (derive (slot-value parser 'parser) value)
         (slot-value parser 'f))))

(defgeneric parse-full (parser input-stream)
  (:method ((parser parser) input-stream)
    (if (endp input-stream)
        (parse-null parser)
        (parse-full (derive parser (stream-car input-stream))
                    (stream-cdr input-stream))))
  (:method ((parser red) input-stream)
    (map-stream (lambda (a) (funcall (slot-value parser 'f) a))
                (parse-full (slot-value parser 'parser) input-stream))))

(defgeneric parse (parser input-stream)
  (:method ((parser parser) input-stream)
    (if (endp input-stream)
        (mapcar (lambda (a) (list a '()))
                (parse-null parser))
        (combine-even (parse (derive parser (stream-car input-stream))
                             (stream-cdr input-stream))
                      (map-stream (lambda (a) (list a input-stream))
                                  (parse-full parser '())))))
  (:method ((parser eq-t) input-stream)
    (if (equal (stream-car input-stream) (slot-value parser 'value))
        (cons-stream (list (stream-car input-stream) (stream-cdr input-stream))
                     '())
        '()))
  (:method ((parser (eql *empty*)) input-stream)
    (declare (ignore input-stream))
    '())
  (:method ((parser eps) input-stream)
    (map-stream (lambda (a) (list a input-stream)) (generator parser)))
  (:method ((parser red) input-stream)
    (map-stream (lambda (result)
                  (destructuring-bind (a &rest rest) result
                    (cons (funcall (slot-value parser 'f) a) rest)))
                (parse (slot-value parser 'parser) input-stream))))

(defgeneric update-child-based-attributes (parser change)
  (:method ((parser parser) change)
    (declare (ignore change))
    (values))
  (:method ((parser eps) change)
    (or-with change
             (setf (parse-null parser) (for-each-stream (generator parser)))))
  (:method ((parser con) change)
    (when (not (find parser (seen change)))
      (push parser (seen change))
      (update-child-based-attributes (first* parser) change)
      (update-child-based-attributes (second* parser) change)
      (setf (initializedp parser) t))
    (or-with change
             (setf (parse-null parser)
                   (remove-duplicates
                    (mapcan (lambda (a)
                              (mapcar (lambda (b) (cons a b))
                                      (parse-null (second* parser))))
                            (parse-null (first* parser)))
                    :test #'equal)))
    (or-with change
             (setf (is-empty parser) (or (is-empty (first* parser))
                                         (is-empty (second* parser)))))
    (or-with change
             (setf (is-nullable parser) (and (not (is-empty parser))
                                             (is-nullable (first* parser))
                                             (is-nullable (second* parser))))))
  (:method ((parser alt) change)
    (when (not (find parser (seen change)))
      (push parser (seen change))
      (update-child-based-attributes (choice1 parser) change)
      (update-child-based-attributes (choice2 parser) change)
      (setf (initializedp parser) t))
    (or-with change
             (setf (parse-null parser)
                   (union (parse-null (choice1 parser))
                          (parse-null (choice2 parser)))))
    (or-with change
             (setf (is-empty parser) (and (is-empty (choice1 parser))
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
                   (remove-duplicates
                    (mapcar (slot-value parser 'f)
                            (parse-null (slot-value parser 'parser)))
                    :test #'equal)))
    (or-with change
             (setf (is-empty parser) (is-empty (slot-value parser 'parser))))
    (or-with change
             (setf (is-nullable parser)
                   (is-nullable (slot-value parser 'parser))))))

(defmacro choice (&rest parsers)
  (if (= (length parsers) 1)
      `,(car parsers)
      `(alt ,(car parsers) (choice ,@(cdr parsers)))))

(defmacro ~ (&rest parsers)
  (if (= (length parsers) 1)
      `,(car parsers)
      `(con ,(car parsers) (~ ,@(cdr parsers)))))

(defmacro *+ (parser)
  `(rep ,parser))

(defmacro ==> (parser function)
  `(red ,parser ,function))

(defun combine-even (s1 s2)
  (cond (s1 (cons-stream (stream-car s1) (combine-odd (stream-cdr s1) s2)))
        (s2 (cons-stream (stream-car s2) (combine-odd s1 (stream-cdr s2))))
        (t '())))

(defun combine-odd (s1 s2)
  (cond (s2 (cons-stream (stream-car s2) (combine-even s1 (stream-cdr s2))))
        (s1 (cons-stream (stream-car s1) (combine-even (stream-cdr s1) s2)))
        (t '())))
