(defpackage yid
  (:use #:cl)
  (:export #:parser
           #:eq-t #:emp #:eps #:con #:alt #:rep #:red
           #:epsilon
           #:parse-full #:parse
           #:choice #:concatenation #:replication
           ;; FIXME: probably shouldn't export these
           #:lazy-let #:make-lazy-string-input-stream))

(in-package #:yid)

;;; Giving us some laziness to take advantage of

(defclass lazy-form ()
  ((form :initarg :form :reader form)
   (forced-value :accessor forced-value)))

(defmethod make-load-form ((object lazy-form) &optional environment)
  (declare (ignore environment))
  `(make-instance ',(class-name (class-of object)) :form ,(form object)))

(defmacro delay (form &optional (class 'lazy-form))
  `(make-instance ',class :form (lambda () ,form)))

(defgeneric force (form)
  (:documentation "Acts as IDENTITY on all objects except LAZY-FORMs (created
                   with DELAY), in which case it evaluates them.")
  (:method (form)
    form)
  (:method ((form lazy-form))
    (if (slot-boundp form 'forced-value)
        (forced-value form)
        (setf (forced-value form) (funcall (form form))))))

(defmacro cons-stream (head tail)
  `(cons ,head (delay ,tail)))

(defun make-lazy-string-input-stream (string)
  (if (> (length string) 0)
      (cons-stream (aref string 0)
                   (delay (make-lazy-string-input-stream (subseq string 1))))
      '()))

;; Probably don't need STREAM-CAR and -CDR, as CDR will return the lazy value,
;; then it'll be expanded as soon as a method is called on it.
(defun stream-car (stream)
  (car stream))

(defun stream-cdr (stream)
  (force (cdr stream)))

(defun force-stream (stream)
  (if (endp stream)
      '()
      (cons (stream-car stream) (force-stream (stream-cdr stream)))))

(defun map-stream (fn &rest streams)
  (if (some #'endp streams)
      '()
      (cons-stream (apply fn (mapcar #'stream-car streams))
                   (apply #'map-stream fn (mapcar #'stream-cdr streams)))))

(defun append-streams (stream1 stream2)
  (if (endp stream1)
      stream2
      (cons-stream (stream-car stream1)
                   (append-streams (stream-cdr stream1) stream2))))

(defmacro lazy-let ((&rest variables) &body body)
  `(let ,(mapcar #'car variables)
     (setf ,@(reduce #'append variables))
     ,@body))

(defmethod no-applicable-method :around (gf &rest args)
  (let* ((found-lazy-p nil)
         (forced-args (mapcar (lambda (arg)
                                (if (typep arg 'lazy-form)
                                    (progn
                                      (setf found-lazy-p t)
                                      (force arg))
                                    arg))
                              args)))
    (if found-lazy-p
        (apply gf forced-args)
        (call-next-method))))

;;; Ok, here's the real stuff

(defclass change-cell ()
  ((changedp :initform nil :accessor changedp)
   (seen :initform () :accessor seen)))

(defmethod or-with ((object change-cell) changed)
  (or (slot-value object 'changedp)
      (setf (slot-value object 'changedp) changed)))

(defclass parser ()
  ((parse-null :initform '())
   (is-empty :initform nil)
   (is-nullable :initform nil)
   (initializedp :initform nil :accessor initializedp)
   (cache :initform (make-hash-table :test #'equal) :reader cache)))

(defclass eq-t (parser)
  ((value :initarg :value)
   (parse-null :initform '())
   (is-empty :initform nil)
   (is-nullable :initform nil)))

(defun eq-t (value)
  (delay (make-instance 'eq-t :value value)))

(defclass emp (parser)
  ((parse-null :initform '())
   (is-empty :initform t)
   (is-nullable :initform nil)))

(defun emp ()
  (delay (make-instance 'emp)))

(defclass eps (parser)
  ((generator :initarg :generator :reader generator)
   (is-empty :initform nil)
   (is-nullable :initform t)))

(defun eps (generator)
  (delay (make-instance 'eps :generator generator)))

(defparameter epsilon (eps (cons-stream '() '())))

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
      (progn
        (initialize-parser parser)
        (slot-value parser 'parse-null))))

(defmethod is-nullable ((parser parser))
  (if (is-empty parser)
      nil
      (progn
        (initialize-parser parser)
        (slot-value parser 'is-nullable))))

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
    (cond ((is-empty parser) (emp))
          ((gethash value (cache parser)) (gethash value (cache parser)))
          (t (setf (gethash value (cache parser)) (call-next-method)))))
  (:method ((parser eq-t) value)
    (if (equal (slot-value parser 'value) value)
        (eps (cons-stream value '()))
        (emp)))
  (:method ((parser emp) value)
    (declare (ignore value))
    parser)
  (:method ((parser eps) value)
    (declare (ignore value))
    (emp))
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
    (red (con (derive (slot-value parser 'parser) value)
              parser)
         #'identity))
  (:method ((parser red) value)
    (red (derive (slot-value parser 'parser) value)
         (slot-value parser 'f))))

(defgeneric parse-full (parser &optional input-stream)
  (:method ((parser parser) &optional (input-stream *standard-input*))
    (if (endp input-stream)
        (parse-null parser)
        (parse-full (derive parser (stream-car input-stream))
                    (stream-cdr input-stream))))
  (:method ((parser red) &optional (input-stream *standard-input*))
    (map-stream (lambda (a) (funcall (slot-value parser 'f) a))
                (parse-full (slot-value parser 'parser) input-stream))))

(defgeneric parse (parser &optional input-stream)
  (:method ((parser parser) &optional (input-stream *standard-input*))
    (if (endp input-stream)
        (mapcar (lambda (a) (list a '()))
                (parse-null parser))
        (combine-even (parse (derive parser (stream-car input-stream))
                             (stream-cdr input-stream))
                      (map-stream (lambda (a) (list a input-stream))
                                  (parse-full parser '())))))
  (:method ((parser eq-t) &optional (input-stream *standard-input*))
    (if (equal (peek-char nil input-stream nil)
               (slot-value parser 'value))
        (cons-stream (list (stream-car input-stream) (stream-cdr input-stream))
                     '())
        '()))
  (:method ((parser emp) &optional (input-stream *standard-input*))
    (declare (ignore input-stream))
    '())
  (:method ((parser eps) &optional (input-stream *standard-input*))
    (map-stream (lambda (a) (list a input-stream)) (generator parser)))
  (:method ((parser red) &optional (input-stream *standard-input*))
    (map-stream (lambda (result)
                  (destructuring-bind (a &rest rest) result
                    (cons (funcall (slot-value parser 'f) a) rest)))
                (parse (slot-value parser 'parser) input-stream))))

(defgeneric update-child-based-attributes (parser change)
  (:method :before ((parser parser) change)
           (declare (ignore change)))
  (:method ((parser parser) change)
    (declare (ignore change))
    (values))
  (:method ((parser eps) change)
    (or-with change
             (setf (parse-null parser) (force-stream (generator parser)))))
  (:method ((parser con) change)
    (when (not (find parser (seen change)))
      (push parser (seen change))
      (update-child-based-attributes (first* parser) change)
      (update-child-based-attributes (second* parser) change)
      (setf (initializedp parser) t))
    (or-with change
             (setf (parse-null parser)
                   (remove-duplicates
                    (mapcar (lambda (a) (mapcan (lambda (b) (cons a b))
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


(defmacro concatenation (&rest parsers)
  (if (= (length parsers) 1)
      `,(car parsers)
      `(con ,(car parsers) (concatenation ,@(cdr parsers)))))

(defmacro replication (parser)
  `(rep ,parser))

(defun combine-even (s1 s2)
  (cond ((not (endp s1))
         (cons-stream (stream-car s1) (combine-odd (stream-cdr s1) s2)))
        ((not (endp s2))
         (cons-stream (stream-car s2) (combine-odd s1 (stream-cdr s2))))
        (t '())))

(defun combine-odd (s1 s2)
  (cond ((not (endp s2))
         (cons-stream (stream-car s2) (combine-even s1 (stream-cdr s2))))
        ((not (endp s1))
         (cons-stream (stream-car s1) (combine-even (stream-cdr s1) s2)))
        (t '())))
