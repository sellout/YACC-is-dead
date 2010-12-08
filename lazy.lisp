(defpackage lazy
  (:use #:cl)
  (:export #:delay #:force
           #:cons-stream #:make-lazy-input-stream
           #:stream-car #:stream-cdr
           #:force-stream #:map-stream #:append-streams
           #:lazy-let))

(in-package #:lazy)

(defclass lazy-form ()
  ((form :initarg :form :reader form)
   (forced-value :accessor forced-value)))

(defmethod make-load-form ((object lazy-form) &optional environment)
  (declare (ignore environment))
  `(make-instance ',(class-name (class-of object)) :form ,(form object)))

(defmethod print-object ((c lazy-form) stream)
  (print-unreadable-object (c stream :type t)
    (print-object (if (slot-boundp c 'forced-value)
                      (forced-value c)
                      :delayed)
                  stream)))

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

(defun make-lazy-input-stream (input-stream)
  (if (subtypep (stream-element-type input-stream) 'character)
      (make-lazy-character-input-stream input-stream)
      (make-lazy-byte-input-stream input-stream)))

(defun make-lazy-character-input-stream (input-stream)
  (handler-case
      (cons-stream (read-char input-stream)
                   (make-lazy-character-input-stream input-stream))
    (end-of-file () '())))

(defun make-lazy-byte-input-stream (input-stream)
  (handler-case
      (cons-stream (read-byte input-stream)
                   (make-lazy-byte-input-stream input-stream))
    (end-of-file () '())))

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
