(defpackage lazy
  (:use #:cl)
  (:export #:delay #:force #:for-each-stream #:lazy-let
           #:cons-stream #:stream-car #:stream-cdr #:stream-caar
           #:stream* #:stream-nth #:append-streams #:stream-nthcdr
           #:stream-rest #:stream-member #:map-stream
           #:stream-remove-if
           #:make-lazy-input-stream
           #:repeatedly #:drop #:take #:make-stream-range))

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

(defun for-each-stream (stream &key (key #'identity))
  (if (endp stream)
      '()
      (cons (funcall key (stream-car stream))
            (for-each-stream (stream-cdr stream) :key key))))

;;; Data & Control Flow

(defmacro lazy-let ((&rest variables) &body body)
  `(let ,(mapcar #'car variables)
     (setf ,@(reduce #'append variables))
     ,@body))

;;; Objects

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

;;; Conses

(defmacro cons-stream (head tail)
  `(cons ,head (delay ,tail)))

(defun stream-car (stream)
  "Identical to CAR, just exists for naming consistency."
  (car stream))

(defun stream-cdr (stream)
  (force (cdr stream)))

(defun stream-caar (stream)
  "Identical to CAAR, just exists for naming consistency."
  (stream-car (stream-car stream)))

(defmacro stream* (&rest objects)
  `(if (= (length ,objects) 1)
      ,(car objects)
      (list* ,@(butlast objects) (delay ,(car (last objects))))))

(defun stream-nth (n stream)
  (stream-car (stream-nthcdr n stream)))

(defun append-streams (stream1 stream2)
  (if (endp stream1)
      stream2
      (cons-stream (stream-car stream1)
                   (append-streams (stream-cdr stream1) stream2))))

(defun stream-nthcdr (n stream)
  (loop
     for i from n downto 0
     for cdr = stream then (stream-cdr cdr)
     finally (return cdr)))

(defun stream-rest (stream)
  (stream-cdr stream))

(defun stream-member (item stream &key key test test-not)
  (if (funcall test item (funcall key (stream-car stream)))
      stream
      (stream-member item (stream-cdr stream) :key key :test test :test-not test-not)))

(defun map-stream (fn &rest streams)
  (if (some #'endp streams)
      '()
      (cons-stream (apply fn (mapcar #'stream-car streams))
                   (apply #'map-stream fn (mapcar #'stream-cdr streams)))))

;;; Sequences

(defun stream-remove-if (predicate stream &key (key #'identity))
  (cond ((endp stream) '())
        ((funcall predicate (funcall key (stream-car stream)))
         (stream-remove-if predicate (stream-cdr stream)))
        (t (cons-stream (stream-car stream)
                        (stream-remove-if predicate (stream-cdr stream)
                                          :key key)))))

;;; IO Streams

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

;;; Other

(defun repeatedly (function &rest arguments)
  (cons-stream (apply function arguments)
               (apply function arguments)))

(defun drop (n stream)
  (loop for i from 0 to n
       for cdr = stream then (stream-cdr cdr)
       finally (return cdr)))

(defun take (n stream)
  (loop for i from 0 to n
       for cdr = stream then (stream-cdr cdr)
       collecting (stream-car cdr)))

(defun make-stream-range (start &optional end (step 1))
  (if (if (plusp step)
          (< start end)
          (< end start))
      (cons-stream start
                   (make-stream-range (+ start step) end step))))
