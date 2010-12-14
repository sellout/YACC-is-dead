(defpackage yid-tests
  (:use #:cl #:yid #:lazy #:fiveam))

(in-package #:yid-tests)

(def-suite scala-tests
    :description "Tests from the Scala implementation of YID.")

(in-suite scala-tests)

(test should-parse-right-recursion
  (lazy-let ((x (eq-t #\x))
             (xl (choice (==> (~ x xl) #'identity)
                         (==> x        (lambda (x) (list x)))))
             (in (make-lazy-input-stream (make-string-input-stream "xxxxx"))))
    (is (equal '(#\x #\x #\x #\x #\x)
               (car (stream-nth 0 (parse xl in)))))))

(test should-parse-right-recursion-with-epsilon
  (lazy-let ((ex (eq-t #\x))
             (exl (choice (==> (~ ex exl) #'identity)
                          epsilon))
             (in (make-lazy-input-stream (make-string-input-stream "xxxxx"))))
    (is (equal '(#\x #\x #\x #\x #\x)
               (car (stream-nth 0 (parse exl in)))))
    (is (equal '()
               (car (stream-nth 1 (parse exl in)))))
    (is (equal '(#\x)
               (car (stream-nth 2 (parse exl in)))))))

(test should-parse-left-recursion
  (lazy-let ((lx (eq-t #\x))
             (lxl (choice (==> (~ lxl lx) #'identity)
                          (==> lx          (lambda (x) (list x)))))
             (in (make-lazy-input-stream (make-string-input-stream "xxxxx"))))
    (is (equal '(((((#\x) . #\x) . #\x) . #\x) . #\x)
               (car (stream-nth 0 (parse lxl in)))))))

(test should-parse-left-recursion-with-epsilon
  (lazy-let ((lex (eq-t #\x))
             (lexl (choice (==> (~ lexl lex) #'identity)
                           epsilon))
             (in (make-lazy-input-stream (make-string-input-stream "xxxxx"))))
    (is (equal '(((((NIL . #\x) . #\x) . #\x) . #\x) . #\x)
               (car (stream-nth 0 (parse lexl in)))))))

(test should-parse-parenthesized-expression
  (lazy-let ((lex (eq-t #\x))
             (lexl (choice (==> (~ lexl lex) #'identity)
                           epsilon))
             (lpar (eq-t #\())
             (rpar (eq-t #\)))
             (par-lexl (==> (~ lpar lexl rpar) (lambda (cat) (cadr cat))))
             (in2 (make-lazy-input-stream (make-string-input-stream "(xxxx)"))))
    (is (equal '((((NIL . #\x) . #\x) . #\x) . #\x)
               (stream-nth 0 (parse-full par-lexl in2))))))

(defclass exp ()
  ())

(defclass sum (exp)
  ((e1 :initarg :e1)
   (e2 :initarg :e2)))

#|
(test should-parse-expression
  (lazy-let ((s (eq-t #\s))
             (x (eq-t #\x))
             (one (make-instance 'exp))
             (exp (choice (==> x
                               (lambda (x) (declare (ignore x)) one))
                          (==> (~ exp s exp)
                               (lambda (cat)
                                 (make-instance 'sum
                                                :e1 (car cat) :e2 (cddr cat))))
                          (==> (~ exp s x)
                               (lambda (cat)
                                 (make-instance 'sum :e1 (car cat) :e2 one)))
                          (==> (~ x s exp)
                               (lambda (cat)
                                 (make-instance 'sum :e1 one :e2 (cddr cat))))
                          (==> exp #'identity)
                          (==> epsilon
                               (lambda (ep) (declare (ignore ep)) one))))
             (xin (make-lazy-input-stream
                   (make-string-input-stream "xsxsxsxsx"))))
    (is (equal '()
               (car (parse-full exp xin))))))
|#

(defclass s-exp ()
  ())

(defclass sx-list (s-exp)
  ((list :initarg :list)))

(defclass sx-cons (s-exp)
  ((head :initarg :head)
   (tail :initarg :tail)))

(defclass sx-sym (s-exp)
  ((symbol :initarg :symbol)))

(defclass sx-nil (s-exp)
  ())

#|
(test should-parse-s-expression
  (lazy-let ((s (eq-t #\s))
             (lpar (eq-t #\())
             (rpar (eq-t #\)))
             (sx (choice (==> (~ lpar sxl rpar)
                              (lambda (cat) (car (cdr cat))))
                         (==> s
                              (lambda (c)
                                (make-instance 'sx-sym :symbol c)))))
             (sxl (==> (*+ sx)
                       (lambda (sxlist)
                         (make-instance 'sx-list :list sxlist))))
             (sin (make-lazy-input-stream
                   (make-string-input-stream "(sss(sss(s)(s)sss)ss(s))"))))
    (is (equal '()
               (car (parse-full sx sin))))))
|#

(eval-when (:compile-toplevel :load-toplevel :execute)  
  (defmacro benchmark (&body body)
    (let ((startvar (gensym)))
      `(let ((,startvar (get-universal-time)))
         ,@body
         (- (get-universal-time) ,startvar)))))

#|
(test should-benchmark
  (lazy-let ((s (eq-t #\s))
             (lpar (eq-t #\())
             (rpar (eq-t #\)))
             (sx (choice (==> (~ lpar sxl rpar)
                              (lambda (cat) (car (cdr cat))))
                         (==> s
                              (lambda (c)
                                (make-instance 'sx-sym :symbol c)))))
             (sxl (==> (*+ sx)
                       (lambda (sxlist)
                         (make-instance 'sx-list :list sxlist))))
             (strings (list "(ssss()ss()s()ss(sss(s)(s)sss)ss(s))"
                            "(ss()ss()ssssssssssssssssssssssssssssss()s(sss(s)(s)sss)ss(s))"
                            "(ss(())ss()ss()s((s)(s)sss)ss(s))"
                            "(ss((s))ss()ss()s((s)(s)sss)ss(s)(s)(s))"))
             (trials (list 9 19 117 978 9171 118170 518170)))
    (dolist (trial trials)
      (let* ((sexp-ns (apply #'concatenate
                             'string
                             (loop for i from 1 to trial
                                for j = (random i)
                                collecting (nth (mod (abs j)
                                                     (length strings))
                                                strings))))
             (input (make-lazy-input-stream (make-string-input-stream sexp-ns)))
             (count 0)
             (time (benchmark (loop until (endp input)
                                 do (destructuring-bind (tree rest)
                                        (parse sx input)
                                      (print (stream-car tree))
                                      (setf count (1+ count)
                                            input (force rest)))))))
        (format t "count: ~d~@
                   sexp~ds.length: ~d~@
                   time: ~d~%"
                count trial (length sexp-ns) time)))))
|#
