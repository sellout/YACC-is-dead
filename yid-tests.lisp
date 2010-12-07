(defpackage yid-tests
  (:use #:cl #:yid))

(in-package #:yid-tests)

(defclass reducible ()
  ((parser :initarg :parser :reader parser)))

(defun rule (parser)
  (make-instance 'reducible :parser parser))

(defun ==> (reducible reduction)
  (red (parser reducible) reduction))

(eval-when (:compile-toplevel :load-toplevel :execute)  
  (defmacro benchmark (&body body)
    (let ((startvar (gensym)))
      `(let ((,startvar (get-universal-time)))
         ,@body
         (- (get-universal-time) ,startvar)))))

(lazy-let ((s (eq-t #\s))
           (x (eq-t #\x))
           (xl (choice (==> (rule (concatenation x xl))
                            #'identity)
                       (==> (rule x) (lambda (x) (list x)))))
           (ex (eq-t #\x))
           (exl (choice (==> (rule (concatenation ex exl))
                             #'identity)
                        epsilon))
           (lx (eq-t #\x))
           (lxl (choice (==> (rule (concatenation lxl lx))
                             #'identity)
                        (==> (rule x) (lambda (x) (list x)))))
           (lex (eq-t #\x))
           (lexl (choice (==> (rule (concatenation lexl lex))
                              #'identity)
                         epsilon))
           (lpar (eq-t #\())
           (rpar (eq-t #\)))
           (par-lexl (==> (rule (concatenation lpar lexl rpar))
                          (lambda (cat) (cadr cat)))))
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

  (lazy-let ((sx (choice (==> (rule (concatenate lpar sxl rpar))
                              (lambda (cat) (car (cdr cat))))
                         (==> (rule s)
                              (lambda (c)
                                (make-instance 'sx-sym :symbol c)))))
             (sxl (==> (rule (replication sx))
                       (lambda (sxlist)
                         (make-instance 'sx-list :list sxlist)))))
    (defclass exp ()
      ())

    (let ((one (make-instance 'exp)))
      (defclass sum (exp)
        ((e1 :initarg :e1)
         (e2 :initarg :e2)))

      (lazy-let ((exp (choice (==> (rule x)
                                   (lambda (x) (declare (ignore x)) one))
                              (==> (rule (concatenation exp s exp))
                                   (lambda (cat)
                                     (make-instance 'sum
                                                    :e1 (car cat)
                                                    :e2 (cddr cat))))
                              (==> (rule (concatenation exp s x))
                                   (lambda (cat)
                                     (make-instance 'sum
                                                    :e1 (car cat) :e2 one)))
                              (==> (rule (concatenation x s exp))
                                   (lambda (cat)
                                     (make-instance 'sum
                                                    :e1 one :e2 (cddr cat))))
                              (==> (rule exp) #'identity)
                              (==> (rule epsilon)
                                   (lambda (ep) (declare (ignore ep)) one)))))
        
        (defun main ()
          (let ((in (make-lazy-input-stream (make-string-input-stream "xxxxx"))))
            (let ((parses (parse xl in)))
              (format t "~a~%" (car parses)))
            (let ((parses2 (parse exl in)))
              (format t "parses2.head = ~a~@
                         parses2.tail.head = ~a~@
                         parses2.tail.tail.head = ~a~%"
                      (car parses2)
                      (car (yid::stream-cdr parses2))
                      (car (yid::stream-cdr (yid::stream-cdr parses2)))))
            (let ((parses3 (parse lxl in)))
              (format t "~a~%" (car parses3)))
            (let ((parses4 (parse lexl in)))
              (format t "~a~%" (car parses4))))
          (let* ((in2 (make-lazy-input-stream (make-string-input-stream "(xxxx)")))
                 (parses5 (parse-full par-lexl in2)))
            (format t "~a~%" (car parses5)))
          (finish-output)
          (let* ((xin (make-lazy-input-stream (make-string-input-stream "xsxsxsxsx")))
                 (xparse1 (parse-full exp xin)))
            (format t "xparse1: ~a~%" xparse1))
          (let* ((sin (make-lazy-input-stream (make-string-input-stream "(sss(sss(s)(s)sss)ss(s))")))
                 (sparse1 (parse-full sx sin)))
            (format t "~a~%" sparse1))
          (let ((strings (list "(ssss()ss()s()ss(sss(s)(s)sss)ss(s))"
                               "(ss()ss()ssssssssssssssssssssssssssssss()s(sss(s)(s)sss)ss(s))"
                               "(ss(())ss()ss()s((s)(s)sss)ss(s))"
                               "(ss((s))ss()ss()s((s)(s)sss)ss(s)(s)(s))"))
                (trials (list 9 19 117 978 9171 118170 518170)))
            (dolist (trial trials)
              (let* ((sexp-ns (apply #'concatenate
                                     'string
                                     (loop for i from 1 to trial
                                        for j = (random j)
                                        collecting (nth (mod (abs j)
                                                             (length strings))
                                                        strings))))
                     (input (make-lazy-input-stream (make-string-input-stream sexp-ns)))
                     (count 0)
                     (time (benchmark (loop until (endp input)
                                         do (multiple-value-bind (tree rest)
                                                (parse sx input)
                                              (declare (ignore tree))
                                              (setf count (1+ count)
                                                    input rest))))))
                (format t "count: ~d~@
                           sexp~ds.length: ~d~@
                           time: ~d~%"
                        count trial (length sexp-ns) time)))))))))
