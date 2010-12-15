(in-package #:yid-tests)

(def-suite racket-tests
    :description "Tests from the Racket implementation of YID."
    :in yid-tests)

(in-suite racket-tests)

(test simple
  (let ((simple (~ 'a 'b)))
    (is (eq nil (yid::nullablep simple)))))

(test xylist
  (lazy-let ((xylist (choice (~ 'x yxlist)
                             (choice 'x *epsilon*)))
             (yxlist (choice (~ 'y xylist)
                             'y)))
    (is (eq t (yid::nullablep xylist)))
    (is (eq nil (yid::nullablep yxlist)))
    (is (eq t (recognizesp xylist '(x y x y))))))

(test alist
  (lazy-let ((alist (choice (~ alist 'a) 'a))
             (alist?? (choice (~ alist 'a) *epsilon*)))
    (is (eq nil (yid::nullablep alist)))
    (is (eq t (yid::nullablep alist??)))
    (is (eq nil (recognizesp alist '(a a a b))))
    (is (eq t (recognizesp alist '(a a a a))))))

(test rlist
  (lazy-let ((rlist (choice (==> (~ 'r rlist)
                                 (lambda (parse) (cons 'a (cdr parse))))
                            (eps (list nil)))))
    (is (eq t (recognizesp rlist '(r r))))
    (is (equal (yid::parse-derive rlist 'r) (yid::parse-derive rlist 'r)))
    (is (equal (yid::parse-derive (yid::parse-derive rlist 'r) 'r)
               (yid::parse-derive (yid::parse-derive rlist 'r) 'r)))
    (is (equal (list '(a a))
               (yid::parse-null (yid::parse-derive (yid::parse-derive rlist 'r)
                                                   'r))))
    (is (equal (list '(a a a))
               (yid::parse-null (yid::parse-derive
                                 (yid::parse-derive (yid::parse-derive rlist
                                                                       'r)
                                                    'r)
                                 'r))))
    (is (equal (list '(a a a a a a a)) (parse rlist '(r r r r r r r))))))

(test nlist
  (lazy-let ((nlist (choice (==> (~ (token #'integerp) nlist) #'identity)
                            (eps (list nil)))))
    (is (equal (list '(1 2 3 4 5)) (parse nlist '(1 2 3 4 5))))))

(test llist
  (lazy-let ((llist (choice (==> (~ llist (token #'symbolp))
                                 (lambda (parse)
                                   (append (car parse) (list (cdr parse)))))
                            (eps (list nil)))))
    (is (equal (list '(a b c d e)) (parse llist '(a b c d e))))))

#|
(test recognition
  (lazy-let ((s (choice (~ s '+ s) 'n))
             (good-input '(N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N))
             (bad-input '(N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + N + + N)))
    (time (recognizesp s good-input))
    (time (recognizesp s bad-input))))
|#

(defun make-sx-bench (n)
  (loop for i from 0 to n collecting 'atom))

(defvar sxin
  `(lp
    lp lp lp lp lp lp lp lp lp lp
    atom
    rp rp rp rp rp rp rp rp rp rp
    ,@(make-sx-bench 100)
    lp lp lp atom rp rp rp
    rp))

(test benchmark
  (lazy-let ((sx-list (choice (~ sx sx-list)
                              (eps '())))
             (sx (choice (==> (~ 'lp sx-list 'rp)
                              (lambda (parse) (cadr parse)))
                         'atom)))
    (time (parse sx-list sxin :compact #'compact))))