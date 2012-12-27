(in-package #:yacc-is-dead-tests)

(def-suite api-tests
    :description "Test the external API, like unit tests are supposed to."
    :in yacc-is-dead-tests)

(in-suite api-tests)

(def-test should-treat-literal-as-eq-parser ()
  (is (equal '(x) (parse 'x '(x)))))

(def-test should-reduce-across-types ()
  (is (equal '("X") (parse (==> 'x #'symbol-name) '(x)))))

(def-test should-match-based-on-predicate ()
  (is (equal '(#\x) (parse (token #'characterp) '(#\x))))
  (is (equal '() (parse (token #'characterp) '(12)))))

(def-test should-survive-right-recursion ()
  (lazy-let ((right-recurse (choice (~ 'x right-recurse) *epsilon*)))
    (is (equal '((x x x x x x x)) (parse right-recurse '(x x x x x x x))))))

(def-test should-survive-left-recursion ()
  (lazy-let ((left-recurse (choice (~ left-recurse 'x) *epsilon*)))
    (is (equal '((((((((NIL . X) . X) . X) . X) . X) . X) . X))
               (parse left-recurse '(x x x x x x x))))))

(def-test should-normalize-left-recursion ()
  (lazy-let ((left-recurse (choice (==> (~ left-recurse 'x)
                                        (lambda (parse)
                                          (append (car parse)
                                                  (list (cdr parse)))))
                                   *epsilon*)))
    (is (equal '((x x x x x x x)) (parse left-recurse '(x x x x x x x))))))

(def-test should-survive-bidi-recursion ()
  (lazy-let ((bidi (choice (~ bidi '+ bidi) (==> 'x #'list))))
    (is (equal '(((X) + (X) + (X) + (X) + (X) + (X) + (X) + X) + X)
               (car (parse bidi '(x + x + x + x + x + x + x + x + x)))))))
