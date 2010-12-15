(in-package #:yid-tests)

(def-suite api-tests
    :description "Test the external API, like unit tests are supposed to."
    :in yid-tests)

(in-suite api-tests)

(test should-treat-literal-as-eq-parser
  (is (equal '(x) (parse 'x '(x)))))

(test should-reduce-across-types
  (is (equal '("X") (parse (==> 'x #'symbol-name) '(x)))))