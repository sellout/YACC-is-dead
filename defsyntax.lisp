(in-package #:yacc-is-dead)

(defun make-new-parser ()
  "Creates a new DEFSYNTAX parser, for holding rule-sets."
  (make-hash-table))

(defvar *parser* (make-new-parser)
  "The default DEFSYNTAX parser, which is modified by DEFSYNTAX.")

(defun get-type-parser (type &optional (parser *parser*))
  "This combines the rules for the specified type with the rules for all of its
   subtypes, recursively."
  (labels ((get-subtype-parsers (type)
             (let ((self (gethash type parser))
                   (sub (let ((class (find-class type nil)))
                          (when class
                            (mapcan (lambda (type)
                                      (get-subtype-parsers (class-name type)))
                                    (closer-mop:class-direct-subclasses class))))))
               (if self (cons self sub) sub))))
    (let ((parsers (remove-duplicates (get-subtype-parsers type))))
      (if (< 1 (length parsers))
          (eval `(choice ,@parsers))
          (car parsers)))))

(defun parse-rule (rule &optional (stream *standard-input*) (parser *parser*))
  "This parses an entire input stream, returning an object that parses according
   to the parser rule named as its first argument."
  (parse (get-type-parser rule parser) (make-lazy-input-stream stream)))

(defmacro tokenize-string (string)
  ``(==> (~ ,@(coerce ,string 'list))
         #'list))

(defun replace-vars-with-types (rule)
    "Returns an alist of variables and functions that extract the value from
     the parsed form."
    (let ((variables)
          (ignorables))
      (values
       (let ((i 0))
         (mapcar (lambda (subrule)
                   (prog1
                       (etypecase subrule
                         (string (let ((var (gensym)))
                                   (push var variables)
                                   (push var ignorables))
                                 (tokenize-string subrule))
                         (list (push (car subrule) variables)
                               (let ((type (cadr subrule)))
                                 (typecase type
                                   (symbol `(get-type-parser ',type *parser*))
                                   (string (tokenize-string type))
                                   (t `(==> ,type #'list)))))
                         (t (let ((var (gensym)))
                              (push var variables)
                              (push var ignorables))
                            `(get-type-parser ',subrule *parser*)))
                     (incf i)))
                 rule))
       (reverse variables)
       ignorables)))

(defun follow-path-through-tree (path tree)
  (if path
      (follow-path-through-tree (cdr path) (nth (car path) tree))
      tree))

(defmacro with-destructured-parse-tree
    ((parse-tree variables ignorables) &body template)
  `(destructuring-bind ,variables ,parse-tree
     (declare (ignore ,@ignorables))
     ,@template))

(defmacro defsyntax ((&rest rule) type &body body)
  "This creates a new syntax and adds it to the parser. The rule is a list of
   tokens and parser constructs. Either can be bound to a variable with the form
   (variable rule). Those variables are then available to the body, which
   returns the value to insert into the AST.

   The type parameter serves two purposes. One is to give this rule a name so it
   can be referenced in other rules. The other purpose exists when the rule is
   the name of a standard-class - not only will the rule for that name be used,
   but also any rules that match the name of subclasses, allowing you to take
   advantage of inheritance in your syntax definitions."
  (multiple-value-bind (expanded-rule variables ignorables)
      (replace-vars-with-types rule)
    `(let ((new-reduction (==> (~ ,@expanded-rule)
                               (lambda (parse-tree)
                                 (declare (ignorable parse-tree))
                                 (with-destructured-parse-tree
                                     (parse-tree ,variables ,ignorables)
                                   ,@body))))
           (old-rule (gethash ',type *parser*)))       
       (setf (gethash ',type *parser*) (if old-rule
                                           (choice new-reduction old-rule)
                                           new-reduction))
       ',type)))
