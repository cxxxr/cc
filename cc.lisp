(in-package :cc)

(defclass ast () ())

(defclass binary-operator (ast)
  ((op :initarg :op :reader ast-op)
   (x :initarg :x :reader ast-x)
   (y :initarg :y :reader ast-y)))

(defclass unary-operator (ast)
  ((op :initarg :op :reader ast-op)
   (x :initarg :x :reader ast-x)))

(define-lexer cc
  ("\\s+" :skip)
  ("$" nil)
  ("[a-zA-Z_][0-9a-zA-Z_]*"
   (values :word (text)))
  ("[0-9]+"
   (values :number
           (parse-integer (text))))
  ("."
   (let ((char (char (text) 0)))
     (values char char))))

(parsergen:defparser parse
  ((toplevel expression))
  (expression
   ((expression #\+ term)
    (make-instance 'binary-operator :op '+ :x $1 :y $3))
   ((expression #\- term)
    (make-instance 'binary-operator :op '- :x $1 :y $3))
   ((term)
    $1))
  (term
   ((term #\* factor)
    (make-instance 'binary-operator :op '* :x $1 :y $3))
   ((term #\/ factor)
    (make-instance 'binary-operator :op '/ :x $1 :y $3))
   ((factor)
    $1))
  (factor
   ((#\( expression #\))
    $2)
   ((#\+ :number)
    $2)
   ((#\- :number)
    (make-instance 'unary-operator :op '- :x $2))
   ((:number)
    $1)))

(defun gen-svm (ast)
  (let ((code '()))
    (labels ((gen-svm (ast)
               (trivia:match ast
                 ((binary-operator op x y)
                  (gen-svm x)
                  (gen-svm y)
                  (push `(,op) code))
                 ((unary-operator op x)
                  (gen-svm x)
                  (push `(,op) code))
                 (x
                  (push `(push ,x) code)))))
      (gen-svm ast)
      (nreverse code))))

(defun comp (code)
  (let ((expr (parse (let ((scanner (make-scanner 'cc code)))
                       (lambda ()
                         (lex scanner))))))
    (gen-svm expr)))
