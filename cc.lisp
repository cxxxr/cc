(in-package :cc)

(defvar *variable-environment*)

(defclass ast () ())

(defclass binary-operator (ast)
  ((op :initarg :op :reader ast-op)
   (x :initarg :x :reader ast-x)
   (y :initarg :y :reader ast-y)))

(defclass add (binary-operator) ())
(defclass sub (binary-operator) ())
(defclass mul (binary-operator) ())
(defclass div (binary-operator) ())
(defclass assign (binary-operator) ())

(defclass unary-operator (ast)
  ((op :initarg :op :reader ast-op)
   (x :initarg :x :reader ast-x)))

(defclass negate (unary-operator) ())

(defclass ident (ast)
  ((name :initarg :name :reader ident-name)
   (num :initarg :num :reader ident-num)))

(defmethod initialize-instance :around ((ident ident) &rest initargs &key name)
  (let ((n (or (gethash name *variable-environment*)
               (setf (gethash name *variable-environment*)
                     (hash-table-count *variable-environment*)))))
    (apply #'call-next-method ident
           :num n
           initargs)))

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

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun binary-expression (a b c)
  (make-instance (ecase b
                   (#\= 'assign)
                   (#\+ 'add)
                   (#\- 'sub)
                   (#\* 'mul)
                   (#\/ 'div))
                 :x a
                 :y c))

(defun unary-expression (a b)
  (ecase a
    (#\+ b)
    (#\- (make-instance 'negate :x b))))

(defun paren-expression (a b c)
  (declare (ignore a c))
  b)

(defun ident (name)
  (make-instance 'ident :name name))
)

(yacc:define-parser *parser*
  (:start-symbol expression)
  (:terminals (#\+ #\- #\* #\/ #\( #\) #\= :number :word))
  (:precedence ((:right #\=) (:left #\* #\/) (:left #\+ #\-)))
  (expression
   (expression #\= expression #'binary-expression)
   (expression #\+ expression #'binary-expression)
   (expression #\- expression #'binary-expression)
   (expression #\* expression #'binary-expression)
   (expression #\/ expression #'binary-expression)
   term)
  (term
   :number
   (:word #'ident)
   (#\+ expression #'unary-expression)
   (#\- expression #'unary-expression)
   (#\( expression #\) #'paren-expression)))

(defun parse (code)
  (let ((*variable-environment* (make-hash-table :test 'equal)))
    (yacc:parse-with-lexer
     (let ((scanner (make-scanner 'cc code)))
       (lambda ()
         (lex scanner)))
     *parser*)))

(defun gen-svm (ast)
  (let ((code '()))
    (labels ((gen-svm (ast)
               (trivia:match ast
                 ((add x y)
                  (gen-svm x)
                  (gen-svm y)
                  (push '(+) code))
                 ((sub x y)
                  (gen-svm x)
                  (gen-svm y)
                  (push '(-) code))
                 ((mul x y)
                  (gen-svm x)
                  (gen-svm y)
                  (push '(*) code))
                 ((div x y)
                  (gen-svm x)
                  (gen-svm y)
                  (push '(/) code))
                 ((negate x)
                  (gen-svm x)
                  (push `(negate) code))
                 (x
                  (push `(push ,x) code)))))
      (gen-svm ast)
      (nreverse code))))

(defun svm-to-wat (code)
  `(module
    (func $main (result i32)
          ,@(loop :for instr :in code
                  :append (trivia:ematch instr
                             ((list 'push arg)
                              `((i32.const ,arg)))
                             ((list '+)
                              `((i32.add)))
                             ((list '-)
                              `((i32.sub)))
                             ((list '*)
                              `((i32.mul)))
                             ((list '/)
                              `((i32.div_s)))
                             ((list 'negate)
                              `((i32.const -1)
                                (i32.mul))))))
    (export "main" (func $main))))

(defun print-wat (wat)
  (let ((*print-case* :downcase))
    (pprint wat)))

(defun wat-to-wasm (wat)
  (with-open-file (*standard-output* "wasm/simple.wat"
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
    (print-wat wat))
  (uiop:run-program (format nil "cd ~A; wat2wasm simple.wat -o simple.wasm"
                            (asdf:system-relative-pathname :cc "wasm/")))
  (alexandria:read-file-into-byte-vector "wasm/simple.wasm"))

(defun octets-to-js-array (octets)
  (format nil "[~{~D~^,~}]" (coerce octets 'list)))

(defun gen-html-file (wasm-octets)
  (with-open-file (*standard-output* "./index.html"
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
    (zen:render-file "./template.html" :wasm (octets-to-js-array wasm-octets))))

(defun comp (code)
  (let* ((expr (parse code))
         (wat (svm-to-wat (gen-svm expr))))
    (gen-html-file
     (wat-to-wasm wat))))
