(in-package :cc)

(defclass ast () ())

(defclass program (ast)
  ((functions
    :initarg :functions
    :reader program-functions)))

(defclass func (ast)
  ((name
    :initarg :name
    :reader func-name)
   (parameters
    :initarg :parameters
    :reader func-parameters)
   (local-idents
    :accessor func-local-idents)
   (statements
    :initarg :statements
    :reader func-statements)))

(defclass binary-operator (ast)
  ((op :initarg :op :reader ast-op)
   (x :initarg :x :reader ast-x)
   (y :initarg :y :reader ast-y)))

(defclass binop-add (binary-operator) ())
(defclass binop-sub (binary-operator) ())
(defclass binop-mul (binary-operator) ())
(defclass binop-div (binary-operator) ())
(defclass binop-rem (binary-operator) ())
(defclass binop-assign (binary-operator) ())
(defclass binop-eq (binary-operator) ())
(defclass binop-ne (binary-operator) ())

(defclass unary-operator (ast)
  ((op :initarg :op :reader ast-op)
   (x :initarg :x :reader ast-x)))

(defclass unop-negate (unary-operator) ())

(defclass ident (ast)
  ((name :initarg :name :reader ident-name)
   (num :initarg :num :accessor ident-num)))

(defclass const (ast)
  ((value :initarg :value :reader const-value)))

(defun walk-ast (ast function)
  (funcall function ast
           (lambda ()
             (trivia:match ast
               ((program functions)
                (dolist (f functions)
                  (walk-ast f function)))
               ((func statements)
                (dolist (stat statements)
                  (walk-ast stat function)))
               ((binary-operator x y)
                (walk-ast x function)
                (walk-ast y function))
               ((unary-operator x)
                (walk-ast x function))))))

(define-lexer cc
  ("\\s+" :skip)
  ("$" nil)
  ("[a-zA-Z_][0-9a-zA-Z_]*"
   (values :word (text)))
  ("[0-9]+"
   (values :number
           (parse-integer (text))))
  ("=="
   (values :eq :eq))
  ("!="
   (values :ne :ne))
  ("."
   (let ((char (char (text) 0)))
     (values char char))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun accept-parsed-program (functions)
    (make-instance 'program :functions functions)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun accept-parsed-function (name parameters { stats })
    (declare (ignore { }))
    (make-instance 'func :name name :parameters parameters :statements stats)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun accept-parsed-parameters (paren parameters)
    (declare (ignore paren))
    (remove-if (lambda (s) (or (string= s ")") (string= s ",")))
               (alexandria:flatten parameters))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun concat-stats (a b c)
    (declare (ignore b))
    (cons a c)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun accept-parsed-binary-expression (a b c)
    (make-instance (ecase b
                     (#\= 'binop-assign)
                     (#\+ 'binop-add)
                     (#\- 'binop-sub)
                     (#\* 'binop-mul)
                     (#\/ 'binop-div)
                     (#\% 'binop-rem)
                     (:eq 'binop-eq)
                     (:ne 'binop-ne))
                   :x a
                   :y c)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun accept-parsed-unary-expression (a b)
    (ecase a
      (#\+ b)
      (#\- (make-instance 'unop-negate :x b)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun accept-parsed-paren-expression (a b c)
    (declare (ignore a c))
    b))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-ident (name)
    (make-instance 'ident :name name)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-const (x)
    (make-instance 'const :value x)))

(yacc:define-parser *parser*
  (:start-symbol program)
  (:terminals (#\+ #\- #\* #\/ #\( #\) #\= #\; #\% #\{ #\} #\, :number :word :eq :ne))
  (:precedence ((:left #\* #\/ #\%) (:left #\+ #\-) (:left :eq :ne) (:right #\=)))
  (program
   (functions #'accept-parsed-program))
  (functions
   ()
   (func functions #'cons))
  (func
   (:word parameters #\{ stats #\} #'accept-parsed-function))
  (parameters
   (#\( parameters* #'accept-parsed-parameters))
  (parameters*
   (:word parameters** #'list)
   (#\) #'list))
  (parameters**
   (#\, :word parameters** #'list)
   (#\) #'list))
  (stats
   ()
   (expression #\; stats #'concat-stats))
  (expression
   (expression #\= expression #'accept-parsed-binary-expression)
   (expression #\+ expression #'accept-parsed-binary-expression)
   (expression #\- expression #'accept-parsed-binary-expression)
   (expression #\* expression #'accept-parsed-binary-expression)
   (expression #\/ expression #'accept-parsed-binary-expression)
   (expression #\% expression #'accept-parsed-binary-expression)
   (expression :eq expression #'accept-parsed-binary-expression)
   (expression :ne expression #'accept-parsed-binary-expression)
   term)
  (term
   (:number #'make-const)
   (:word #'make-ident)
   (#\+ expression #'accept-parsed-unary-expression)
   (#\- expression #'accept-parsed-unary-expression)
   (#\( expression #\) #'accept-parsed-paren-expression)))

(defun parse (code)
  (yacc:parse-with-lexer
   (let ((scanner (make-scanner 'cc code)))
     (lambda ()
       (lex scanner)))
   *parser*))

(defun walk-ast-with-name-resolution (ast)
  (let (env)
    (declare (special env))
    (walk-ast ast
              (lambda (ast cont)
                (trivia:match ast
                  ((func)
                   (let ((env
                           (make-hash-table :test 'equal)))
                     (declare (special env))
                     (funcall cont)
                     (setf (func-local-idents ast)
                           (alexandria:hash-table-keys env))))
                  ((ident name)
                   (setf (ident-num ast)
                         (or (gethash name env)
                             (setf (gethash name env)
                                   (hash-table-count env))))
                   (funcall cont))
                  (_
                   (funcall cont)))))))

(defun signature-name (str)
  (intern (format nil "$~A" (string-upcase str))))

(defun gen-seq (&rest args)
  (apply #'append args))

(defun gen (op &rest args)
  (list (cons op args)))

(defgeneric gen-wat-aux (ast))

(defmethod gen-wat-aux ((ast program))
  (mapcar #'gen-wat-aux (program-functions ast)))

(defmethod gen-wat-aux ((ast func))
  (let ((name (func-name ast))
        (local-idents (func-local-idents ast))
        (statements (func-statements ast)))
    `(func ,(signature-name name) (result i32)
           ,@(loop :repeat (length local-idents) :collect '(local i32))
           ,@(loop :for statement* :on statements
                   :for statement := (first statement*)
                   :append (gen-wat-aux statement)
                   :when (rest statement*)
                   :append (gen 'drop)))))

(defmethod gen-wat-aux ((ast ident))
  (let ((num (ident-num ast)))
    (gen 'get_local num)))

(defmethod gen-wat-aux ((ast binop-assign))
  (let ((x (ast-x ast))
        (y (ast-y ast)))
    (trivia:ematch x
      ((ident num)
       (gen-seq (gen-wat-aux y)
                (gen 'tee_local num))))))

(defmethod gen-wat-aux ((ast binary-operator))
  (gen-seq (gen-wat-aux (ast-x ast))
           (gen-wat-aux (ast-y ast))
           (gen (etypecase ast
                  (binop-add 'i32.add)
                  (binop-sub 'i32.sub)
                  (binop-mul 'i32.mul)
                  (binop-div 'i32.div)
                  (binop-rem 'i32.rem_s)
                  (binop-eq 'i32.eq)
                  (binop-ne 'i32.ne)))))

(defmethod gen-wat-aux ((ast unop-negate))
  (gen-seq (gen-wat-aux (ast-x ast))
           (gen 'i32.const -1)
           (gen 'i32.mul)))

(defmethod gen-wat-aux ((ast const))
  (gen 'i32.const (const-value ast)))

(defun gen-wat (ast)
  `(module
    ,@(gen-wat-aux ast)
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
                            (asdf:system-relative-pathname :cc "wasm/"))
                    :output *standard-output*
                    :error-output *error-output*)
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
  (let ((ast (parse code)))
    (walk-ast-with-name-resolution ast) 
    (let ((wat (gen-wat ast)))
      (pprint wat)
      (terpri)
      (gen-html-file
       (wat-to-wasm wat)))))
