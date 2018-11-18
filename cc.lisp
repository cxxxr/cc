(in-package :cc)

(defvar *variable-environment*)

(defclass ast () ())

(defclass program (ast)
  ((statements
    :initarg :statements
    :reader program-statements)))

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

(defclass unary-operator (ast)
  ((op :initarg :op :reader ast-op)
   (x :initarg :x :reader ast-x)))

(defclass unop-negate (unary-operator) ())

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
  ("=="
   (values :eq :eq))
  ("."
   (let ((char (char (text) 0)))
     (values char char))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun accept-parsed-binary-expression (a b c)
  (make-instance (ecase b
                   (#\= 'binop-assign)
                   (#\+ 'binop-add)
                   (#\- 'binop-sub)
                   (#\* 'binop-mul)
                   (#\/ 'binop-div)
                   (#\% 'binop-rem)
                   (:eq 'binop-eq))
                 :x a
                 :y c))

(defun accept-parsed-unary-expression (a b)
  (ecase a
    (#\+ b)
    (#\- (make-instance 'unop-negate :x b))))

(defun accept-parsed-paren-expression (a b c)
  (declare (ignore a c))
  b)

(defun make-ident (name)
  (make-instance 'ident :name name))

(defun concat-stats (a b c)
  (declare (ignore b))
  (cons a c))

(defun accept-parsed-program (stats)
  (make-instance 'program :statements stats))
)

(yacc:define-parser *parser*
  (:start-symbol program)
  (:terminals (#\+ #\- #\* #\/ #\( #\) #\= #\; #\% :number :word :eq))
  (:precedence ((:left #\* #\/ #\%) (:left #\+ #\-) (:left :eq) (:right #\=)))
  (program
   (stats #'accept-parsed-program))
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
   term)
  (term
   :number
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

(defun gen-wat-1 (ast)
  (let ((code '()))
    (labels ((gen (x)
               (push x code))
             (gen-binop (x y op)
               (gen-rec x)
               (gen-rec y)
               (gen `(,op)))
             (gen-rec (ast)
               (trivia:match ast
                 ((program statements)
                  (loop :for statement* :on statements
                        :for statement := (first statement*)
                        :do (gen-rec statement)
                            (when (rest statement*)
                              (gen '(drop)))))
                 ((ident num)
                  (gen `(get_local ,num)))
                 ((binop-assign x y)
                  (trivia:ematch x
                    ((ident num)
                     (gen-rec y)
                     (gen `(tee_local ,num)))))
                 ((binop-add x y)
                  (gen-binop x y 'i32.add))
                 ((binop-sub x y)
                  (gen-binop x y 'i32.sub))
                 ((binop-mul x y)
                  (gen-binop x y 'i32.mul))
                 ((binop-div x y)
                  (gen-binop x y 'i32.div))
                 ((binop-rem x y)
                  (gen-binop x y 'i32.rem_s))
                 ((binop-eq x y)
                  (gen-binop x y 'i32.eq))
                 ((unop-negate x)
                  (gen-rec x)
                  (gen '(i32.const -1))
                  (gen '(i32.mul)))
                 ((satisfies integerp)
                  (gen `(i32.const ,ast))))))
      (gen-rec ast)
      (nreverse code))))

(defun gen-wat (ast)
  `(module
    (func $main (result i32)
          ,@(loop :repeat (hash-table-count *variable-environment*) :collect `(local i32))
          ,@(gen-wat-1 ast))
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
  (let ((*variable-environment* (make-hash-table :test 'equal)))
    (let* ((ast (parse code))
           (wat (gen-wat ast)))
      (pprint wat)
      (gen-html-file
       (wat-to-wasm wat)))))
