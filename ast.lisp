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
   (stat-block
    :initarg :stat-block
    :reader func-stat-block)))

(defclass stat-block (ast)
  ((statements
    :initarg :statements
    :reader stat-block-statements)))

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
(defclass binop-lt (binary-operator) ())
(defclass binop-le (binary-operator) ())
(defclass binop-gt (binary-operator) ())
(defclass binop-ge (binary-operator) ())

(defclass unary-operator (ast)
  ((op :initarg :op :reader ast-op)
   (x :initarg :x :reader ast-x)))

(defclass unop-negate (unary-operator) ())

(defclass ident (ast)
  ((name :initarg :name :reader ident-name)
   (num :initarg :num :accessor ident-num)))

(defclass const (ast)
  ((value :initarg :value :reader const-value)))

(defclass call-function (ast)
  ((name :initarg :name :reader call-function-name)
   (arguments :initarg :arguments :reader call-function-arguments)))

(defvar *indent-level* 0)

(defun print-indent (stream)
  (loop :repeat *indent-level* :do (write-string "  " stream)))

(defmethod print-object ((ast ast) stream)
  (fresh-line stream)
  (print-indent stream)
  (format stream "~A {~%" (type-of ast))
  (let ((*indent-level* (1+ *indent-level*)))
    (loop :for slot :in (clos:class-slots (class-of ast))
          :for name := (clos:slot-definition-name slot)
          :for value := (slot-value ast name)
          :when (slot-boundp ast name)
          :do (print-indent stream)
              (cond ((consp value)
                     (format stream "~A: (~%" name)
                     (let ((*indent-level* (1+ *indent-level*)))
                       (dolist (x value) (prin1 x stream) (terpri stream)))
                     (print-indent stream)
                     (format stream ")~%" name))
                    (t
                     (format stream "~A: ~S~%" name value)))))
  (print-indent stream)
  (write-char #\} stream))

(defun walk-ast (ast function)
  (funcall function ast
           (lambda ()
             (trivia:match ast
               ((program functions)
                (dolist (f functions)
                  (walk-ast f function)))
               ((func stat-block)
                (walk-ast stat-block function))
               ((stat-block statements)
                (dolist (stat statements)
                  (walk-ast stat function)))
               ((binary-operator x y)
                (walk-ast x function)
                (walk-ast y function))
               ((unary-operator x)
                (walk-ast x function))
               ((call-function arguments)
                (dolist (arg arguments)
                  (walk-ast arg function)))))))
