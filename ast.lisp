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
