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

(defclass stat-if (ast)
  ((test
    :initarg :test
    :reader stat-if-test)
   (then
    :initarg :then
    :reader stat-if-then)
   (else
    :initarg :else
    :reader stat-if-else)))

(defclass stat-return (ast)
  ((expr
    :initform nil
    :initarg :expr
    :reader stat-return-expr)))

(defclass stat-label (ast)
  ((name
    :initarg :name
    :reader stat-label-name)))

(defclass stat-goto (ast)
  ((name
    :initarg :name
    :reader stat-goto-name)))

(defclass binop (ast)
  ((x :initarg :x :reader binop-x)
   (y :initarg :y :reader binop-y)))

(defclass binop-add (binop) ())
(defclass binop-sub (binop) ())
(defclass binop-mul (binop) ())
(defclass binop-div (binop) ())
(defclass binop-rem (binop) ())
(defclass binop-assign (binop) ())
(defclass binop-eq (binop) ())
(defclass binop-ne (binop) ())
(defclass binop-lt (binop) ())
(defclass binop-le (binop) ())
(defclass binop-gt (binop) ())
(defclass binop-ge (binop) ())

(defclass unary-operator (ast)
  ((x :initarg :x :reader binop-x)))

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
    (loop :for slot :in (c2mop:class-slots (class-of ast))
          :for name := (c2mop:slot-definition-name slot)
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
               ((stat-if test then else)
                (walk-ast test function)
                (walk-ast then function)
                (walk-ast else function))
               ((stat-return expr)
                (walk-ast expr function))
               ((binop x y)
                (walk-ast x function)
                (walk-ast y function))
               ((unary-operator x)
                (walk-ast x function))
               ((call-function arguments)
                (dolist (arg arguments)
                  (walk-ast arg function)))))))
