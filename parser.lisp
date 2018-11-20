(in-package :cc)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun accept-parsed-program (functions)
    (make-instance 'program :functions functions)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun accept-parsed-function (name parameters { stats })
    (declare (ignore { }))
    (make-instance 'func
                   :name name
                   :parameters (mapcar #'make-ident parameters)
                   :statements stats)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun accept-parsed-parameters (paren parameters)
    (declare (ignore paren))
    (remove-if (lambda (s) (member s '(#\) #\,)))
               (alexandria:flatten parameters))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun concat-stats (first rest)
    (cons first rest)))

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
                     (:ne 'binop-ne)
                     (:lt 'binop-lt)
                     (:le 'binop-le)
                     (:gt 'binop-gt)
                     (:ge 'binop-ge))
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
  (defun accept-parsed-stat-expression (expr \;)
    (declare (ignore \;))
    expr))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun accept-parsed-call-function (name arguments)
    (make-instance 'call-function :name name :arguments arguments)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun accept-parsed-arguments (paren arguments)
    (declare (ignore paren))
    (remove-if (lambda (x) (member x '(#\) #\,)))
               (alexandria:flatten arguments))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-ident (name)
    (make-instance 'ident :name name)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-const (x)
    (make-instance 'const :value x)))

(yacc:define-parser *parser*
  (:start-symbol program)
  (:terminals (#\+ #\- #\* #\/ #\( #\) #\= #\; #\% #\{ #\} #\, :number :word :eq :ne
                   :ge :gt :le :lt))
  (:precedence ((:left #\* #\/ #\%)
                (:left #\+ #\-)
                (:left :lt :le :gt :ge)
                (:left :eq :ne)
                (:right #\=)))
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
   (stat stats #'concat-stats))
  (stat
   (expression #\; #'accept-parsed-stat-expression))
  (expression
   (expression #\= expression #'accept-parsed-binary-expression)
   (expression #\+ expression #'accept-parsed-binary-expression)
   (expression #\- expression #'accept-parsed-binary-expression)
   (expression #\* expression #'accept-parsed-binary-expression)
   (expression #\/ expression #'accept-parsed-binary-expression)
   (expression #\% expression #'accept-parsed-binary-expression)
   (expression :eq expression #'accept-parsed-binary-expression)
   (expression :ne expression #'accept-parsed-binary-expression)
   (expression :ge expression #'accept-parsed-binary-expression)
   (expression :gt expression #'accept-parsed-binary-expression)
   (expression :le expression #'accept-parsed-binary-expression)
   (expression :lt expression #'accept-parsed-binary-expression)
   call-function
   term)
  (call-function
   (:word arguments #'accept-parsed-call-function))
  (arguments
   (#\( arguments* #'accept-parsed-arguments))
  (arguments*
   (expression arguments** #'list)
   (#\) #'list))
  (arguments**
   (#\, expression arguments** #'list)
   (#\) #'list))
  (term
   (:number #'make-const)
   (:word #'make-ident)
   (#\+ expression #'accept-parsed-unary-expression)
   (#\- expression #'accept-parsed-unary-expression)
   (#\( expression #\) #'accept-parsed-paren-expression)))

(defun set-ident-env (ident env)
  (setf (ident-num ident)
        (or (gethash (ident-name ident) env)
            (setf (gethash (ident-name ident) env)
                  (hash-table-count env)))))

(defun walk-ast-with-name-resolution (ast)
  (let (env)
    (declare (special env))
    (walk-ast ast
              (lambda (ast cont)
                (trivia:match ast
                  ((func parameters)
                   (let ((env
                           (make-hash-table :test 'equal)))
                     (declare (special env))
                     (dolist (ident parameters)
                       (set-ident-env ident env))
                     (funcall cont)
                     (setf (func-local-idents ast)
                           (mapcar #'make-ident (alexandria:hash-table-keys env)))))
                  ((ident)
                   (set-ident-env ast env)
                   (funcall cont))
                  (_
                   (funcall cont)))))))

(defun parse (code)
  (let ((ast (yacc:parse-with-lexer
              (let ((scanner (make-scanner 'cc code)))
                (lambda ()
                  (lex scanner)))
              *parser*)))
    (walk-ast-with-name-resolution ast)
    ast))
