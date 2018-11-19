(in-package :cc)

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

(defun parse (code)
  (let ((ast (yacc:parse-with-lexer
              (let ((scanner (make-scanner 'cc code)))
                (lambda ()
                  (lex scanner)))
              *parser*)))
    (walk-ast-with-name-resolution ast)
    ast))
