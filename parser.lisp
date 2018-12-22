(in-package :cc)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun accept-parsed-program (functions)
    (make-instance 'program :functions functions)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun accept-parsed-function (&rest args)
    (trivia:match args
      ((list name parameters stat-block)
       (make-instance 'func
                      :name name
                      :parameters (mapcar #'make-ident parameters)
                      :stat-block stat-block))
      ((list type name parameters stat-block)
       (make-instance 'func
                      :name name
                      :parameters (mapcar #'make-ident parameters)
                      :stat-block stat-block
                      :return-type type)))))

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
  (defun accept-parsed-stat-block ({ stats })
    (declare (ignore { }))
    (make-instance 'stat-block :statements stats)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun accept-parsed-stat-expression (expr \;)
    (declare (ignore \;))
    expr))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun accept-parsed-if (_if \( test \) then else)
    (declare (ignore _if \( \) \{ \}))
    (make-instance 'stat-if :test test :then then :else else)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun accept-parsed-else (_else stat)
    (declare (ignore _else))
    stat))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun accept-parsed-return (&rest args)
    (declare (ignore _return))
    (trivia:ematch args
      ((list _ expr #\;)
       (make-instance 'stat-return :expr expr))
      ((list _ #\;)
       (make-instance 'stat-return :expr nil)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun accept-parsed-goto (goto name delim)
    (declare (ignore goto delim))
    (make-instance 'stat-goto :name name)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun accept-parsed-label (name colon)
    (declare (ignore colon))
    (make-instance 'stat-label :name name)))

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
  (:terminals (#\+ #\- #\* #\/ #\( #\) #\= #\; #\% #\{ #\} #\, #\: :number :word :eq :ne
                   :ge :gt :le :lt :if :else :return :goto :int))
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
   (:word parameters stat-block #'accept-parsed-function)
   (:int :word parameters stat-block #'accept-parsed-function))
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
   stat-block
   stat-if
   stat-return
   stat-goto
   stat-label
   (expression #\; #'accept-parsed-stat-expression))
  (stat-if
   (:if #\( expression #\) stat stat-else #'accept-parsed-if))
  (stat-else
   (:else stat #'accept-parsed-else)
   ())
  (stat-block
   (#\{ stats #\} #'accept-parsed-stat-block))
  (stat-return
   (:return expression #\; #'accept-parsed-return)
   (:return #\; #'accept-parsed-return))
  (stat-goto
   (:goto :word #\; #'accept-parsed-goto))
  (stat-label
   (:word #\: #'accept-parsed-label))
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

(defun parse (code)
  (let ((ast (yacc:parse-with-lexer
              (let ((scanner (make-scanner 'cc code)))
                (lambda ()
                  (lex scanner)))
              *parser*)))
    ast))
