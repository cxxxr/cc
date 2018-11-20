(in-package :cc)

(defstruct (scanner (:constructor %make-scanner))
  name
  input
  position)

(defun make-scanner (name input)
  (%make-scanner :name name :input input :position 0))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun gen-matcher-with-rules (rules)
    (alexandria:with-unique-names (scanner outer r-start r-end)
      `(lambda (,scanner)
         (block ,outer
           ,@(loop :for (regex . body) :in rules
                   :collect `(multiple-value-bind (,r-start ,r-end)
                                 (ppcre:scan ,regex (scanner-input ,scanner)
                                             :start (scanner-position ,scanner))
                               (when (eql ,r-start (scanner-position ,scanner))
                                 (setf (scanner-position ,scanner) ,r-end)
                                 ,(if (equal body '(:skip))
                                      nil
                                      `(macrolet ((text ()
                                                    `(subseq (scanner-input ,',scanner)
                                                             ,',r-start
                                                             ,',r-end)))
                                         (return-from ,outer (progn ,@body))))))))))))

(defmacro define-lexer (name &body rules)
  `(setf (get ',name 'matcher)
         ,(gen-matcher-with-rules rules)))

(defun lex (scanner)
  (funcall (get (scanner-name scanner) 'matcher) scanner))

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
  (">="
   (values :ge :ge))
  (">"
   (values :gt :gt))
  ("<="
   (values :le :le))
  ("<"
   (values :lt :lt))
  ("."
   (let ((char (char (text) 0)))
     (values char char))))
