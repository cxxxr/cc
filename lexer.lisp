(in-package :cc)

(defstruct (scanner (:constructor %make-lexer))
  name
  input
  position)

(defun make-lexer (name input)
  (%make-lexer :name name :input input :position 0))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun gen-matcher-with-rules (rules)
  (with-unique-names (scanner outer r-start r-end text)
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
                                    `(let ((,text (subseq (scanner-input ,scanner)
                                                          ,r-start
                                                          ,r-end)))
                                       (flet ((text () ,text))
                                         (return-from ,outer (progn ,@body))))))))))))
)

(defmacro define-lexer (name &body rules)
  `(setf (get ',name 'matcher)
         ,(gen-matcher-with-rules rules)))

(defun lex (scanner)
  (funcall (get (scanner-name scanner) 'matcher) scanner))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-lexer cc
  ("\\s+" :skip)
  ("[a-zA-Z_][0-9a-zA-Z_]*"
   (values (text) :word))
  ("[0-9]+"
   (values (parse-integer (text))
           :number))
  ("."
   (let ((char (char (text) 0)))
     (values char char))))
