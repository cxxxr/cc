(in-package :cc)

(defvar *indent* 0)

(defmacro with-assemble (() &body body)
  (let ((indent (gensym "INDENT")))
    `(labels ((do-indent (&optional (n 1))
                (incf *indent* (* n 4)))
              (,indent ()
                (loop :repeat *indent* :do (write-char #\space)))
              (mov (x y)
                (,indent)
                (format t "mov ~A, ~A~%" x y))
              (add (x y)
                (,indent)
                (format t "add ~A, ~A~%" x y))
              (sub (x y)
                (,indent)
                (format t "sub ~A, ~A~%" x y))
              (ret ()
                (,indent)
                (write-line "ret")))
       (let ((*indent* 0))
         ,@body))))

(define-lexer cc
  ("\\s+" :skip)
  ("$" (values :eof :eof))
  ("[a-zA-Z_][0-9a-zA-Z_]*"
   (values (text) :word))
  ("[0-9]+"
   (values (parse-integer (text))
           :number))
  ("."
   (let ((char (char (text) 0)))
     (values char char))))

(defun comp (code)
  (write-line ".intel_syntax noprefix")
  (write-line ".global _main")
  (write-line "_main:")
  (with-assemble ()
    (do-indent 1)
    (let ((scanner (make-scanner 'cc code)))
      (multiple-value-bind (value kind) (lex scanner)
        (case kind
          ((#\+)
           (multiple-value-bind (value kind) (lex scanner)
             (assert (eq kind :number))
             (mov "rax" value)))
          ((#\-)
           (multiple-value-bind (value kind) (lex scanner)
             (assert (eq kind :number))
             (mov "rax" (- value))))
          (otherwise
           (assert (eq kind :number))
           (mov "rax" value))))
      (loop
       (multiple-value-bind (value kind) (lex scanner)
         (declare (ignore value))
         (when (eq kind :eof) (return))
         (assert (member kind '(#\+ #\-)))
         (multiple-value-bind (value2 kind2) (lex scanner)
           (assert (eq kind2 :number))
           (ecase kind
             (#\+ (add "rax" value2))
             (#\- (sub "rax" value2))))))
      (ret)
      (do-indent -1))))

(defun comp-test (input value)
  (let ((directory (asdf:system-source-directory :cc)))
    (uiop:with-temporary-file (:stream stream
                               :pathname pathname
                               :type "s"
                               :directory directory)
      (let ((*standard-output* stream))
        (comp input))
      :close-stream
      (prove:is
       (nth-value 2 (uiop:run-program
                     (format nil "cd ~A; gcc ~A; ./a.out"
                             directory
                             (enough-namestring pathname directory))
                     :output *standard-output*
                     :error-output *error-output*
                     :ignore-error-status t))
       value))))
    
(defun test ()
  (let (#+lispworks (prove.color:*enable-colors* nil))
    (prove:plan nil)
    (comp-test "42" 42)
    (comp-test "5 + 20 - 4" 21)
    (comp-test "-1" 255)
    (comp-test "-5+   10" 5)
    (comp-test "+3 - 1" 2)
    (prove:finalize)))
