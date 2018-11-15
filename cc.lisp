(in-package :cc)

(defclass ast () ())

(defclass binary-operator (ast)
  ((op :initarg :op :reader ast-op)
   (x :initarg :x :reader ast-x)
   (y :initarg :y :reader ast-y)))

(defclass unary-operator (ast)
  ((op :initarg :op :reader ast-op)
   (x :initarg :x :reader ast-x)))

(define-lexer cc
  ("\\s+" :skip)
  ("$" nil)
  ("[a-zA-Z_][0-9a-zA-Z_]*"
   (values :word (text)))
  ("[0-9]+"
   (values :number
           (parse-integer (text))))
  ("."
   (let ((char (char (text) 0)))
     (values char char))))

(parsergen:defparser parse
  ((toplevel expression))
  (expression
   ((expression #\+ term)
    (make-instance 'binary-operator :op '+ :x $1 :y $3))
   ((expression #\- term)
    (make-instance 'binary-operator :op '- :x $1 :y $3))
   ((term)
    $1))
  (term
   ((term #\* factor)
    (make-instance 'binary-operator :op '* :x $1 :y $3))
   ((term #\/ factor)
    (make-instance 'binary-operator :op '/ :x $1 :y $3))
   ((factor)
    $1))
  (factor
   ((#\( expression #\))
    $2)
   ((#\+ :number)
    $2)
   ((#\- :number)
    (make-instance 'unary-operator :op '- :x $2))
   ((:number)
    $1)))

(defun gen-svm (ast)
  (let ((code '()))
    (labels ((gen-svm (ast)
               (trivia:match ast
                 ((binary-operator op x y)
                  (gen-svm x)
                  (gen-svm y)
                  (push `(,op) code))
                 ((unary-operator op x)
                  (gen-svm x)
                  (push `(,op) code))
                 (x
                  (push `(push ,x) code)))))
      (gen-svm ast)
      (nreverse code))))

(defun svm-to-wat (code)
  `(module
    (func $main (result i32)
          ,@(loop :for instr :in code
                  :collect (trivia:ematch instr
                             ((list 'push arg)
                              `(i32.const ,arg))
                             ((list '+)
                              `(i32.add))
                             ((list '-)
                              `(i32.sub))
                             ((list '*)
                              `(i32.mul))
                             ((list '/)
                              `(i32.div_s)))))
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
  (uiop:run-program "cd wasm; wat2wasm simple.wat -o simple.wasm")
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
  (let* ((expr (parse (let ((scanner (make-scanner 'cc code)))
                        (lambda ()
                          (lex scanner)))))
         (wat (svm-to-wat (gen-svm expr))))
    (gen-html-file
     (wat-to-wasm wat))))
