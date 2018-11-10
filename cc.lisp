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

(defun comp (args)
  (write-line ".intel_syntax noprefix")
  (write-line ".global _main")
  (write-line "_main:")
  (unless (consp args) (setf args (list args)))
  (with-assemble ()
    (do-indent 1)
    (case (first args)
      ((+)
       (pop args)
       (mov "rax" (pop args)))
      ((-)
       (pop args)
       (mov "rax" (- (pop args))))
      (otherwise
       (let ((x (pop args)))
         (assert (integerp x))
         (mov "rax" x))))
    (loop :for (op arg) :on args :by #'cddr
          :do
          (assert (integerp arg))
          (ecase op
            (+ (add "rax" arg))
            (- (sub "rax" arg))))
    (ret)
    (do-indent -1))
  (values))

(defun comp-test (form value)
  (let ((directory (asdf:system-source-directory :cc)))
    (uiop:with-temporary-file (:stream stream
                               :pathname pathname
                               :type "s"
                               :directory directory)
      (let ((*standard-output* stream))
        (comp form))
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
    (comp-test 42 42)
    (comp-test '(5 + 20 - 4) 21)
    (comp-test '(- 1) 255)
    (comp-test '(- 5 + 10) 5)
    (comp-test '(+ 3 - 1) 2)
    (prove:finalize)))
