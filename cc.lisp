(in-package :cc)

(defun comp (x)
  (write-line ".intel_syntax noprefix")
  (write-line ".global _main")
  (write-line "_main:")
  (format t "mov rax, ~D~%" x)
  (write-line "ret"))

(defun test ()
  (let ((value 42))
    (with-open-file (*standard-output* "test.s"
                                       :direction :output
                                       :if-does-not-exist :create
                                       :if-exists :supersede)
      (comp value))
    (uiop:run-program "gcc test.s"
                      :output *standard-output*
                      :error-output *error-output*)
    (= value (nth-value 2 (uiop:run-program "./a.out"
                                            :output *standard-output*
                                            :error-output *error-output*
                                            :ignore-error-status t)))))
