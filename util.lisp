(in-package :cc)

(defmacro push-end (x list)
  `(setf ,list (append ,list (list ,x))))