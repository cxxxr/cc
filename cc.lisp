(in-package :cc)

#|
(defun comp (code)
  (let* ((ast (parse code))
         (wat (gen-wat ast)))
    (pprint wat)
    (terpri)
    (gen-html-file
     (wat-to-wasm wat))))

(defun comp-file (file)
  (comp (uiop:read-file-string file)))
|#

(defun comp (code)
  (cfg (compile1 (parse code))))

(defun comp-file (file)
  (comp (uiop:read-file-string file)))
