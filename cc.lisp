(in-package :cc)

(defun comp (code)
  (let* ((ast (parse code))
         (wat (gen-wat ast)))
    (pprint wat)
    (terpri)
    (gen-html-file
     (wat-to-wasm wat))))
