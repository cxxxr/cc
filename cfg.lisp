(in-package :cc)

(defstruct cfg-node name code)

(defun cfg (compile1-function)
  (pprint compile1-function)
  (let* ((current-node (make-cfg-node :name 0 :code '()))
         (nodes '())
         (tr-table))
    (dolist (c (compile1-function-code compile1-function))
      (alexandria:destructuring-case c
        ((LABEL name)
         (push-end current-node nodes)
         (setf current-node (make-cfg-node :name name :code '())))
        (((JUMP TJUMP) name)
         (push-end (cons (cfg-node-name current-node) name) tr-table)
         (push-end c (cfg-node-code current-node)))
        ((t &rest rest)
         (declare (ignore rest))
         (push-end c (cfg-node-code current-node)))))
    (push-end current-node nodes)
    (values nodes
            tr-table)))
