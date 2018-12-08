(in-package :cc)

(defun signature-name (x)
  (intern (format nil "$~:@(~A~)" x)))

(defun unique-signature-name ()
  (signature-name (gensym)))

(defun wat-goto (switch-selector-var switch-label selector-value)
  `((SET_LOCAL ,switch-selector-var ,selector-value)
    (BR ,switch-label)))

(defun wat-code (code switch-selector-var switch-label)
  (loop :for instr :in code
        :append (trivia:match instr
                  ((instr-jump arg1)
                   (wat-goto switch-selector-var switch-label arg1))
                  ((instr :opcode 'CALL arg1)
                   `((CALL ,(signature-name arg1))))
                  (_
                   (if (slot-boundp instr 'arg1)
                       `((,(instr-opcode instr) ,(instr-arg1 instr)))
                       `((,(instr-opcode instr))))))))

(defun wat-switch (cfg-nodes)
  (let ((switch-selector-var (unique-signature-name))
        (switch-label (unique-signature-name)))
    (labels ((block-or-loop (prev-label)
               (if (eq switch-label prev-label)
                   'LOOP
                   'BLOCK))
             (next (nodes)
               (if (null (rest nodes))
                   `(RETURN)
                   (let ((next (cfg-node-name (second nodes))))
                     (wat-goto switch-selector-var switch-label next))))
             (f (nodes prev-label)
               (if (null nodes)
                   `(,(block-or-loop prev-label)
                     ,prev-label
                     (BR_TABLE ,@(mapcar #'cfg-node-name cfg-nodes)
                               ,(cfg-node-name (alexandria:lastcar cfg-nodes))
                               (GET_LOCAL ,switch-selector-var)))
                   (let ((cfg-node (first nodes)))
                     `(,(block-or-loop prev-label)
                       ,prev-label
                       ,(f (rest nodes) (signature-name (cfg-node-name cfg-node)))
                       ,@(wat-code (cfg-node-code cfg-node) switch-selector-var switch-label)
                       ,@(next nodes))))))
      (f cfg-nodes switch-label))))

(defun compile-wat-fn (fn)
  `(func ,(signature-name (fn-name fn))
         ,(fn-parameters fn)
         (result i32)
         ,(wat-switch (cfg-nodes (fn-code fn)))))

(defun compile-wat (fn-list)
  (dolist (fn fn-list)
    (pprint (compile-wat-fn fn))))
