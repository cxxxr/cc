(in-package :cc)

(defvar *local-variables*)

(defun %signature-name (x)
  (intern (format nil "$~:@(~A~)" x)))

(defun signature-name (x)
  (%signature-name x))

(defun unique-signature-name ()
  (%signature-name (gensym)))

(defun block-signature-name (node-num)
  (%signature-name (format nil "_BLOCK_~D" node-num)))

(defun wat-goto (switch-selector-var switch-label selector-value)
  `((I32.CONST ,selector-value)
    (SET_LOCAL ,switch-selector-var)
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
    (pushnew switch-selector-var *local-variables*)
    (labels ((block-or-loop (prev-label)
               (if (eq switch-label prev-label)
                   `(LOOP ,prev-label (RESULT i32))
                   `(BLOCK ,prev-label)))
             (next (nodes)
               (if (null (rest nodes))
                   `((RETURN (i32.const 0)))
                   (let ((next (cfg-node-name (second nodes))))
                     (wat-goto switch-selector-var switch-label next))))
             (f (nodes prev-label)
               (if (null nodes)
                   `(,@(block-or-loop prev-label)
                     (BR_TABLE ,@(mapcar #'block-signature-name (mapcar #'cfg-node-name cfg-nodes))
                               ,(block-signature-name (cfg-node-name (alexandria:lastcar cfg-nodes)))
                               (GET_LOCAL ,switch-selector-var)))
                   (let ((cfg-node (first nodes)))
                     `(,@(block-or-loop prev-label)
                       ,(f (rest nodes) (block-signature-name (cfg-node-name cfg-node)))
                       ,@(wat-code (cfg-node-code cfg-node) switch-selector-var switch-label)
                       ,@(next nodes))))))
      (f cfg-nodes switch-label))))

(defun compile-wat-fn (fn)
  (let* ((*local-variables* '())
         (body (wat-switch (cfg-nodes (fn-code fn)))))
    `(func ,(signature-name (fn-name fn))
           ,@(fn-parameters fn)
           (RESULT i32)
           ,@(loop :for name :in *local-variables* :collect `(LOCAL ,name i32))
           ,body)))

(defun compile-wat (fn-list)
  `(module
    (import "console" "log" (func $log (param i32) (result i32)))
    ,@(mapcar #'compile-wat-fn fn-list)
    (export "main" (func $main))))
