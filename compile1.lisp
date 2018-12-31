(in-package :cc)

(defvar *compile1-variables* '())

(defclass instr ()
  ((opcode :initarg :opcode :reader instr-opcode)
   (arg1 :initarg :arg1 :accessor instr-arg1)))

(defclass instr-label (instr) ())
(defclass instr-jump (instr) ())
(defclass instr-tjump (instr-jump) ())

(defmethod print-object ((instr instr) stream)
  (print-unreadable-object (instr stream)
    (if (slot-boundp instr 'arg1)
        (format stream "~A ~A" (instr-opcode instr) (instr-arg1 instr))
        (princ (instr-opcode instr) stream))))

(defstruct fn
  name
  parameters
  code
  return-type)

(defun compile1-local-variable-p (name)
  (position name *compile1-variables* :test #'string= :key #'ident-name))

(defun compile1-gen-label (x)
  (gensym x))

(defun compile1-gen (op &optional (arg1 nil arg1-p))
  (list (apply #'make-instance
               (case op
                 (LABEL 'instr-label)
                 (JUMP 'instr-jump)
                 (TJUMP 'instr-tjump)
                 (otherwise 'instr))
               :opcode op
               (if arg1-p (list :arg1 arg1)))))

(defun compile1-genseq (&rest args)
  (apply #'append args))

(defgeneric compile1-aux (ast return-value-p))

(defmethod compile1-aux ((ast program) return-value-p)
  (loop :for func :in (program-functions ast)
        :collect (compile1-aux func nil)))

(defmethod compile1-aux ((ast func) return-value-p)
  (let ((*compile1-variables* (func-parameters ast)))
    (make-fn
     :name (func-name ast)
     :parameters (loop :for () :in (func-parameters ast) :collect `(param i32))
     :code (check-label-names (compile1-aux (func-stat-block ast) nil))
     :return-type (func-return-type ast))))

(defmethod compile1-aux ((ast stat-block) return-value-p)
  (loop :for statement :in (stat-block-statements ast)
        :append (compile1-aux statement nil)))

(defmethod compile1-aux ((ast stat-if) return-value-p)
  (let ((then-label (compile1-gen-label "then"))
        (end-label (compile1-gen-label "else")))
    (compile1-genseq (compile1-aux (stat-if-test ast) t)
                     (compile1-gen 'TJUMP then-label)
                     (when (stat-if-else ast) (compile1-aux (stat-if-else ast) nil))
                     (compile1-gen 'JUMP end-label)
                     (compile1-gen 'LABEL then-label)
                     (compile1-aux (stat-if-then ast) nil)
                     (compile1-gen 'LABEL end-label))))

(defmethod compile1-aux ((ast stat-return) return-value-p)
  (compile1-genseq (if (stat-return-expr ast)
                       (compile1-aux (stat-return-expr ast) t))
                   (compile1-gen 'RETURN)))

(defmethod compile1-aux ((ast stat-label) return-value-p)
  (compile1-gen 'LABEL (stat-label-name ast)))

(defmethod compile1-aux ((ast stat-goto) return-value-p)
  (compile1-gen 'JUMP (stat-goto-name ast)))

(defmethod compile1-aux ((ast binop) return-value-p)
  (compile1-genseq (compile1-aux (binop-x ast) t)
                   (compile1-aux (binop-y ast) t)
                   (compile1-gen (etypecase ast
                                   (binop-add 'I32.ADD)
                                   (binop-sub 'I32.SUB)
                                   (binop-mul 'I32.MUL)
                                   (binop-div 'I32.DIV)
                                   (binop-rem 'I32.REM_S)
                                   (binop-eq 'I32.EQ)
                                   (binop-ne 'I32.NE)
                                   (binop-lt 'I32.LT_S)
                                   (binop-le 'I32.LE_S)
                                   (binop-gt 'I32.GT_S)
                                   (binop-ge 'I32.GE_S)))
                   (unless return-value-p
                     (compile1-gen 'DROP))))

(defmethod compile1-aux ((ast binop-assign) return-value-p)
  (trivia:ematch (binop-x ast)
    ((ident name)
     (let ((i (compile1-local-variable-p name)))
       (compile1-genseq (compile1-aux (binop-y ast) t)
                        (if i
                            (compile1-gen (if return-value-p
                                              'TEE_LOCAL
                                              'SET_LOCAL)
                                          i)
                            ;; TODO: nmaeの指定
                            (compile1-gen (if return-value-p
                                              'TEE_GLOBAL
                                              'SET_GLOBAL)
                                          )))))))

(defmethod compile1-aux ((ast unop-negate) return-value-p)
  (compile1-genseq (compile1-aux (unop-x ast) t)
                   (compile1-gen 'I32.CONST -1)
                   (compile1-gen 'I32.MUL)))

(defmethod compile1-aux ((ast ident) return-value-p)
  (when return-value-p
    (let ((i (compile1-local-variable-p (ident-name ast))))
      (compile1-genseq (if i
                           (compile1-gen 'GET_LOCAL i)
                           ;; TODO: nameの指定
                           (compile1-gen 'GET_GLOBAL))))))

(defmethod compile1-aux ((ast const) return-value-p)
  (when return-value-p
    (compile1-gen 'I32.CONST (const-value ast))))

(defmethod compile1-aux ((ast call-function) return-value-p)
  (compile1-genseq (loop :for a :in (call-function-arguments ast)
                         :append (compile1-aux a t))
                   (compile1-gen 'CALL (call-function-name ast))
                   (unless return-value-p
                     (compile1-gen 'DROP))))

(defun check-label-names (code)
  (let ((label-names '()))
    (flet ((find-label (name) (find name label-names :test #'equal)))
      (dolist (instr code)
        (trivia:match instr
          ((instr-label arg1)
           (when (find-label arg1)
             (error "ラベル名が重複しています: ~A" arg1))
           (push-end arg1 label-names))))
      (loop :for instr :in code
            :do (trivia:match instr
                  ((instr-jump arg1)
                   (unless (find-label arg1) (error "ラベルが存在しません: ~A" arg1)))
                  (_
                   instr)))))
  code)

(defun compile1 (ast)
  (compile1-aux ast nil))
