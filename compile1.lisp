(in-package :cc)

(defvar *compile1-variables* '())

(defstruct fn
  name
  parameters
  code
  cfg)

(defun compile1-local-variable-p (name)
  (position name *compile1-variables* :test #'string= :key #'ident-name))

(defun compile1-gen-label (x)
  (gensym x))

(defun compile1-gen (op &rest args)
  (list (cons op args)))

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
     :parameters (loop :for () :in (func-parameters ast) :collect `(local i32))
     :code (resolve-label-names (compile1-aux (func-stat-block ast) nil)))))

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
