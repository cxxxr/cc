(in-package :cc)

(defun signature-name (str)
  (intern (format nil "$~A" (string-upcase str))))

(defun gen-seq (&rest args)
  (apply #'append args))

(defun gen (op &rest args)
  (list (cons op args)))

(defgeneric gen-wat-aux (ast))

(defmethod gen-wat-aux ((ast program))
  (mapcar #'gen-wat-aux (program-functions ast)))

(defmethod gen-wat-aux ((ast func))
  (let ((name (func-name ast))
        (parameters (func-parameters ast))
        (local-idents (func-local-idents ast))
        (stat-block (func-stat-block ast)))
    `(func ,(signature-name name)
           ,@(loop :repeat (length parameters) :collect '(param i32))
           (result i32)
           ,@(loop :repeat (length local-idents) :collect '(local i32))
           ,@(gen-wat-aux stat-block)
           ,@(gen 'i32.const 0)
           ,@(gen 'return))))

(defmethod gen-wat-aux ((ast stat-block))
  (let ((statements (stat-block-statements ast)))
    (loop :for statement :in statements
          :append (gen-wat-aux statement)
          :append (gen 'drop))))

(defmethod gen-wat-aux ((ast stat-if))
  (let* ((then (stat-if-then ast))
         (else (stat-if-else ast))
         (then-code (gen-wat-aux then))
         (else-code (if else
                        (gen-wat-aux else)
                        (gen 'i32.const 0))))
    (gen-seq (gen-wat-aux (stat-if-test ast))
             `((if (result i32) (i32.eqz)
                   (then
                    ,@else-code)
                   (else
                    ,@then-code))))))

(defmethod gen-wat-aux ((ast stat-return))
  (gen-seq (gen-wat-aux (stat-return-expr ast))
           (gen 'return)))

(defmethod gen-wat-aux ((ast ident))
  (let ((num (ident-num ast)))
    (gen 'get_local num)))

(defmethod gen-wat-aux ((ast binop-assign))
  (let ((x (ast-x ast))
        (y (ast-y ast)))
    (trivia:ematch x
      ((ident num)
       (gen-seq (gen-wat-aux y)
                (gen 'tee_local num))))))

(defmethod gen-wat-aux ((ast binop))
  (gen-seq (gen-wat-aux (ast-x ast))
           (gen-wat-aux (ast-y ast))
           (gen (etypecase ast
                  (binop-add 'i32.add)
                  (binop-sub 'i32.sub)
                  (binop-mul 'i32.mul)
                  (binop-div 'i32.div)
                  (binop-rem 'i32.rem_s)
                  (binop-eq 'i32.eq)
                  (binop-ne 'i32.ne)
                  (binop-lt 'i32.lt_s)
                  (binop-le 'i32.le_s)
                  (binop-gt 'i32.gt_s)
                  (binop-ge 'i32.ge_s)))))

(defmethod gen-wat-aux ((ast unop-negate))
  (gen-seq (gen-wat-aux (ast-x ast))
           (gen 'i32.const -1)
           (gen 'i32.mul)))

(defmethod gen-wat-aux ((ast const))
  (gen 'i32.const (const-value ast)))

(defmethod gen-wat-aux ((ast call-function))
  (gen-seq (mapcan #'gen-wat-aux (call-function-arguments ast))
           (gen 'call (signature-name (call-function-name ast)))))

(defun gen-wat (ast)
  `(module
    ,@(gen-wat-aux ast)
    (export "main" (func $main))))

(defun print-wat (wat)
  (let ((*print-case* :downcase))
    (pprint wat)))

(defun wat-to-wasm (wat)
  (with-open-file (*standard-output* "wasm/simple.wat"
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
    (print-wat wat))
  (uiop:run-program (format nil "cd ~A; wat2wasm simple.wat -o simple.wasm"
                            (asdf:system-relative-pathname :cc "wasm/"))
                    :output *standard-output*
                    :error-output *error-output*)
  (alexandria:read-file-into-byte-vector "wasm/simple.wasm"))

(defun octets-to-js-array (octets)
  (format nil "[~{~D~^,~}]" (coerce octets 'list)))

(defun gen-html-file (wasm-octets)
  (with-open-file (*standard-output* "./index.html"
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
    (zen:render-file "./template.html" :wasm (octets-to-js-array wasm-octets))))
