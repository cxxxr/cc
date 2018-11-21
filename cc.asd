(defsystem "cc"
  :depends-on ("prove"
               "yacc"
               "trivia"
               "cl-ppcre"
               "alexandria"
               "zenekindarl"
               "closer-mop")
  :serial t
  :components ((:file "package")
               (:file "ast")
               (:file "lexer")
               (:file "parser")
               (:file "compile")
               (:file "cc")))
