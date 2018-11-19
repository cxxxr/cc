(defsystem "cc"
  :depends-on ("prove"
               "yacc"
               "trivia"
               "cl-ppcre"
               "alexandria"
               "zenekindarl")
  :serial t
  :components ((:file "package")
               (:file "ast")
               (:file "lexer")
               (:file "parser")
               (:file "compile")
               (:file "cc")))
