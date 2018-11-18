(defsystem "cc"
  :depends-on ("prove"
               "yacc"
               "trivia"
               "cl-ppcre"
               "alexandria"
               "zenekindarl")
  :serial t
  :components ((:file "package")
               (:file "lexer")
               (:file "cc")))
