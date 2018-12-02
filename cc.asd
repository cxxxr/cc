(defsystem "cc"
  :depends-on ("yacc"
               "trivia"
               "cl-ppcre"
               "alexandria"
               "zenekindarl"
               "closer-mop")
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "ast")
               (:file "lexer")
               (:file "parser")
               (:file "compile1")
               (:file "cfg")
               (:file "compile-wat")
               (:file "cc")))
