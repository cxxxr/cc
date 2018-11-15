(require "parsergen")

(defsystem "cc"
  :depends-on ("prove"
               "trivia"
               "cl-ppcre"
               "alexandria"
               "zenekindarl")
  :serial t
  :components ((:file "package")
               (:file "lexer")
               (:file "cc")))
