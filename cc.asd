(require "parsergen")

(defsystem "cc"
  :depends-on ("prove" "trivia" "cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "lexer")
               (:file "cc")))
