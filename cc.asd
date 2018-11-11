(defsystem "cc"
  :depends-on ("prove")
  :serial t
  :components ((:file "package")
               (:file "lexer")
               (:file "cc")))
