(asdf:defsystem staple-code-parser
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A code parser tool for documentation markup"
  :homepage "https://Shinmera.github.io/staple/"
  :bug-tracker "https://github.com/Shinmera/staple/issues"
  :source-control (:git "https://github.com/Shinmera/staple.git")
  :serial T
  :components ((:file "package")
               (:file "environment")
               (:file "walker")
               (:file "special-forms")
               (:file "standard-forms")
               (:file "to-definitions")
               (:file "documentation"))
  :depends-on (:alexandria
               :definitions
               :concrete-syntax-tree
               :concrete-syntax-tree-lambda-list
               :concrete-syntax-tree-destructuring
               :eclector
               :eclector-concrete-syntax-tree
               :documentation-utils))
