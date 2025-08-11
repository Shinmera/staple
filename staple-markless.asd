(asdf:defsystem staple-markless
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Markdown processing support for Staple"
  :homepage "https://shinmera.com/docs/staple/"
  :bug-tracker "https://shinmera.com/project/staple/issues"
  :source-control (:git "https://shinmera.com/project/staple.git")
  :serial T
  :components ((:file "markless"))
  :depends-on (:staple
               :cl-markless-plump))
