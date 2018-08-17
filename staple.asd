#|
 This file is a part of Staple
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem staple
  :version "2.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A tool to generate documentation about Lisp projects through an HTML template."
  :homepage "https://Shinmera.github.io/staple/"
  :bug-tracker "https://github.com/Shinmera/staple/issues"
  :source-control (:git "https://github.com/Shinmera/staple.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "xref")
               (:file "transform")
               (:file "code-format")
               (:file "clip")
               (:file "page")
               (:file "project")
               (:file "inference")
               (:file "documentation"))
  :depends-on (:staple-package-recording
               :staple-code-parser
               :babel
               :clip
               :cl-ppcre
               :definitions
               :pathname-utils
               :language-codes
               :documentation-utils))
