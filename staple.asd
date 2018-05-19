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
  :homepage "https://github.com/Shinmera/staple"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "xref")
               (:file "transform")
               (:file "clip")
               (:file "page")
               (:file "project")
               (:file "inference")
               (:file "documentation"))
  :depends-on (:staple-package-recording
               :babel
               :clip
               :cl-ppcre
               :definitions
               :documentation-utils))
