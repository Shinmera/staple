#|
 This file is a part of Staple
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.staple.asdf
  (:use :cl :asdf))
(in-package :org.tymoonnext.staple.asdf)

(defsystem staple
  :name "Staple Documentation Generator"
  :version "0.0.1"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A tool to generate documentation about Lisp projects through an HTML template."
  :homepage "https://github.com/Shinmera/staple"
  :serial T
  :components ((:file "package")
               (:file "symbols")
               (:file "clip")
               (:file "documentation")
               (:file "stapler"))
  :depends-on (:clip
               :closer-mop
               :cl-ppcre
               :trivial-arguments
               :3bmd
               :3bmd-ext-code-blocks
               #+:sbcl :sb-cltl2))
