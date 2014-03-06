#|
 This file is a part of Staple
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
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
  :serial T
  :components ((:file "package")
               (:file "symbols")
               (:file "templating"))
  :depends-on (:lquery
               :closer-mop
               :alexandria
               :split-sequence
               :cl-ppcre))
