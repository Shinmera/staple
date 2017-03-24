#|
 This file is a part of Staple
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem staple
  :name "Staple Documentation Generator"
  :version "1.0.1"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A tool to generate documentation about Lisp projects through an HTML template."
  :homepage "https://github.com/Shinmera/staple"
  :serial T
  :components ((:file "package")
               (:file "symbols")
               (:file "clip")
               (:file "fulltext")
               (:file "stapler")
               (:file "documentation"))
  :depends-on (:clip
               :closer-mop
               :cl-ppcre
               :trivial-arguments
               :3bmd
               :3bmd-ext-code-blocks
               :documentation-utils
               (:feature :sbcl (:require :sb-cltl2))))
