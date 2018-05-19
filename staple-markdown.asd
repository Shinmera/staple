#|
 This file is a part of Staple
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem staple-markdown
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Markdown processing support for Staple"
  :homepage "https://github.com/Shinmera/staple"
  :serial T
  :components ((:file "markdown"))
  :depends-on (:staple
               :3bmd
               :3bmd-ext-code-blocks))
