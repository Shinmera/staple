#|
 This file is a part of Staple
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem staple-server
  :version "2.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "An interactive documentation viewer using Staple"
  :homepage "https://Shinmera.github.io/staple/"
  :bug-tracker "https://github.com/Shinmera/staple/issues"
  :source-control (:git "https://github.com/Shinmera/staple.git")
  :serial T
  :components ((:file "package")
               (:file "server")
               (:file "documentation"))
  :depends-on (:staple-markdown
               :staple-markless
               :hunchentoot
               :documentation-utils
               :dissect))
