#|
 This file is a part of Staple
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem staple-package-recording
  :name "Staple System Package Recorder"
  :version "1.0.1"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Collects information about packages being defined with an ASDF system."
  :homepage "https://Shinmera.github.io/staple/"
  :bug-tracker "https://github.com/Shinmera/staple/issues"
  :source-control (:git "https://github.com/Shinmera/staple.git")
  :serial T
  :components ((:file "recording"))
  :depends-on ())
