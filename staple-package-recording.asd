(asdf:defsystem staple-package-recording
  :name "Staple System Package Recorder"
  :version "1.0.1"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Collects information about packages being defined with an ASDF system."
  :homepage "https://shinmera.com/docs/staple/"
  :bug-tracker "https://shinmera.com/project/staple/issues"
  :source-control (:git "https://shinmera.com/project/staple.git")
  :serial T
  :components ((:file "recording"))
  :depends-on ())
