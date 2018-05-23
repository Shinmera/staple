#|
 This file is a part of Staple
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.staple)

(defvar *language-code-map* (make-hash-table :test 'equalp))

(defun load-language-codes (file table)
  (with-open-file (stream file)
    (loop for entry = (read stream NIL :eof)
          until (eql entry :eof)
          do (setf (gethash (first entry) table) (rest entry)))
    table))

(load-language-codes
 (asdf:system-relative-pathname :staple "data/iso-639-1.lisp")
 *language-code-map*)

(load-language-codes
 (asdf:system-relative-pathname :staple "data/iso-639-3.lisp")
 *language-code-map*)

(defun code->language (code)
  (gethash code *language-code-map*))
