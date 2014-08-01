#|
 This file is a part of Staple
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.staple)

(defvar *documentation-names* (list "DOCUMENTATION" "documentation"
                                    "README" "readme"
                                    "ABOUT" "about"))
(defvar *documentation-types* (list "md" "txt" "html" "htm" "xhtml" ""))

(defgeneric parse-documentation-file (type stream))

(defmethod parse-documentation-file (type stream)
  (plump::slurp-stream stream))

(defmethod parse-documentation-file ((type (eql :md)) stream)
  (with-output-to-string (string)
    (3bmd:parse-string-and-print-to-stream
     (parse-documentation-file T stream) string)))

(defun find-documentation-file (asdf)
  (dolist (type *documentation-types*)
    (dolist (name *documentation-names*)
      (let ((pathname (merge-pathnames (make-pathname :name name :type type)
                                       (asdf:system-source-directory asdf))))
        (when (probe-file pathname)
          (return-from find-documentation-file pathname))))))

(defun prepare-documentation (system doc)
  (let ((doc (or doc (find-documentation-file system))))
    (etypecase doc
      (string doc)
      (pathname
       (with-open-file (stream doc :direction :input :if-does-not-exist :error)
         (parse-documentation-file
          (intern (string-upcase (pathname-type doc)) "KEYWORD") stream))))))
