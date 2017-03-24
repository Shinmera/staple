#|
 This file is a part of Staple
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.staple)

(defvar *documentation-names* (list "DOCUMENTATION" "documentation"
                                    "README" "readme")
  "A list of strings denoting common file names (without extension) for documentation files.
If you have your own file name, push it onto this list.")

(defvar *documentation-types* (list "md" "txt" "html" "htm" "xhtml" "")
  "A list of strings denoting common file types/extensions for documentation files.
If you have your own file type, push it onto this list.")

(defvar *logo-names* (list "logo" "-logo" "")
  "A list of strings denoting commong file names (without extension) for logos.
If you have your own file name, push it onto this list.")

(defvar *logo-types* (list "svg" "png" "jpg" "jpeg" "gif" "bmp")
  "A list of strings denoting commong image types/extensions for logos.
If you have your own file type, push it onto this list.")

(defgeneric parse-documentation-file (type stream)
  (:documentation "Used to perform special parsing on certain documentation files (such as Markdown).
The type should be an EQL-specializer to a keyword of the file-type/extension.

By default only .md files are specially handled, everything else is simply read as a string."))

(defmethod parse-documentation-file (type stream)
  (plump:slurp-stream stream))

(defmethod parse-documentation-file ((type (eql :md)) stream)
  (let ((3bmd-code-blocks:*code-blocks* T))
    (with-output-to-string (string)
      (3bmd:parse-string-and-print-to-stream
       (parse-documentation-file T stream) string))))

(defun find-documentation-file (asdf)
  "Attempts to find a documentation file in the given asdf system's source directory.
This relies on *DOCUMENTATION-NAMES* and *DOCUMENTATION-TYPES* to find an appropriate file."
  (let ((dir (asdf:system-source-directory asdf)))
    (when dir
      (dolist (type *documentation-types*)
        (dolist (name *documentation-names*)
          (let ((pathname (make-pathname :name name :type type :defaults dir)))
            (when (probe-file pathname)
              (return-from find-documentation-file pathname))))))))

(defun prepare-documentation (system doc)
  "Attempts to prepare the documentation for the given system.
In the case of a pathname, PARSE-DOCUMENTATION-FILE is called.
If the doc is NIL, a matching documentation file is attempted to be found through
FIND-DOCUMENTATION-FILE. If nothing is foudn for that as well, an empty string is
returned instead."
  (let ((doc (or doc (find-documentation-file system))))
    (etypecase doc
      (string doc)
      (pathname
       (with-open-file (stream doc :direction :input :if-does-not-exist :error)
         (parse-documentation-file
          (intern (string-upcase (pathname-type doc)) "KEYWORD") stream)))
      (null ""))))

(defun find-logo-file (asdf)
  "Attempts to find a logo file in the given asdf system's source directory.
See *LOGO-NAMES* and *LOGO-TYPES*. The system will also try to find files by 
prepending or appending the system name to the logo names."
  (let ((dir (asdf:system-source-directory asdf))
        (logo-names (append *logo-names*
                            (mapcar (lambda (name) (concatenate 'string (asdf:component-name asdf) name)) *logo-names*)
                            (mapcar (lambda (name) (concatenate 'string name (asdf:component-name asdf))) *logo-names*))))
    (when dir
      (dolist (type *logo-types*)
        (dolist (name logo-names)
          (let ((pathname (make-pathname :name name :type type :defaults dir)))
            (when (probe-file pathname)
              (return-from find-logo-file pathname))))))))
