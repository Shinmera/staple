#|
 This file is a part of Staple
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.staple)

(defvar *pathname-type-type-map*
  '((:text "txt" "text")
    (:html "htm" "html" "xhtml")))

(defun pathname-type->type (type &optional errorp)
  (loop for (result . types) in *pathname-type-type-map*
        do (when (find type types :test #'string-equal)
             (return result))
        finally (when errorp (error "Unknown pathname-type ~s." type))))

(defun (setf pathname-type->type) (types type)
  (check-type type keyword)
  (check-type types list)
  (setf *pathname-type-type-map*
        (remove type *pathname-type-type-map* :key #'car))
  (when types
    (setf *pathname-type-type-map*
          (list* (list* type types)
                 *pathname-type-type-map*)))
  types)

(defgeneric compile-source (source type))

(defmethod compile-source (source (type string))
  (compile-source source (pathname-type->type type)))

(defmethod compile-source ((source pathname) type)
  (compile-source
   (with-open-file (in source)
     (with-output-to-string (out)
       (loop with buffer = (make-array 4096 :element-type 'character)
             for read = (read-sequence buffer in)
             while (< 0 read)
             do (write-sequence buffer out))))
   type))

(defmethod compile-source ((source string) (type :html))
  (plump:parse source))

(defmethod compile-source ((source pathname) (type :html))
  (plump:parse source))

(defmethod compile-source ((source string) (type :text))
  source)

(defmacro define-source-compiler ((type &rest pathname-types) (input) &body body)
  (check-type type keyword)
  `(progn
     (setf (pathname-type->type ,type) ',pathname-types)
     (defmethod compile-source ((,input string) (type ,type))
       ,@body)))
