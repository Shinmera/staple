#|
 This file is a part of Staple
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.staple)

(defvar *pathname-type-type-map* ())

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
  (compile-source source (pathname-type->type type T)))

(defmethod compile-source ((source pathname) type)
  (compile-source (read-file source) type))

(defmethod compile-source ((source pathname) (type (eql T)))
  (compile-source source (pathname-type source)))

(defmacro define-source-compiler ((type &rest pathname-types) (input) &body body)
  (check-type type keyword)
  `(progn
     ,@(when pathname-types `((setf (pathname-type->type ,type) ',pathname-types)))
     (defmethod compile-source ((,input string) (type (eql ,type)))
       ,@body)
     ',type))

(define-source-compiler (:html "htm" "html" "xhtml") (input)
  (plump:parse input))

(define-source-compiler (:text "txt" "text") (input)
  input)
