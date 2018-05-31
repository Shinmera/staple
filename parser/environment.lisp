#|
 This file is a part of Staple
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.staple.code-parser)

(defclass environment ()
  ((parent :initarg :parent :initform NIL :reader parent)
   (namespaces :initform (make-hash-table :test 'eq) :reader namespaces)))

(defun namespace (namespace environment)
  (gethash namespace (namespaces environment)))

(defun ensure-namespace (namespace environment)
  (ensure-gethash namespace (namespaces environment)
                  (make-hash-table :test 'eq)))

(defmethod lookup (name namespace (environment environment))
  (multiple-value-bind (value defined-p)
      (when-let ((namespace (namespace namespace environment)))
        (gethash name namespace))
    (if defined-p
        (values value T)
        (when-let ((parent (parent environment)))
          (lookup name namespace parent)))))

(defmethod (setf lookup) (value name namespace (environment environment))
  (setf (gethash name (ensure-namespace namespace environment)) value))

(defun augment-environment! (environment names values)
  (loop for name in names
        for value in values
        do (etypecase name
             (symbol
              (setf (lookup name 'variable environment) value))
             ((cons symbol symbol)
              (destructuring-bind (name . namespace) name
                (setf (lookup name namespace environment) value)))))
  environment)

(defun augmented-environment (parent names values &key (class (class-of parent)))
  (augment-environment! (make-instance class :parent parent) names values))
