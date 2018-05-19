#|
 This file is a part of Staple
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.staple)

(defvar *document-patterns*
  '("readme" "documentation" "about"))

(defvar *image-patterns*
  '("\\.svg$" "\\.png$" "\\.jpg$" "\\.jpeg$" "\\.gif$" "\\.bmp$"))

(defvar *default-template*
  (asdf:system-relative-pathname :staple "default.ctml"))

(defun extract-language (string)
  (cl-ppcre:do-matches-as-strings (code "\\b\\w{2,3}\\b" string)
    (let ((found (gethash code *language-code-map*)))
      (when found (return found)))))

(defclass simple-project (project)
  ((pages :initarg :pages :accessor pages))
  (:default-initargs :pages ()))

(defun find-files (directory patterns)
  (let ((docs ()))
    (do-directory-tree (file directory docs)
      (when (loop for pattern in patterns
                  thereis (cl-ppcre:scan pattern (file-namestring file)))
        (push file docs)))))

(defmethod system-documents ((system asdf:system))
  (find-files (asdf:system-source-directory system)
              *document-patterns*))

(defmethod system-images ((system asdf:system))
  (find-files (asdf:system-source-directory system)
              *image-patterns*))

(defun ensure-system-options-complete (options)
  (flet ((complete-by-systems (option function)
           (unless (getf options option)
             (setf (getf options option)
                   (loop for system in (getf options :systems)
                         append (funcall function system))))))
    (complete-by-systems :documents #'system-documents)
    (complete-by-systems :images #'system-images)
    (complete-by-systems :packages #'system-packages)
    options))

(defgeneric system-options (system)
  (:method-combination append :most-specific-first))

(defmethod system-options append ((system asdf:system))
  (list :systems (list system)
        :template *default-template*
        :if-exists :error))

(defmethod system-options :around ((system asdf:system))
  (ensure-system-options-complete (call-next-method)))

(defmethod infer-project ((system asdf:system) &rest options)
  (load-extension system)
  (let ((options (system-options system)))
    (let ((*standard-output* (make-broadcast-stream)))
      (handler-bind ((style-warning #'muffle-warning))
        (mapc #'asdf:load-system (getf options :systems))))
    (let ((pages ()))
      
      (make-instance 'project :pages pages))))
