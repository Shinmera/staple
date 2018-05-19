#|
 This file is a part of Staple
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.staple)

(defclass project ()
  ((pages :initarg :pages :accessor pages))
  (:default-initargs :pages ()))

(defmethod generate ((project project) &key (if-exists :supersede))
  (with-simple-restart (abort "Abort ~a" project)
    (dolist (page (pages project))
      (with-simple-restart (continue "Ignore ~a" page)
        (generate page :if-exists if-exists)))))

(defmethod extension-file (system)
  (make-pathname :name "staple.ext" :type "lisp"
                 :defaults (asdf:system-source-directory system)))

(defgeneric find-project (project &key &allow-other-keys))

(defmethod find-project (name &rest args)
  (let ((system (asdf:find-system name NIL)))
    (when system
      (apply #'find-project system args))))

(defmethod find-project ((system asdf:system) &rest args)
  (let ((ext (extension-file system)))
    (when (probe-file ext)
      (load ext :verbose NIL :print NIL)))
  ;; Now that the extension might have been loaded we can look
  ;; for new methods on this function specific to the system.
  (when (or (find-method #'find-project () `((eql ,system)))
            (find-method #'find-project () `((eql ,(system-name system)))))
    (apply #'find-project (system-name system) args)))

(defgeneric infer-project (project &key &allow-other-keys))

(defmethod infer-project (name &rest args)
  (let ((system (asdf:find-system name NIL)))
    (when system
      (apply #'infer-project system args))))

(defmethod generate (project &rest args)
  (let ((project (or (apply #'find-project project args)
                     (apply #'infer-project project args)
                     (error "Cannot generate documentation for ~s: Could not find or infer a project."
                            project))))
    (apply #'generate project args)))
