#|
 This file is a part of Staple
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.staple)

(defvar *loaded-extensions*)

(defclass project ()
  ())

(defgeneric pages (project))

(defmethod generate ((project project) &key (if-exists :supersede))
  (with-simple-restart (abort "Abort ~a" project)
    (dolist (page (pages project))
      (with-simple-restart (continue "Ignore ~a" page)
        (generate page :if-exists if-exists)))))

(defgeneric extension-file (system))

(defmethod extension-file (system)
  (make-pathname :name "staple.ext" :type "lisp"
                 :defaults (asdf:system-source-directory system)))

(defgeneric find-project (project &key &allow-other-keys))

(defmethod find-project (name &rest args)
  (let ((system (asdf:find-system name NIL)))
    (when system
      (apply #'find-project system args))))

(defun load-extension (system)
  (let ((*loaded-extensions* (or *loaded-extensions* (make-hash-table :test 'eq)))
        (system (ensure-system system)))
    (unless (gethash system *loaded-extensions*)
      (setf (gethash system *loaded-extensions*) T)
      (loop for dependency in (asdf:system-depends-on system)
            for depsys = (asdf/find-component:resolve-dependency-spec system dependency)
            do (when depsys (load-extension depsys)))
      (let ((extension (or extension (extension-file system))))
        (when (probe-file extension)
          (load extension))))))

(defmethod find-project ((system asdf:system) &rest args)
  (load-extension system)
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
