#|
 This file is a part of Staple
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.staple)

(defvar *load-prohibited-systems*
  (loop for name in '("asdf" "asdf-package-system" "asdf/defsystem" "asdf/driver" "asdf/prelude")
        for sys = (asdf:find-system name NIL)
        when sys collect sys))
(defvar *loaded-extensions*)

(defclass project ()
  ((output :initarg :output :accessor output)))

(defgeneric pages (project))

(defmethod generate ((project project) &rest args)
  (let ((results ()))
    (with-simple-restart (abort "Abort ~a" project)
      (dolist (page (pages project))
        (with-simple-restart (continue "Ignore ~a" page)
          (push (apply #'generate page args) results))))
    (values project (nreverse results))))

(defmethod relative-path ((to project) from)
  (relative-path (output to) from))

(defmethod relative-path (to (from project))
  (relative-path (output from) to))

(defclass simple-project (project)
  ((pages :initarg :pages :accessor pages))
  (:default-initargs
   :pages ()))

(defgeneric extension-file (system))

(defmethod extension-file (system)
  (let ((source (asdf:system-source-directory system)))
    (when source (make-pathname :name "staple.ext" :type "lisp" :defaults source))))

(defgeneric find-project (project &key &allow-other-keys))

(defmethod find-project (name &rest args)
  (let ((system (asdf:find-system name NIL)))
    (when system
      (apply #'find-project system args))))

(defun make-extension-load-table ()
  (let ((table (make-hash-table :test 'eq)))
    (dolist (sys *load-prohibited-systems* table)
      (setf (gethash sys table) T))))

(defun load-extension (system)
  (let ((*loaded-extensions* (if (boundp '*loaded-extensions*)
                                 *loaded-extensions*
                                 (make-extension-load-table)))
        (system (ensure-system system)))
    (unless (gethash system *loaded-extensions*)
      (setf (gethash system *loaded-extensions*) T)
      (load-system-quietly system)
      (loop for dependency in (asdf:system-depends-on system)
            for depsys = (asdf/find-component:resolve-dependency-spec system dependency)
            do (when depsys (load-extension depsys)))
      (let ((extension (extension-file system)))
        (when (and extension (probe-file extension))
          (load extension)))
      system)))

(defmethod find-project ((system asdf:system) &rest args)
  (load-extension system)
  ;; Now that the extension might have been loaded we can look
  ;; for new methods on this function specific to the system.
  (when (or (find-method #'find-project () `((eql ,system)) NIL)
            (find-method #'find-project () `((eql ,(system-name system))) NIL))
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
