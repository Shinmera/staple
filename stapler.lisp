#|
 This file is a part of Staple
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.staple)

(defvar *extension-file* #p"staple.ext.lisp")
(defvar *modern-template* (merge-pathnames "modern.ctml" (asdf:system-source-directory :staple)))
(defvar *legacy-template* (merge-pathnames "plain.ctml" (asdf:system-source-directory :staple)))
(defvar *default-template* *modern-template*)
(defvar *root-clipboard* NIL)

(defun root (field)
  (clip *root-clipboard* field))

(defun to-out (pathname)
  (merge-pathnames (format NIL "~a.out.~a" (pathname-name pathname) (pathname-type pathname)) pathname))

(defun compact (node)
  (typecase node
    (plump:text-node
     (setf (plump:text node) (cl-ppcre:regex-replace-all "(^\\s+)|(\\s+$)" (plump:text node) " ")))
    (plump:element
     (unless (string-equal "pre" (plump:tag-name node))
       (loop for child across (plump:children node)
             do (compact child))))
    (plump:nesting-node
     (loop for child across (plump:children node)
           do (compact child))))
  node)

(defun staple (in &key (out (to-out in)) (if-exists :supersede) clip-args (compact T))
  (let ((*package* (find-package "STAPLE"))
        (*root-clipboard* (apply #'make-clipboard clip-args))
        (document (plump:parse in)))
    (let ((document (apply #'clip:process document clip-args)))
      (when compact (compact document))
      (etypecase out
        ((or string pathname)
         (with-open-file (stream out :direction :output :if-exists if-exists)
           (plump:serialize document stream))
         out)
        (stream
         (plump:serialize document out)
         out)
        (null
         (plump:serialize document NIL))
        (T
         document)))))

(defgeneric system-options (system)
  (:method-combination append :most-specific-first))

(defmethod system-options append ((system asdf:system))
  (list
   :asdf system
   :compact T
   :documentation (find-documentation-file system)
   :logo (let ((file (find-logo-file system)))
           (when file (uiop:enough-pathname file (asdf:system-source-directory system))))
   :name (asdf:component-name system)
   :out (when (asdf:system-source-directory system)
          (merge-pathnames "about.html" (asdf:system-source-directory system)))
   :packages (system-packages system)
   :template *default-template*
   :if-exists :error))

(defgeneric system-package-symbols (system package))

(defmethod system-package-symbols ((system asdf:system) package)
  (package-symbol-objects package))

(defun merge-plist (override base)
  (let ((new (copy-list override)))
    (loop for (key val) on base by #'cddr
          do (unless (getf new key)
               (setf (getf new key) val)))
    new))

(defun ensure-system (system)
  (etypecase system
    (asdf:system system)
    (T (asdf:find-system system T))))

(defvar *loaded-extensions* NIL)

(defun load-extension (system &optional extension)
  (let ((*loaded-extensions* (or *loaded-extensions* (make-hash-table :test 'eq)))
        (system (ensure-system system)))
    (unless (gethash system *loaded-extensions*)
      (setf (gethash system *loaded-extensions*) T)
      (loop for dependency in (asdf:system-depends-on system)
            for depsys = (asdf/find-component:resolve-dependency-spec system dependency)
            do (when depsys (load-extension depsys)))
      (let ((extension (or extension
                           (asdf:system-relative-pathname system *extension-file*))))
        (when (probe-file extension)
          (load extension))))))

(defun generate (asdf-system &rest args &key compact documentation extension logo
                                             name out packages template if-exists
                                             &allow-other-keys)
  (declare (ignore compact documentation logo name out packages template if-exists))
  (let ((asdf-system (ensure-system asdf-system)))
    ;; Load system and extension if necessary/possible.
    (let ((*standard-output* (make-broadcast-stream)))
      (unless (asdf:component-loaded-p asdf-system)
        (asdf:load-system asdf-system))
      (load-extension asdf-system extension))
    ;; Gather system options and normalise.
    (let ((options (merge-plist args (system-options asdf-system))))
      (cond ((eql asdf-system (getf options :asdf))
             (setf (getf options :packages)
                   (loop for package in (getf options :packages)
                         collect (etypecase package
                                   (symbol (string package))
                                   (package (package-name package))
                                   (string package))))
             (setf (getf options :documentation)
                   (prepare-documentation asdf-system (getf options :documentation)))
             ;; Staple the system
             (let ((*current-packages* (getf options :packages))
                   (*system-under-construction* asdf-system))
               (staple (getf options :template)
                       :clip-args options
                       :compact (getf options :compact)
                       :out (getf options :out)
                       :if-exists (getf options :if-exists))))
            (T ;; ASDF system has changed. retry.
             (apply #'generate (getf options :asdf) args))))))
