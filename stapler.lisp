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
(defvar *before-load-packages* (make-hash-table :test 'eql))
(defvar *system-packages* (make-hash-table :test 'eql))

(defun efind-package (name)
  (or (find-package name)
      (find-package (string-upcase name))
      (error "No package with name ~s could be found." name)))

(defun system-packages (system)
  (let* ((system (etypecase system
                   (asdf:system system)
                   ((or symbol string) (asdf:find-system system T))))
         (packages (gethash system *system-packages* :not-recorded)))
    (cond ((eql :not-recorded packages)
           ;; Heuristic, ech.
           (let ((pkg (find-package (asdf:component-name system))))
             (when pkg (list pkg))))
          (T
           packages))))

(defun (setf system-packages) (packages system)
  (let ((system (etypecase system
                  (asdf:system system)
                  ((or symbol string) (asdf:find-system system T))))
        (packages (loop for package in packages
                        collect (etypecase package
                                  (package package)
                                  ((or string symbol) (efind-package package))))))
    (setf (gethash system *system-packages*) packages)))

;; Record all packages before system load
(defmethod asdf:perform :after ((o asdf:prepare-op) (s asdf:system))
  (setf (gethash s *before-load-packages*) (list-all-packages)))

;; Difference recorded list against current list to get all packages defined.
(defmethod asdf:perform :after ((o asdf:load-op) (s asdf:system))
  (let* ((old-packages (gethash s *before-load-packages* (list-all-packages)))
         (new-packages (set-difference (list-all-packages) old-packages)))
    ;; Combine with previous ones to account for potential package addition
    ;; after later reloading of the system.
    (setf (system-packages s)
          (union (system-packages s) (reverse new-packages)))))

(defun root (field)
  (clip *root-clipboard* field))

(defun to-out (pathname)
  (merge-pathnames (format NIL "~a.out.~a" (pathname-name pathname) (pathname-type pathname)) pathname))

(defun system-out (system)
  (merge-pathnames "about.html" (asdf:system-source-directory (asdf:find-system system))))

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

(defun generate (asdf-system &key
                               (packages (system-packages asdf-system))
                               (name asdf-system)
                               documentation logo
                               (out (system-out asdf-system))
                               (template *default-template*)
                               (compact T)
                               (if-exists :error))
  (when (typep asdf-system 'asdf:system)
    (setf asdf-system (asdf:component-name asdf-system)))
  (unless (asdf:component-loaded-p (asdf:find-system asdf-system T))
    (asdf:load-system asdf-system))
  (when (probe-file (asdf:system-relative-pathname asdf-system *extension-file*))
    (load (asdf:system-relative-pathname asdf-system *extension-file*)))
  (let* ((asdf (asdf:find-system asdf-system))
         (name (string name))
         (packages (loop for package in packages
                         collect (etypecase package
                                   (symbol (string package))
                                   (package (package-name package))
                                   (string package))))
         (documentation (prepare-documentation asdf documentation))
         (logo (if (pathnamep out)
                   (uiop:enough-pathname (or logo (find-logo-file asdf)) (uiop:pathname-directory-pathname out))
                   logo))
         (*current-packages* packages))
    (staple
     template
     :out out :if-exists if-exists
     :clip-args (list :asdf asdf
                      :name name
                      :packages packages
                      :documentation documentation
                      :logo logo)
     :compact compact)))
