#|
 This file is a part of Staple
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.staple)

(defvar *modern-template* (merge-pathnames "modern.ctml" (asdf:system-source-directory :staple)))
(defvar *legacy-template* (merge-pathnames "plain.ctml" (asdf:system-source-directory :staple)))
(defvar *default-template* *modern-template*)
(defvar *root-clipboard* NIL)

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
                               (packages (list asdf-system))
                               (name asdf-system)
                               documentation logo
                               (out (system-out asdf-system))
                               (template *default-template*)
                               (compact T)
                               (if-exists :error))
  (when (typep asdf-system 'asdf:system)
    (setf asdf-system (asdf:component-name asdf-system)))
  (let* ((asdf (or (asdf:find-system asdf-system)
                   (error "No such ASDF system: ~a" asdf-system)))
         (name (string name))
         (packages (mapcar #'string packages))
         (documentation (prepare-documentation asdf documentation))
         (logo (if (pathnamep out)
                   (uiop:enough-pathname (or logo (find-logo-file asdf)) (uiop:pathname-directory-pathname out))
                   logo))
         (*current-packages* packages))
    (staple
     template
     :out out :if-exists if-exists
     :clip-args (list 'asdf asdf-system 'name name 'packages packages 'documentation documentation 'logo logo)
     :compact compact)))