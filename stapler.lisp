#|
 This file is a part of Staple
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.staple)

(defvar *default-template* (merge-pathnames "default.ctml" (asdf:system-source-directory :staple))
  "Pathname to the default template to use in GENERATE.")
(defvar *root-clipboard* NIL
  "Side-storage in order to allow accessing of the lower-level clipboard 
from within different clipboard environments.")

(defun root (field)
  "Shorthand for (CLIP *ROOT-CLIPBOARD FIELD)"
  (clip *root-clipboard* field))

(defun to-out (pathname)
  "Returns a pathname whose file-name (not extension) is postfixed by .out ."
  (merge-pathnames (format NIL "~a.out.~a" (pathname-name pathname) (pathname-type pathname)) pathname))

(defun system-out (system)
  "Returns a pathname to 'about.html' within the given system's source-directory."
  (merge-pathnames "about.html" (asdf:system-source-directory (asdf:find-system system))))

(defun compact (node)
  (typecase node
    (plump:text-node
     (setf (plump:text node) (string-trim '(#\Space #\Newline #\Tab #\Return #\Linefeed #\Page)
                                          (plump:text node))))
    (plump:element
     (unless (string-equal "pre" (plump:tag-name node))
       (loop for child across (plump:children node)
             do (compact child))))
    (plump:nesting-node
     (loop for child across (plump:children node)
           do (compact child))))
  node)

(defun staple (in &key (out (to-out in)) (if-exists :supersede) clip-args)
  "Performs stapling actions/clip processing on the IN document.

IN is parsed by PLUMP:PARSE and the results are written to OUT.
Through CLIP-ARGS additional arguments can be passed to CLIP:GENERATE.
These will also appear in the *ROOT-CLIPBOARD*."
  (let ((*package* (find-package "STAPLE"))
        (*root-clipboard* (apply #'make-clipboard clip-args))
        (document (plump:parse in)))
    (let ((document (compact (apply #'clip:process document clip-args))))
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
                               (if-exists :error))
  "Generates documentation for the given asdf-system.

ASDF-SYSTEM    --- The name or object of the ASDF system to write documentation for.
PACKAGES       --- A list of package names to documentate.
NAME           --- The name of the project.
DOCUMENTATION  --- A string or pathname that contains additional documentation info.
LOGO           --- A string or URL to the logo to use in the template. If not supplied
                   no logo image is inserted.
OUT            --- The file to write the resulting documentation to.
TEMPLATE       --- Pathname to the clip template file to process.
IF-EXISTS      --- Argument for WITH-OPEN-FILE."
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
     :clip-args (list 'asdf asdf-system 'name name 'packages packages 'documentation documentation 'logo logo))))
