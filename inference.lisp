#|
 This file is a part of Staple
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.staple)

(defvar *document-patterns*
  '("README" "readme" "documentation"))

(defvar *image-patterns*
  '("\\.svg$" "\\.png$" "\\.jpg$" "\\.jpeg$" "\\.gif$" "\\.bmp$"))

(defvar *default-template*
  (asdf:system-relative-pathname :staple "default/default.ctml"))

(defclass simple-page (system-page)
  ((document :initarg :document :accessor document)
   (images :initarg :images :accessor images))
  (:default-initargs
   :document NIL
   :images ()
   :input *default-template*))

(defmethod initialize-instance :after ((page simple-page) &key document output language)
  (unless output
    (error "OUTPUT required."))
  (unless language
    (setf (language page) (or (when document (extract-language (file-namestring document)))
                              (when output (extract-language (file-namestring output)))
                              "en")))
  (unless (or (pathname-name output)
              (pathname-type output))
    (setf (output page) (merge-pathnames (filename page) (output page)))))

(defmethod filename ((page simple-page))
  (let ((lang (unless (find (language page) '("en" "eng") :test #'string-equal)
                (language page))))
    (make-pathname :name (format NIL "index~@[-~a~]" lang)
                   :type "html")))

(defmethod definition-wanted-p ((definition definitions:definition) (project simple-page))
  (eql :external (definitions:visibility definition)))

(defmethod definition-wanted-p ((definition definitions:method) (project simple-page))
  NIL)

(defmethod definition-wanted-p ((definition definitions:package) (project simple-page))
  NIL)

(defmethod definition-wanted-p ((definition definitions:compiler-macro) (project simple-page))
  NIL)

(defmethod definition-wanted-p ((definition definitions:declaration) (project simple-page))
  NIL)

(defmethod compile-source (document (page simple-page))
  (let ((*package* (first (packages page))))
    (markup-code-snippets-ignoring-errors
     (compile-source document T))))

(defmethod template-data append ((page simple-page))
  (list :documentation (when (document page)
                         (compile-source (document page) page))
        :images (loop for image in (images page)
                      collect (file-namestring image))))

(defmethod documents ((system asdf:system))
  (let ((source (asdf:system-source-directory system)))
    (when source
      (remove-if-not (lambda (path) (pathname-type->type (pathname-type path)))
                     (find-files source *document-patterns*)))))

(defmethod images ((system asdf:system))
  (let ((source (asdf:system-source-directory system)))
    (when source
      (find-files source *image-patterns*))))

(defmethod subsystems ((system asdf:system))
  ;; FIXME: Package-inferred-systems and such?
  ())

(defmethod page-type ((system asdf:system))
  'simple-page)

(defmethod template ((system asdf:system))
  *default-template*)

(defmethod output-directory ((system asdf:system))
  (merge-pathnames "doc/" (asdf:system-source-directory system)))

(define-condition no-known-output-directory (error)
  ((system :initarg :system :reader system))
  (:report (lambda (c s) (format s "Cannot infer output directory for ~a."
                                 (asdf:component-name (system c))))))

(defmethod infer-project ((system asdf:system) &key output-directory images documents page-type template packages subsystems)
  (load-extension system)
  (let* ((output-directory (or output-directory (output-directory system)))
         (documents (or documents (documents system) '(NIL)))
         (images (or images (images system)))
         (page-type (or page-type (page-type system)))
         (input (or template (template system)))
         (packages (or packages (packages system)))
         (subsystems (or subsystems (subsystems system))))
    (with-value-restart output-directory
      (unless (and (pathnamep output-directory)
                   (pathname-utils:directory-p output-directory))
        (error 'no-known-output-directory :system system)))
    (let ((pages ()))
      (flet ((p (page) (push page pages)))
        (dolist (document documents)
          (p (make-instance page-type
                            :input input
                            :output output-directory
                            :system system
                            :document document
                            :images images
                            :packages packages)))
        (dolist (image images)
          (p (make-instance 'static-page
                            :input image
                            :output (pathname-utils:file-in output-directory image)))))
      (make-instance 'simple-project :pages pages))))
