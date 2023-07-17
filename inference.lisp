(in-package #:org.shirakumo.staple)

(defvar *document-patterns*
  '("README" "readme" "documentation"))

(defvar *image-patterns*
  '("\\.svg$" "\\.png$" "\\.jpg$" "\\.jpeg$" "\\.gif$" "\\.bmp$"))

(defvar *default-template*
  (asdf:system-relative-pathname :staple "default/default.ctml"))

(defclass simple-page (system-page)
  ((document-package :initarg :document-package :accessor document-package)
   (document :initarg :document :accessor document)
   (images :initarg :images :accessor images))
  (:default-initargs
   :document NIL
   :images ()
   :document-package NIL
   :input *default-template*))

(defmethod initialize-instance :after ((page simple-page) &key document output language)
  (unless output
    (error "OUTPUT required."))
  (unless language
    (setf (language page) (or (when document (extract-language (file-namestring document)))
                              (when output (extract-language (file-namestring output)))
                              :en)))
  (unless (or (pathname-name output)
              (pathname-type output))
    (setf (output page) (merge-pathnames (filename page) (output page)))))

(defmethod filename ((page simple-page))
  (make-pathname :name (if (find (language page) '(:en :eng))
                           "index"
                           (format NIL "index-~(~a~)" (language page)))
                 :type "html"))

(defmethod definition-wanted-p ((definition definitions:definition) (project simple-page))
  (eql :external (definitions:visibility definition)))

(defmethod definition-wanted-p ((definition definitions:method) (project simple-page))
  NIL)

(defmethod definition-wanted-p ((definition definitions:package) (project simple-page))
  NIL)

(defmethod definition-wanted-p ((definition definitions:compiler-macro) (project simple-page))
  NIL)

#+sbcl
(defmethod definition-wanted-p ((definition definitions:declaration) (project simple-page))
  NIL)

(defmethod compile-source ((document pathname) (page simple-page))
  (let ((*package* (or (document-package page)
                       (first (packages page))
                       (find-package "CL-USER"))))
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
      (find-files source *image-patterns* :max-depth 1))))

(defmethod subsystems ((system asdf:system))
  (let ((subsystems ()))
    (asdf:map-systems
     (lambda (subsystem)
       (when (and (not (eql subsystem system))
                  (prefix-p (asdf:component-name system)
                            (asdf:component-name subsystem)))
         (push subsystem subsystems))))
    subsystems))

(defmethod page-type ((system asdf:system))
  'simple-page)

(defmethod template ((system asdf:system))
  *default-template*)

(defmethod output-directory ((system asdf:system))
  (merge-pathnames "docs/" (asdf:system-source-directory system)))

(define-condition no-known-output-directory (error)
  ((system :initarg :system :reader system))
  (:report (lambda (c s) (format s "Cannot infer output directory for ~a."
                                 (asdf:component-name (system c))))))

(defmethod infer-project ((system asdf:system) &key output-directory (images NIL images-p) (documents NIL documents-p) page-type template (packages NIL packages-p) (subsystems NIL subsystems-p))
  (load-extension system)
  (let* ((output-directory (or output-directory (output-directory system)))
         (documents (if documents-p documents (documents system)))
         (images (if images-p images (images system)))
         (page-type (or page-type (page-type system)))
         (template (or template (template system)))
         (packages (if packages-p packages (packages system)))
         (subsystems (if subsystems-p subsystems (subsystems system))))
    (with-value-restart output-directory
      (unless (and (pathnamep output-directory)
                   (pathname-utils:directory-p output-directory))
        (error 'no-known-output-directory :system system)))
    (let ((project (make-instance 'simple-project :output output-directory)))
      (flet ((p (page) (push page (pages project))))
        ;; Do subsystems first to filter documents list.
        (dolist (spec subsystems)
          (destructuring-bind (subsystem . args) (if (listp spec) spec (list spec))
            (let ((sub-directory (or (getf args :output-directory)
                                     (pathname-utils:subdirectory output-directory (asdf:component-name subsystem))))
                  (subdocuments (or (getf args :documents) (documents subsystem) '(NIL)))
                  (images (or (getf args :images) (images subsystem) images))
                  (page-type (or (getf args :page-type) (page-type subsystem) page-type))
                  (template (or (getf args :template) (template subsystem) template))
                  (packages (or (getf args :packages) (packages subsystem))))
              ;; If we have the same source directory, and the documents are
              ;; automatically discovered, we'll set them to NIL here to avoid
              ;; documents intended for the primary system from being used for
              ;; a subsystem.
              (when (subsetp documents subdocuments)
                (setf subdocuments '(NIL)))
              ;; Otherwise, remove all documents from the primary system.
              (setf documents (set-difference documents subdocuments :test #'equal))
              ;; And add pages for the subsystem.
              (dolist (document subdocuments)
                (p (make-instance page-type
                                  :project project
                                  :input template
                                  :output sub-directory
                                  :system subsystem
                                  :document document
                                  :images images
                                  :packages packages)))
              ;; Images!
              (dolist (image images)
                (p (make-instance 'static-page
                                  :project project
                                  :input image
                                  :output (pathname-utils:file-in sub-directory image)))))))
        ;; Pages for the primary documents.
        (dolist (document (or documents '(NIL)))
          (p (make-instance page-type
                            :project project
                            :input template
                            :output output-directory
                            :system system
                            :document document
                            :images images
                            :packages packages)))
        ;; Images and stuff.
        (dolist (image images)
          (p (make-instance 'static-page
                            :project project
                            :input image
                            :output (pathname-utils:file-in output-directory image)))))
      project)))
