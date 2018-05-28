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
  (asdf:system-relative-pathname :staple "default.ctml"))

(defun extract-language (string)
  (cl-ppcre:do-matches-as-strings (code "\\b\\w{2,3}\\b" string)
    (let ((found (language-codes:names code)))
      (when found
        (return (values code found))))))

(defclass simple-page (system-page)
  ((document :initarg :document :accessor document))
  (:default-initargs
   :document NIL
   :input *default-template*))

(defmethod initialize-instance :after ((page simple-page) &key document output language)
  (unless language
    (setf (language page) (or (when document (extract-language (file-namestring document)))
                              (when output (extract-language (file-namestring output)))
                              "en"))))

(defmethod definition-wanted-p (definition (page simple-page))
  (definition-wanted-p definition (project page)))

(defmethod template-data append (project (page simple-page))
  (list :documentation (when (document page)
                         (compile-source (document page) T))))

(defclass simple-project (project)
  ((pages :initarg :pages :accessor pages)
   (logo :initarg :logo :accessor logo))
  (:default-initargs
   :pages ()
   :logo NIL))

(defmethod template-data append ((project simple-project) page)
  (list :logo (logo project)))

(defmethod definition-wanted-p ((definition definitions:definition) (project simple-project))
  (eql :external (definitions:visibility definition)))

(defmethod definition-wanted-p ((definition definitions:method) (project simple-project))
  NIL)

(defmethod definition-wanted-p ((definition definitions:package) (project simple-project))
  NIL)

(defmethod definition-wanted-p ((definition definitions:compiler-macro) (project simple-project))
  NIL)

(defmethod definition-wanted-p ((definition definitions:declaration) (project simple-project))
  NIL)

(defmethod system-documents ((system asdf:system))
  (remove-if-not (lambda (path) (pathname-type->type (pathname-type path)))
                 (find-files (asdf:system-source-directory system)
                             *document-patterns*)))

(defmethod system-images ((system asdf:system))
  (find-files (asdf:system-source-directory system)
              *image-patterns*))

(defmethod system-subsystems ((system asdf:system))
  ;; FIXME: Package-inferred-systems and such?
  ())

(defmethod system-output-directory ((system asdf:system))
  (asdf:system-source-directory system))

(defmethod system-output-file ((system asdf:system) document)
  (let* ((lang (extract-language (pathname-name document)))
         (lang (unless (find lang '("en" "eng") :test #'string-equal) lang)))
    (merge-pathnames (make-pathname :name (format NIL "index~@[-~a~]" lang)
                                    :type "html")
                     document)))

;; FIXME: subsystems
(defmethod infer-project ((system asdf:system) &key output-directory logo)
  (load-extension system)
  (let ((*standard-output* (make-broadcast-stream)))
    (handler-bind ((style-warning #'muffle-warning))
      (asdf:load-system system)))
  (let ((project (make-instance 'simple-project
                                ;; FIXME: score image files
                                :logo (or logo (first (system-images system)))))
        (output-directory (or output-directory (system-output-directory system)))
        (documents (system-documents system)))
    (if documents
        (loop for document in documents
              for output = (system-output-file system (merge-pathnames output-directory document))
              do (push (make-instance 'simple-page
                                      :output output
                                      :system system
                                      :document document
                                      :project project)
                       (pages project)))
        (push (make-instance 'simple-page
                             :output (system-output-file system output-directory)
                             :system system
                             :project project)
              (pages project)))
    project))
