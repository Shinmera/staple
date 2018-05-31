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

(defun extract-language (string)
  (cl-ppcre:do-matches-as-strings (code "\\b\\w{2,3}\\b" string)
    (let ((found (language-codes:names code)))
      (when found
        (return (values code found))))))

(defclass simple-page (system-page)
  ((document :initarg :document :accessor document)
   (images :initarg :images :accessor images))
  (:default-initargs
   :document NIL
   :images ()
   :input *default-template*))

(defmethod initialize-instance :after ((page simple-page) &key document output language)
  (unless language
    (setf (language page) (or (when document (extract-language (file-namestring document)))
                              (when output (extract-language (file-namestring output)))
                              "en"))))

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

(defmethod template-data append ((page simple-page))
  (list :documentation (when (document page)
                         (markup-code-snippets
                          (compile-source (document page) T)))
        :images (loop for image in (images page)
                      collect (etypecase image
                                (pathname (resolve-source-link (list :file image) page))))))

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

(defmethod output-directory ((system asdf:system))
  (asdf:system-source-directory system))

(defmethod output-file ((system asdf:system) document)
  (let* ((lang (extract-language (pathname-name document)))
         (lang (unless (find lang '("en" "eng") :test #'string-equal) lang)))
    (merge-pathnames (make-pathname :name (format NIL "index~@[-~a~]" lang)
                                    :type "html")
                     document)))

(define-condition no-known-output-directory (error)
  ((system :initarg :system :reader system))
  (:report (lambda (c s) (format s "Cannot infer output directory for ~a."
                                 (asdf:component-name (system c))))))

;; FIXME: subsystems
(defmethod infer-project ((system asdf:system) &key output-directory images documents page-type)
  (load-extension system)
  (let* ((output-directory (or output-directory (output-directory system)))
         (documents (or documents (documents system)))
         (images (or images (images system)))
         (page-type (or page-type (page-type system))))
    (restart-case (unless output-directory
                    (error 'no-known-output-directory :system system))
      (use-value (value &optional condition)
        (declare (ignore condition))
        (setf output-directory value)))
    (let ((pages (if documents
                     (loop for document in documents
                           for output = (output-file system (merge-pathnames output-directory document))
                           collect (make-instance page-type
                                                  :output output
                                                  :system system
                                                  :document document
                                                  :images images))
                     (list (make-instance page-type
                                          :output (output-file system output-directory)
                                          :system system
                                          :images images)))))
      (make-instance 'simple-project :pages pages))))
