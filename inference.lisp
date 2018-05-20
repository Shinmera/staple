#|
 This file is a part of Staple
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.staple)

(defvar *document-patterns*
  '("README" "readme" "documentation" "about"))

(defvar *image-patterns*
  '("\\.svg$" "\\.png$" "\\.jpg$" "\\.jpeg$" "\\.gif$" "\\.bmp$"))

(defvar *default-template*
  (asdf:system-relative-pathname :staple "default.ctml"))

(defun extract-language (string)
  (cl-ppcre:do-matches-as-strings (code "\\b\\w{2,3}\\b" string)
    (let ((found (gethash code *language-code-map*)))
      (when found (return found)))))

(defclass simple-page (system-page)
  ((document :initarg :document :accessor document))
  (:default-initargs
   :document (error "DOCUMENT required.")
   :input *default-template*))

(defmethod template-data append (project (page simple-page))
  (list :documentation (compile-source (document page) T)
        :language (or (extract-language (pathname-name (document page)))
                      "en")))

(defclass simple-project (project)
  ((pages :initarg :pages :accessor pages)
   (logo :initarg :logo :accessor logo))
  (:default-initargs
   :pages ()
   :logo NIL))

(defmethod template-data append ((project simple-project) page)
  (list :logo (logo project)))

(defun find-files (directory patterns)
  (let ((docs ()))
    (do-directory-tree (file directory docs)
      (when (loop for pattern in patterns
                  thereis (cl-ppcre:scan pattern (file-namestring file)))
        (push file docs)))))

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
        (output-directory (or output-directory (system-output-directory system))))
    (loop for document in (system-documents system)
          for output = (system-output-file system (merge-pathnames output-directory document))
          do (push (make-instance 'simple-page
                                  :output output
                                  :system system
                                  :document document
                                  :project project)
                   (pages project)))
    project))
