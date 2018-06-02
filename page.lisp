#|
 This file is a part of Staple
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.staple)

(defvar *page*)

(defclass page ()
  ((title :initarg :title :accessor title)
   (language :initarg :language :initform "en" :accessor language)
   (output :initarg :output :accessor output))
  (:default-initargs
   :output NIL
   :title NIL))

(defmethod initialize-instance :after ((page page) &key output language)
  (unless language
    (setf (language page) (or (when output (extract-language (file-namestring output)))
                              "en"))))

(defmethod print-object ((page page) stream)
  (print-unreadable-object (page stream :type T)
    (format stream "~s (~a)" (title page) (language page))))

(defgeneric generate (page &key if-exists &allow-other-keys))

(defmethod generate :around ((page page) &key)
  (let ((*page* page))
    (call-next-method)))

(defmethod generate :before ((page page) &key)
  (with-value-restart (output page)
    (unless (typep (output page) 'stream-designator)
      (error "The output file for ~a is not a stream designator."
             page))))

(defclass input-page (page)
  ((input :initarg :input :accessor input))
  (:default-initargs
   :input NIL))

(defmethod generate :before ((page input-page) &key)
  (with-value-restart (input page)
    (unless (typep (input page) 'stream-designator)
      (error "The input file for ~a is not a pathname."
             page))))

(defclass static-page (input-page)
  ())

(defmethod generate ((page static-page) &key (if-exists :error))
  (unless (equal (output page) (input page))
    (with-stream (out (output page) :direction :output
                                    :if-exists if-exists
                                    :element-type '(unsigned-byte 8))
      (with-stream (in (input page) :direction :input
                                    :element-type '(unsigned-byte 8))
        (loop with buffer = (make-array 4096 :element-type '(unsigned-byte 8))
              for read = (read-sequence buffer in)
              while (< 0 read)
              do (write-sequence buffer out :end read))))))

(defclass compiled-page (input-page)
  ())

(defmethod generate ((page compiled-page) &key (if-exists :error) (compact T))
  (let ((data (compile-source (input page) (pathname-type (input page))))
        (plump:*tag-dispatchers* plump:*html-tags*))
    (handler-bind ((plump:invalid-xml-character #'abort)
                   (plump:discouraged-xml-character #'continue))
      (etypecase data
        (plump:node
         (with-stream (out (output page) :direction :output
                                         :if-exists if-exists)
           (when compact (compact data))
           (plump:serialize data out)))
        (string
         (with-stream (out (output page) :direction :output
                                         :if-exists if-exists)
           (write-string data out)))
        ((vector (unsigned-byte 8))
         (with-stream (out (output page) :direction :output
                                         :if-exists if-exists
                                         :element-type '(unsigned-byte 8))
           (write-sequence data out)))))))

(defclass templated-page (input-page)
  ())

(defgeneric template-data (page)
  (:method-combination append :most-specific-first))

(defmethod template-data append ((page templated-page))
  (list :title (title page)
        :language (language page)
        :input (input page)
        :output (output page)
        :page page))

(defmethod generate ((page templated-page) &key (if-exists :error) (compact T))
  (with-stream (out (output page) :direction :output
                                  :if-exists if-exists)
    (handler-bind ((plump:invalid-xml-character #'abort)
                   (plump:discouraged-xml-character #'continue))
      (let* ((*package* #.*package*)
             (plump:*tag-dispatchers* plump:*html-tags*)
             (node (apply #'clip:process
                          (plump:parse (input page))
                          (template-data page))))
        (when compact (compact node))
        (plump:serialize node out)))))

(defclass definitions-index-page (templated-page)
  ((packages :initform NIL :accessor packages))
  (:default-initargs
   :title "Index"))

(defmethod shared-initialize :after ((page definitions-index-page) slots &key packages)
  (when packages (setf (packages page) packages)))

(defmethod (setf packages) :around (packages (page definitions-index-page))
  (call-next-method (mapcar #'ensure-package packages) page))

(defmethod template-data append ((page definitions-index-page))
  (list :packages (packages page)))

(defgeneric format-documentation (definition page))

(defmethod format-documentation ((definition definitions:definition) (page definitions-index-page))
  (format-documentation (maybe-lang-docstring definition (language page)) page))

(defmethod format-documentation ((null null) (page definitions-index-page))
  NIL)

(defmethod format-documentation ((docstring string) (page definitions-index-page))
  (flet ((replace-see (string start end mstart mend rstart rend)
           (declare (ignore start end))
           (let* ((match (subseq string (aref rstart 0) (aref rend 0)))
                  (identifier (plump:decode-entities match))
                  (xref (xref identifier)))
             (if xref
                 (format NIL "See <a href=\"~a\" class=\"xref\">~a</a>"
                         (plump:encode-entities xref) match)
                 (subseq string mstart mend)))))
    (let* ((docstring (plump:encode-entities docstring))
           (docstring (cl-ppcre:regex-replace-all "[sS]ee (.*)" docstring #'replace-see)))
      (format NIL "<pre>~a</pre>" docstring))))

(defgeneric resolve-source-link (source page))

(defmethod resolve-source-link ((definition definitions:definition) (page definitions-index-page))
  (let ((source (absolute-source-location (definitions:source-location definition))))
    (when source (resolve-source-link source page))))

(defmethod resolve-source-link (source (page definitions-index-page))
  (cond ((pathname-utils:subpath-p (truename (getf source :file))
                                   (truename (uiop:pathname-directory-pathname (output page))))
         (format NIL "~a~@[#~a:~a~]"
                 (enough-namestring (truename (getf source :file))
                                    (truename (uiop:pathname-directory-pathname (output page))))
                 (getf source :row) (getf source :col)))
        (T
         (format NIL "file://~a~@[#~a:~a~]"
                 (truename (getf source :file)) (getf source :row) (getf source :col)))))

(defgeneric definition-wanted-p (definition page))

(defmethod definition-wanted-p ((definition definitions:definition) (page definitions-index-page))
  T)

(defgeneric definitions (page package))

(defmethod definitions ((page definitions-index-page) package)
  (delete-if-not (lambda (def) (definition-wanted-p def page))
                 (definitions:find-definitions package :package package)))

(defmethod definitions :around (page package)
  (sort-definitions (call-next-method)))

(defclass system-page (definitions-index-page)
  ((system :initarg NIL :accessor system)))

(defmethod shared-initialize :after ((page system-page) slots &key system title)
  (when system (setf (system page) system))
  (unless (packages page)
    (setf (packages page) (packages (system page))))
  (unless title
    (setf (title page) (titleize (asdf:component-name system)))))

(defmethod (setf system) :around (system (page system-page))
  (call-next-method (etypecase system
                      ((or string symbol) (asdf:find-system system T))
                      (asdf:system system))
                    page))

(defmethod template-data append ((page system-page))
  (list :system (system page)))

(defun github-project-root (github-url)
  (cl-ppcre:register-groups-bind (user-1 user-2 repo) ("(?:https?://|//)?(?:github.com/([\\w-]+)|([\\w-]+).github.io)/([\\w-]+)" github-url)
    (format NIL "https://github.com/~a/~a" (or user-1 user-2) repo)))

(defmethod resolve-source-link (source (page system-page))
  (cond ((and (search "github" (asdf:system-homepage (system page)))
              (pathname-utils:subpath-p (truename (getf source :file))
                                        (truename (asdf:system-source-directory (system page)))))
         (format NIL "~a/blob/master/~a~@[#L~a~]"
                 (github-project-root (asdf:system-homepage (system page)))
                 (enough-namestring (getf source :file)
                                    (asdf:system-source-directory (system page)))
                 (getf source :row)))
        (T
         (call-next-method))))
