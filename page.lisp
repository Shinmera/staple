(in-package #:org.shirakumo.staple)

(defvar *page*)

(defclass page ()
  ((title :initarg :title :accessor title)
   (language :initarg :language :initform :en :accessor language)
   (output :initarg :output :accessor output)
   (project :initarg :project :accessor project))
  (:default-initargs
   :output NIL
   :title NIL
   :project (error "PROJECT required.")))

(defmethod initialize-instance :after ((page page) &key output language)
  (unless language
    (setf (language page) (or (when output (extract-language (file-namestring output)))
                              :en))))

(defmethod print-object ((page page) stream)
  (print-unreadable-object (page stream :type T)
    (format stream "~s (~a)" (title page) (language page))))

(defgeneric page-variants (page))

(defmethod page-variants ((page page))
  (loop for other in (pages (project page))
        when (and (not (eq other page))
                  (equal (title page) (title other)))
        collect other))

(defgeneric page-siblings (page))

(defmethod page-siblings ((page page))
  (loop for other in (pages (project page))
        when (and (title other)
                  (equal (language page) (language other))
                  (or (eq page other) (not (equal (title page) (title other)))))
        collect other))

(defgeneric generate (page &key &allow-other-keys))

(defmethod generate :around ((page page) &key)
  (let ((*page* page))
    (call-next-method)))

(defmethod generate :before ((page page) &key)
  (with-value-restart (output page)
    (unless (typep (output page) 'stream-designator)
      (error "The output file for ~a is not a stream designator."
             page))
    (when (typep (output page) 'pathname)
      (ensure-directories-exist (output page)))))

(defmethod relative-path ((to page) (project (eql T)))
  (relative-path to (project to)))

(defmethod relative-path ((to page) from)
  (relative-path (output to) from))

(defmethod relative-path (to (from page))
  (relative-path to (output from)))

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
        :project (project page)
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
  ((packages :initform NIL :accessor packages)))

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
                  xref)
             (cond ((cl-ppcre:scan "^[-a-zA-Z]+://" identifier)
                    (format NIL "See <a href=\"~a\" class=\"exref\">~a</a>"
                            match match))
                   (T
                    (cl-ppcre:register-groups-bind (identifier type) ("^(.*?)\\s*(?:\\(([a-zA-Z\\-:]+)\\))?$" identifier)
                      (setf type (if type
                                     (multiple-value-bind (name package) (parse-symbol type)
                                       (cond ((null package)
                                              (find-symbol name (find-package (string '#:definitions))))
                                             ((find-package package)
                                              (find-symbol name (find-package package)))))
                                     T))
                      (setf xref (xref identifier type))
                      (if xref
                          (format NIL "See <a href=\"~a\" class=\"xref\">~a</a>"
                                  (plump:encode-entities xref) match)
                          (subseq string mstart mend))))))))
    (let* ((docstring (plump:encode-entities docstring))
           (docstring (cl-ppcre:regex-replace-all "[sS]ee (.*)" docstring #'replace-see)))
      (format NIL "<pre>~a</pre>" docstring))))

(defgeneric resolve-source-link (source page))

(defmethod resolve-source-link ((definition definitions:definition) (page definitions-index-page))
  (when (find (symbol-package (definitions:symbol definition)) (packages page))
    (let ((source (absolute-source-location (definitions:source-location definition))))
      (when source (resolve-source-link source page)))))

(defmethod resolve-source-link (source (page definitions-index-page))
  (cond ((pathname-utils:subpath-p
          (truename (getf source :file))
          (truename (output (project page))))
         (format NIL "~a~@[#~a:~a~]"
                 (pathname-utils:relative-pathname
                  (truename (getf source :file))
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

(defun gitlab-project-root (gitlab-url)
  (cl-ppcre:register-groups-bind (user-1 user-2 repo) ("(?:https?://|//)?(?:gitlab.com/([\\w-]+)|([\\w-]+).gitlab.io)/([\\w-]+)" gitlab-url)
    (format NIL "https://gitlab.com/~a/~a" (or user-1 user-2) repo)))

(defmethod resolve-source-link (source (page system-page))
  (if (pathname-utils:subpath-p (truename (getf source :file))
                                (truename (asdf:system-source-directory (system page))))
      (let ((homepage (asdf:system-homepage (system page))))
        (cond ((search "github" homepage)
               (format NIL "~a/blob/HEAD/~a~@[#L~a~]"
                       (github-project-root (asdf:system-homepage (system page)))
                       (enough-namestring (getf source :file)
                                          (asdf:system-source-directory (system page)))
                       (getf source :row)))
              ((search "gitlab" homepage)
               (format NIL "~a/-/blob/HEAD/~a~@[#L~a~]"
                       (gitlab-project-root (asdf:system-homepage (system page)))
                       (enough-namestring (getf source :file)
                                          (asdf:system-source-directory (system page)))
                       (getf source :row)))
              (T
               (call-next-method))))
      (call-next-method)))
