(in-package #:org.shirakumo.staple.server)

(defvar *server-build* NIL)
(defvar *acceptor* NIL)
(defvar *tmpdir* (merge-pathnames "staple-server/" (uiop:temporary-directory)))

(defun all-systems ()
  (sort (asdf/system-registry:registered-systems*) #'string< :key #'asdf:component-name))

(defun data-file (path)
  (asdf:system-relative-pathname :staple-server (format NIL "data/~a" path)))

(defun system-link (system)
  (format NIL "/~a/" (asdf:component-name system)))

(defun system-path (system)
  (merge-pathnames (make-pathname :directory `(:relative ,(asdf:component-name system)))
                   *tmpdir*))

(defun find-system-in-path (path)
  (let ((systems ()))
    (asdf:map-systems
     (lambda (sys)
       (when (staple::prefix-p (asdf:component-name sys) path)
         (push sys systems))))
    (first (sort systems #'> :key (lambda (s) (length (asdf:component-name s)))))))

(defun safe-prin1 (thing)
  (or (ignore-errors (prin1-to-string thing))
      "<error during printing>"))

(defmacro or* (&rest vals)
  (let ((arg (gensym "ARG")))
    `(or ,@(loop for val in vals
                 collect `(let ((,arg ,val))
                            (if (stringp ,arg)
                                (unless (string= ,arg "") ,arg)
                                ,arg))))))

(defun cache-system (system &optional dir)
  (unless (typep system 'asdf:system) (setf system (asdf:find-system system T)))
  (unless dir (setf dir (system-path system)))
  (format T "~& > Generating cache for ~a." (asdf:component-name system))
  (ensure-directories-exist dir)
  (let ((*server-build* T))
    (staple:generate system :if-exists :supersede
                            :output-directory dir)
    ;; Modify HTML files to work better in the server environment.
    (staple::do-directory-tree (file dir)
      (when (find (pathname-type file) '("html" "htm" "xhtml") :test #'string-equal)
        (let ((document (plump:parse file)))
          (lquery:$ document "a[href^=file://]"
            (each (lambda (el)
                    (setf (plump:attribute el "href")
                          (format NIL "/source~a" (subseq (plump:attribute el "href")
                                                          (length "file://")))))))
          (lquery:$ document "[src^=file://]"
            (each (lambda (el)
                    (setf (plump:attribute el "src")
                          (format NIL "/file~a" (subseq (plump:attribute el "src")
                                                        (length "file://")))))))
          (lquery:$ document "body" (append (lquery:$ (initialize (data-file "nav.ctml")) "nav")))
          (lquery:$ document (write-to-file file)))))))

(defun clear-cache (&optional system)
  (uiop:delete-directory-tree
   (if system
       (system-path system)
       *tmpdir*)
   :validate (lambda (p) (uiop:subpathp p *tmpdir*))))

(defclass acceptor (hunchentoot:acceptor)
  ()
  (:default-initargs
   :port 5123
   :message-log-destination NIL
   :access-log-destination NIL))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor acceptor) request)
  (let* ((path (subseq (hunchentoot:url-decode (hunchentoot:script-name request)) 1))
         (system (find-system-in-path path)))
    (restart-case
        (handler-bind
            ((error (lambda (e)
                      (dissect:with-capped-stack ()
                        (use-value (serve-error e) e)))))
          (cond
            ((string= path "")
             (serve-system-list))
            ((staple::prefix-p "file/" path)
             (hunchentoot:handle-static-file (subseq path 4)))
            ((staple::prefix-p "source/" path)
             (serve-source (subseq path 6)))
            (system
             (serve-system-docs system (subseq path (length (asdf:component-name system)))))
            (T
             (hunchentoot:handle-static-file (data-file path)))))
      (use-value (value &optional error)
        (declare (ignore error))
        value))))

(defun start (&key (port 5123))
  (when *acceptor*
    (error "Server already running!"))
  (let ((acceptor (make-instance 'acceptor :port port)))
    (hunchentoot:start acceptor)
    (setf *acceptor* acceptor)
    (format T "~&Your documentation browser is now running on http://localhost:~a/~%"
            (hunchentoot:acceptor-port acceptor))))

(defun stop ()
  (unless *acceptor*
    (error "Server is not running!"))
  (hunchentoot:stop *acceptor*)
  (setf *acceptor* NIL))

(defun serve-system-list ()
  (plump:serialize
   (clip:process (data-file "list.ctml")
                 :systems (all-systems))
   NIL))

(defmacro with-error-unwind (form &body body)
  (let ((completed (gensym "COMPLETED")))
    `(let ((,completed NIL))
       (unwind-protect (multiple-value-prog1 ,form
                         (setf ,completed T))
         (unless ,completed
           ,@body)))))

(defun serve-system-docs (system path)
  (let* ((dir (system-path system))
         (path (if (string= "" path) "" (subseq path 1)))
         (path (if (string= "" path) "index.html" path))
         (path (merge-pathnames dir path)))
    (when (or* (not (uiop:directory-exists-p dir))
               (hunchentoot:get-parameter "rebuild"))
      (with-error-unwind (cache-system system dir)
        (clear-cache system)))
    (hunchentoot:handle-static-file path)))

(defun serve-source (path)
  (plump:serialize
   (clip:process (data-file "file.ctml")
                 :path path)
   NIL))

(defun serve-error (err)
  (plump:serialize
   (clip:process (data-file "error.ctml")
                 :env (dissect:capture-environment err))
   NIL))

(staple:define-xref-resolver server (definition)
  (when *server-build*
    (let ((sys (loop for sys in (all-systems)
                     for packages = (staple:packages sys)
                     do (when (find (definitions:package definition) packages)
                          (return sys)))))
      (when sys
        (format NIL "/~a/#~a"
                (staple:url-encode (asdf:component-name sys))
                (staple:url-encode (staple:definition-id definition)))))))
