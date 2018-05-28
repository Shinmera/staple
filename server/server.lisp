#|
 This file is a part of Staple
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.staple.server)

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

(defun prefix-p (prefix string)
  (and (<= (length prefix) (length string))
       (string-equal prefix string :end2 (length prefix))))

(defun suffix-p (suffix string)
  (and (<= (length suffix) (length string))
       (string-equal suffix string :start2 (- (length string) (length suffix)))))

(defun find-system-in-path (path)
  (let ((systems ()))
    (asdf:map-systems
     (lambda (sys)
       (when (prefix-p (asdf:component-name sys) path)
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
            ((prefix-p "file/" path)
             (serve-file (subseq path 4)))
            (system
             (serve-system-docs system (subseq path (length (asdf:component-name system)))))
            (T
             (hunchentoot:handle-static-file (data-file path)))))
      (use-value (value &optional error)
        (declare (ignore error))
        value))))

(defun start ()
  (when *acceptor*
    (error "Server already running!"))
  (let ((acceptor (make-instance 'acceptor)))
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

(defun serve-system-docs (system path)
  (let* ((dir (system-path system))
         (path (if (string= "" path) "" (subseq path 1)))
         (path (if (string= "" path) "index.html" path))
         (path (merge-pathnames dir path)))
    (when (or* (not (uiop:directory-exists-p dir))
               (hunchentoot:get-parameter "rebuild")) 
      (ensure-directories-exist dir)
      (staple::generate system :if-exists :supersede
                               :output-directory dir))
    (if (string= "html" (pathname-type path))
        (let ((document (plump:parse path)))
          (lquery:$ document "[href^=file://]"
            (each (lambda (el)
                    (setf (plump:attribute el "href")
                          (format NIL "/file~a" (subseq (plump:attribute el "href")
                                                        (length "file://")))))))
          (lquery:$ document "body" (append (lquery:$ (initialize (data-file "nav.ctml")) "nav")))
          (plump:serialize document NIL))
        (hunchentoot:handle-static-file path))))

(defun serve-file (path)
  (plump:serialize
   (clip:process (data-file "file.ctml")
                 :path path)))

(defun serve-error (err)
  (plump:serialize
   (clip:process (data-file "error.ctml")
                 :env (dissect:capture-environment err))
   NIL))
