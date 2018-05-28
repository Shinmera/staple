#|
 This file is a part of Staple
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.staple.server)

(defvar *acceptor* NIL)

(defun all-systems ()
  (sort (asdf/system-registry:registered-systems*) #'string< :key #'asdf:component-name))

(defun data-file (path)
  (asdf:system-relative-pathname :staple-server (format NIL "data/~a" path)))

(defun system-link (system)
  (format NIL "/~a/" (asdf:component-name system)))

(defclass acceptor (hunchentoot:acceptor)
  ()
  (:default-initargs
   :port 5123
   :message-log-destination NIL
   :access-log-destination NIL))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor acceptor) request)
  (let* ((path (subseq (hunchentoot:url-decode (hunchentoot:script-name request)) 1))
         (pos (position #\/ path)))
    (handler-case
        (cond
          ((string= path "")
           (serve-system-list))
          (pos
           (let ((sys (subseq path 0 pos))
                 (path (subseq path (1+ pos))))
             (serve-system-docs sys path)))
          (T
           (hunchentoot:handle-static-file (data-file path))))
      (error (err)
        (serve-error err)))))

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

(defun serve-system-docs (sys path))

(defun serve-error (err)
  (princ-to-string err))
