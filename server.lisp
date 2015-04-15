#|
 This file is a part of Staple
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:staple-server
  (:nicknames #:org.tymoonnext.staple.server)
  (:use #:cl)
  (:export
   #:start
   #:stop))
(in-package #:staple-server)

(defvar *acceptor* NIL)
(defvar *cache* (make-hash-table :test 'equalp))

(defun start ()
  (when *acceptor*
    (error "Server already running!"))
  (let ((acceptor (make-instance 'hunchentoot:easy-acceptor
                                 :port 8080
                                 :message-log-destination NIL
                                 :access-log-destination NIL)))
    (hunchentoot:start acceptor)
    (setf *acceptor* acceptor)))

(defun stop ()
  (unless *acceptor*
    (error "Server is not running!"))
  (hunchentoot:stop *acceptor*)
  (setf *acceptor* NIL))

(defun split (char string &key (start 0) (end (length string)))
  (loop with result = ()
        with buffer = (make-string-output-stream)
        for i from start below end
        for item = (elt string i)
        do (cond ((char= char item)
                  (push (get-output-stream-string buffer) result)
                  (setf buffer (make-string-output-stream)))
                 (T
                  (write-char item buffer)))
        finally (progn
                  (push (get-output-stream-string buffer) result)
                  (return (nreverse result)))))

(defun show-system-list ()
  (plump:serialize
   (clip:process (asdf:system-relative-pathname :staple-server "server.ctml")
                 :systems (let ((systems ()))
                            (asdf:map-systems (lambda (sys) (push sys systems)))
                            (sort systems #'string< :key #'asdf:component-name)))
   NIL))

(defun system-url (name)
  (format NIL "/~a/" name))

(defun find-package* (name)
  (or (find-package name)
      (find-package (string-upcase name))))

(defun starts-with-p (start string &key (test 'eql))
  (and (<= (length start) (length string))
       (loop for a across start
             for b across string
             always (funcall test a b))))

(defun smart-find-packages (name)
  (delete-if-not
   (lambda (name)
     (block NIL
       (do-external-symbols (symbol (find-package* name))
         (when (eql (find-package* name) (symbol-package symbol))
           (return T)))))
   (delete-duplicates
    (cons name
          (loop for package in (list-all-packages)
                when (starts-with-p name (package-name package) :test #'char-equal)
                collect (package-name package)))
    :test #'string-equal)))

(defun show-system (name)
  (handler-bind ((error #'invoke-debugger))
    (or (gethash name *cache*)
        (setf (gethash name *cache*)
              (staple:generate name :packages (smart-find-packages name) :out NIL)))))

(progn
  (defun handler (request)
    (let* ((path (hunchentoot:url-decode (hunchentoot:script-name request)))
           (dirs (split #\/ path :start 1)))
      (cond
        ((and (null (cdr dirs))
              (string= (car dirs) ""))
         (lambda ()
           (show-system-list)))
        ((string= (car (last dirs)) "")
         (lambda ()
           (show-system (format NIL "~{~a~^/~}" (butlast dirs)))))
        (T
         (lambda ()
           (hunchentoot:handle-static-file
            (asdf:system-relative-pathname
             (format NIL "~{~a~^/~}" (butlast dirs))
             (car (last dirs)))))))))
  (setf hunchentoot:*dispatch-table* (list #'handler)))
