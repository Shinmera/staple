#|
 This file is a part of Staple
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.staple)

(defvar *page*)

(defclass page ()
  ((title :initarg :title :accessor title)
   (output :initarg :output :accessor output)
   (project :initarg :project :accessor project))
  (:default-initargs
   :output NIL
   :title NIL
   :project (error "PROJECT required.")))

(defgeneric generate (page &key if-exists &allow-other-keys))

(defmethod generate :around ((page page) &key)
  (let ((*page* page))
    (call-next-method)))

(defmethod generate :before ((page page) &key)
  (with-value-restart (output page)
    (unless (typep (output page) '(or string pathname))
      (error "The output file for ~a is not a pathname."
             page))))

(defclass input-page (page)
  ((input :initarg :input :accessor input))
  (:default-initargs
   :input NIL))

(defmethod generate :before ((page input-page) &key)
  (with-value-restart (input page)
    (unless (typep (input page) 'pathname)
      (error "The input file for ~a is not a pathname."
             page))
    (unless (probe-file (input page))
      (error "The input file for ~a does not exist:~%  ~s"
             page (input page)))))

(defclass static-page (input-page)
  ())

(defmethod generate ((page static-page) &key (if-exists :error))
  (with-open-file (out (output page) :direction :output
                                     :if-exists if-exists
                                     :element-type '(unsigned-byte 8))
    (with-open-file (in (input page) :direction :input
                                     :element-type '(unsigned-byte 8))
      (loop with buffer = (make-array 4096 :element-type '(unsigned-byte 8))
            for read = (read-sequence buffer in)
            while (< 0 read)
            do (write-sequence buffer out :end read)))))

(defclass compiled-page (input-page)
  ())

(defmethod generate ((page compiled-page) &key (if-exists :error) (compact T))
  (let ((data (compile-source (input page) (pathname-type (input page)))))
    (handler-bind ((plump:invalid-xml-character #'abort)
                   (plump:discouraged-xml-character #'continue))
      (etypecase data
        (plump:node
         (with-open-file (out (output page) :direction :output
                                            :if-exists if-exists)
           (when compact (compact data))
           (plump:serialize data out)))
        (string
         (with-open-file (out (output page) :direction :output
                                            :if-exists if-exists)
           (write-string data out)))
        ((vector (unsigned-byte 8))
         (with-open-file (out (output page) :direction :output
                                            :if-exists if-exists
                                            :element-type '(unsigned-byte 8))
           (write-sequence data out)))))))

(defclass templated-page (input-page)
  ())

(defgeneric template-data (project page)
  (:method-combination append :most-specific-first))

(defmethod template-data append (project (page templated-page))
  (list :title (title page)))

(defmethod generate ((page templated-page) &key (if-exists :error) (compact T))
  (with-open-file (out (output page) :direction :output
                                     :if-exists if-exists)
    (handler-bind ((plump:invalid-xml-character #'abort)
                   (plump:discouraged-xml-character #'continue))
      (let* ((*package* #.*package*)
             (node (apply #'clip:process
                          (plump:parse (input page))
                          (template-data (project page) page))))
        (when compact (compact node))
        (plump:serialize node out)))))

(defclass symbol-index-page (templated-page)
  ((packages :initform NIL :accessor packages)))

(defmethod shared-initialize :after ((page symbol-index-page) slots &key packages)
  (when packages (setf (packages page) packages)))

(defmethod (setf packages) :around (packages (page symbol-index-page))
  (call-next-method (ensure-package-defs packages) page))

(defmethod template-data append (project (page symbol-index-page))
  (list :packages (packages page)))

(defclass system-page (symbol-index-page)
  ((system :initarg NIL :accessor system)))

(defmethod shared-initialize :after ((page system-page) slots &key system)
  (when system (setf (system page) system))
  (unless (packages page)
    (setf (packages page) (system-packages (system page))))
  (unless (title page)
    (setf (title page) (asdf:component-name (system page)))))

(defmethod (setf system) :around (system (page system-page))
  (call-next-method (etypecase system
                      ((or string symbol) (asdf:find-system system T))
                      (asdf:system system))
                    page))

(defmethod template-data append (project (page system-page))
  (list :system (system page)))
