#|
 This file is a part of Staple
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.staple)

(defclass page ()
  ((output :initarg :output :accessor output)
   (project :initarg :project :accessor project))
  (:default-initargs
   :output NIL
   :project (error "PROJECT required.")))

(defgeneric generate (page &key if-exists &allow-other-keys))

(defmethod generate :before ((page page) &key)
  (with-value-restart (output page)
    (unless (typep (output page) 'pathname)
      (error "The output file for ~a is not a pathname."
             page))))

(defclass input-page ()
  ((input :accessor input)))

(defmethod generate :before ((page input-page) &key)
  (with-value-restart (input page)
    (unless (typep (input page) 'pathname)
      (error "The input file for ~a is not a pathname."
             page))
    (unless (uiop:directory-exists-p (input page))
      (error "The input file for ~a does not exist:~%  ~s"
             page (input page)))))

(defclass static-page (input-page)
  ())

(defmethod generate ((page static-page) &key (if-exists :error))
  (with-open-file (out (output page) :if-exists if-exists :element-type '(unsigned-byte 8))
    (with-open-file (in (input page) :element-type '(unsigned-byte 8))
      (loop with buffer = (make-array 4096 :element-type '(unsigned-byte 8))
            for read = (read-sequence buffer in)
            while (< 0 read)
            do (write-sequence buffer out :end read)))))

(defclass compiled-page (input-page)
  ())

(defmethod generate ((page compiled-page) &key (if-exists :error) (compact T))
  (let ((data (compile-source (input page) (pathname-type (input page)))))
    (etypecase data
      (plump:node
       (with-open-file (out (output page) :if-exists if-exists)
         (when compact (compact data))
         (plump:serialize data out)))
      (string
       (with-open-file (out (output page) :if-exists if-exists)
         (write-string data out)))
      ((vector (unsigned-byte 8))
       (with-open-file (out (output page) :if-exists if-exists :element-type '(unsigned-byte 8))
         (write-sequence data out))))))

(defclass templated-page (input-page)
  ())

(defgeneric template-data (project page))

(defmethod generate ((page templated-page) &key (if-exists :error) (compact T))
  (with-open-file (out (output page) :if-exists if-exists)
    (let ((node (apply #'clip:process
                       (plump:parse (input page))
                       (template-data (project page) page))))
      (when compact (compact node))
      (plump:serialize node out))))
