#|
 This file is a part of Staple
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.staple)

(defun to-out (pathname)
  (merge-pathnames (format NIL "~a.out.~a" (pathname-name pathname) (pathname-type pathname)) pathname))

(defun staple (in &key (out (to-out in)) (if-exists :supersede))
  (let ((*package* (find-package "STAPLE"))
        (document (plump:parse in)))
    (let ((document (clip:process document)))
      (with-open-file (stream out :direction :output :if-exists if-exists)
        (plump:serialize document stream)))
    out))
