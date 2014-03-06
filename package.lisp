#|
 This file is a part of Staple
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:staple
  (:nicknames #:org.tymoonnext.staple)
  (:use #:cl #:lquery #:alexandria #:closer-mop #:split-sequence)
  (:shadowing-import-from
   #:cl #:defmethod #:defgeneric #:standard-generic-function)
  (:export
   #:generate))
