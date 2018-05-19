#|
 This file is a part of Staple
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:staple
  (:nicknames #:org.shirakumo.staple)
  (:use #:cl #:org.shirakumo.staple.recording)
  ;; recording.lisp
  (:export
   #:system-packages))
