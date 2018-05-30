#|
 This file is a part of Staple
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:staple-code-parser
  (:nicknames #:org.shirakumo.staple.code-parser)
  (:use #:cl #:alexandria)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree))
  (:export
   #:parse
   #:parse-result->definition-list))
