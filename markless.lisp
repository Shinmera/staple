#|
 This file is a part of Staple
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.staple)

(define-source-compiler (:markless "mess") (input)
  (cl-markless:output (cl-markless:parse input T)
                      :target (plump-dom:make-root)
                      :format :plump))
