#|
 This file is a part of Staple
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.staple)

(define-source-compiler (:markdown "md") (input)
  (let ((3bmd-code-blocks:*code-blocks* T)
        #.(if (find-symbol "*GENERATE-HEADER-IDS*" "3BMD")
              `(3bmd:*generate-header-ids* T)
              (gensym "Stub")))
    (with-output-to-string (out)
      (3bmd:parse-string-and-print-to-stream
       input out))))
