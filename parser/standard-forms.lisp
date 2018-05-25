#|
 This file is a part of Staple
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.staple.code-parser)

(define-walk-compound-form T (cst environment)
  (cst:db source (operator . arguments) cst
    (declare (ignore source))
    (if (cst:atom operator)
        (if-let ((expander (macro-function (cst:raw operator))))
          (let ((expansion (perform-and-record-macro-expansion expander cst)))
            `(:macro
              ,(cst:source cst)
              ,(walk operator)
              ,(walk expansion)))
          `(:call
            ,(cst:source cst)
            ,(walk operator)
            ,@(mapcar #'walk (cst:listify arguments))))
        (error "~@<Non-atom operator not yet implemented: ~S~:@>" cst))))

(defun perform-and-record-macro-expansion (expander cst)
  (let* ((expansion/raw (funcall expander (cst:raw cst) nil))
         (reconstructed (cst:reconstruct expansion/raw cst t)))
    (labels ((record (node source-and-targets)
               (push node (cdr source-and-targets))
               (when (cst:consp node)
                 (record (cst:first node) source-and-targets)
                 (record (cst:rest node)  source-and-targets))))
      (record reconstructed (cons cst '()))
      reconstructed)))
