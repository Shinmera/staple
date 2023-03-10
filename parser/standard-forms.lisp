#|
 This file is a part of Staple
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.staple.code-parser)

(define-walk-compound-form T (cst environment)
  (cst:db source (operator . arguments) cst
    (if (cst:atom operator)
        (if-let ((expander (ignore-errors (macro-function (cst:raw operator)))))
          (handler-case
              (let ((expansion (perform-and-record-macro-expansion expander cst)))
                `(:macro ,source
                         ,(walk operator)
                         ,(walk expansion)))
            (error ()
              ;; Just.. bail. Not ideal, could try stuff like walking body forms
              ;; and skipping expansion. Maybe.
              `(:macro ,source
                       ,(walk operator)
                       ,(cst:raw cst))))
          `(:call ,source
                  ,(walk operator)
                  ,@(mapcar #'walk (cst:listify arguments))))
        (error "~@<Non-atom operator not yet implemented: ~S~:@>" cst))))

(defun perform-and-record-macro-expansion (expander cst)
  (let* ((expansion/raw (funcall expander (cst:raw cst) nil))
         (reconstructed (cst:reconstruct T expansion/raw cst)))
    (labels ((record (node source-and-targets)
               (push node (cdr source-and-targets))
               (when (cst:consp node)
                 (record (cst:first node) source-and-targets)
                 (record (cst:rest node)  source-and-targets))))
      (record reconstructed (cons cst '()))
      reconstructed)))

;; FIXME: Type walker required
;; (define-walk-compound-form typep (cst environment)
;;   (cst:db source (operator form type . env) cst
;;     (declare (ignore env))
;;     `(:call ,source
;;             ,(walk operator)
;;             ,(walk form)
;;             )))

(defun maybe-unquote (thing)
  (if (and (consp thing) (eql 'quote (first thing)))
      (second thing)
      thing))

(define-walk-compound-form make-instance (cst environment)
  (cst:db source (operator type . args) cst
    `(progn ,source
            (:call ,(cons (car source) (cdr (cst:source operator)))
                   (:function ,(cst:source operator) ,(cst:raw operator))
                   (:type ,(cst:source type) ,(maybe-unquote (cst:raw type)))
                   ,@(mapcar #'walk (cst:listify args))))))

(define-walk-compound-form defclass (cst environment)
  (cst:db source (operator name superclasses slots . options) cst
    `(:macro ,source
             ,(walk operator)
             (progn ,source
                    ,@(loop for class in (cst:listify superclasses)
                            collect `(:type ,(cst:source class) ,(cst:raw class)))))))

;; Try handling defmethod directly to avoid lengthy macroexpansion and to
;; handle inference of class types.
(define-walk-compound-form defmethod (cst environment)
  (cst:db source (operator name . args) cst
    (let ((qualifiers (loop for item = (cst:first args)
                            while (and (cst:atom item) (not (cst:null item)))
                            collect item
                            do (setf args (cst:rest args))))
          (lambda-list (cst:first args))
          (body (cst:rest args)))
      (declare (ignore qualifiers))
      (multiple-value-bind (declarations documentation forms)
          (cst:separate-function-body body)
        (declare (ignore declarations documentation))
        `(:macro ,source
                 ,(walk operator)
                 (lambda ,source
                   ,(walk (cst:parse-specialized-lambda-list T lambda-list) environment)
                   (function ,(cst:source name) ,(cst:raw name))
                   ,@(walk-implicit-progn forms environment)))))))

;; Try handling the distinction between setf functions and setf-expanders.
(define-walk-compound-form setf (cst environment)
  (cst:db source (operator . pairs) cst
    (flet ((handle-place (place value)
             (cond ((cst:atom place)
                    `(setq ,source (,(walk place)) (,(walk value))))
                   ((fboundp `(setf ,(cst:raw (cst:first place))))
                    `(:call ,(cons (car source) (cdr (cst:source value)))
                            (:function ,(cst:source (cst:first place))
                                       (setf ,(cst:raw (cst:first place))))
                            ,(walk value)
                            ,@(mapcar #'walk (cst:listify (cst:rest place)))))
                   (T
                    ;; Not sure how to check for setf-expanders or expand to
                    ;; them, so we just do a basic macro expansion to at least
                    ;; potentially get information about the arguments.
                    (let ((expansion (perform-and-record-macro-expansion
                                      (macro-function 'setf)
                                      (cst:list operator place value))))
                      `(:macro ,source
                               ,(walk operator)
                               ,(walk expansion)))))))
      (let ((pairs (cst:listify pairs)))
        `(progn ,source
                ,@(loop for (place value) on pairs by #'cddr
                        collect (handle-place place value)))))))

(define-walk-compound-form loop (cst environment)
  (cst:db source (operator . args) cst
    `(:macro ,source
             ,(walk operator)
             (lambda ,source
               ()
               ,@(walk-implicit-progn args environment)))))

;; Transform literals in funcalls' function arguments to functions.
(flet ((walk-funcallish (cst environment)
         (cst:db source (operator function . arguments) cst
           (let ((function (walk function environment)))
             `(:call ,source
                     ,(walk operator environment)
                     ,(if (eql :literal (first function))
                          `(function ,@(rest function))
                          function)
                     ,@(loop for argument in (cst:listify arguments)
                             collect (walk argument environment)))))))
  (define-walk-compound-form funcall (cst environment)
    (walk-funcallish cst environment))

  (define-walk-compound-form apply (cst environment)
    (walk-funcallish cst environment)))
