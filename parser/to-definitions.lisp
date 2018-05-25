#|
 This file is a part of Staple
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.staple.code-parser)

(defmethod find-definitions (type args))

(defmacro define-definition-resolver (type args &body body)
  (let ((argsg (gensym "ARGS")))
    `(defmethod find-definitions ((,(gensym "TYPE") (eql ,type)) ,argsg)
       (destructuring-bind ,args ,argsg
         ,@body))))

(define-definition-resolver :call (name &rest arguments)
  (declare (ignore arguments))
  (definitions:find-definitions (third name) :type 'definitions:function))

(define-definition-resolver :macro (name expansion)
  (declare (ignore expansion))
  (definitions:find-definitions (third name) :type 'definitions:macro))

(define-definition-resolver :variable (name)
  (definitions:find-definitions name :type 'definitions:variable))

(define-definition-resolver :type (name)
  (definitions:find-definitions name :type 'definitions:type))

(defmethod sub-results (type args))

(defmacro define-sub-results (type args &body body)
  (let ((argsg (gensym "ARGS")))
    `(defmethod sub-results ((,(gensym "TYPE") (eql ,type)) ,argsg)
       (destructuring-bind ,args ,argsg
         ,@body))))

(define-sub-results :eval-when (&rest args)
  args)

(define-sub-results :the (type form)
  (list type form))

(define-sub-results :locally (&rest forms)
  forms)

(define-sub-results :lambda (variables &rest forms)
  (append (loop for var in variables append var) forms))

(define-sub-results :named-lambda (name variables &rest forms)
  (append (list name) (loop for var in variables append var) forms))

(define-sub-results :labels (names values &rest forms)
  (append names values forms))

(define-sub-results :flet (names definitions &rest forms)
  (append names definitions forms))

(define-sub-results :macrolet (names definitions &rest forms)
  (append names definitions forms))

(define-sub-results :symbol-macrolet (names expansions &rest forms)
  (append names expansions forms))

(define-sub-results :if (test then &optional else)
  (if else
      (list test then else)
      (list test then)))

(define-sub-results :let (names values &rest forms)
  (append names values forms))

(define-sub-results :let* (names values &rest forms)
  (append names values forms))

(define-sub-results :block (name &rest forms)
  (list* name forms))

(define-sub-results :return-from (name value)
  (list name value))

(define-sub-results :tagbody (labels abstractions)
  (append labels abstractions))

(define-sub-results :go (label)
  (list label))

(define-sub-results :progn (&rest forms)
  forms)

(define-sub-results :macro (name expansion)
  (list name expansion))

(define-sub-results :call (name &rest arguments)
  (list* name arguments))

(defun parse-result-to-definition-list (result)
  (let ((results ()))
    (labels ((traverse (type location &rest args)
               (when location
                 (dolist (def (find-definitions type args))
                   ;; Only keep results for different locations or different types.
                   (unless (loop for (pdef ploc) in results
                                 thereis (and (equal ploc location)
                                              (eql (type-of def) (type-of pdef))))
                     (pushnew (list def location) results :key #'second :test #'equal))))
               (loop for form in (sub-results type args)
                     do (apply #'traverse form))))
      (loop for form in result
            do (apply #'traverse form))
      results)))
