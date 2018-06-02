#|
 This file is a part of Staple
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.staple.code-parser)

(defmethod find-definitions (type source args))

(defmacro define-definition-resolver (type (source &rest args) &body body)
  (let ((argsg (gensym "ARGS")))
    `(defmethod find-definitions ((,(gensym "TYPE") (eql ',type)) ,source ,argsg)
       (destructuring-bind ,args ,argsg
         ,@body))))

(defun tie-to-source (source defs)
  (loop for def in defs collect (list def source)))

(define-definition-resolver :call (source name &rest arguments)
  (declare (ignore arguments))
  (tie-to-source (second name)
                 (definitions:find-definitions (third name) :type 'definitions:function)))

(define-definition-resolver :macro (source name expansion)
  (declare (ignore expansion))
  (tie-to-source (second name)
                 (definitions:find-definitions (third name) :type 'definitions:macro)))

(define-definition-resolver :variable (source name)
  (tie-to-source source
                 (definitions:find-definitions name :type 'definitions:variable)))

(define-definition-resolver type (source name)
  (tie-to-source source
                 (definitions:find-definitions name :type 'definitions:type)))

(define-definition-resolver function (source name)
  (tie-to-source source
                 (definitions:find-definitions name :type 'definitions:function)))

(defmethod sub-results (type args))

(defmacro define-sub-results (type args &body body)
  (let ((argsg (gensym "ARGS")))
    `(defmethod sub-results ((,(gensym "TYPE") (eql ',type)) ,argsg)
       (destructuring-bind ,args ,argsg
         ,@body))))

(define-sub-results block (name &rest forms)
  (list* name forms))

(define-sub-results catch (tag &rest forms)
  (list* tag forms))

(define-sub-results eval-when (&rest args)
  args)

(define-sub-results flet (names definitions &rest forms)
  (append names definitions forms))

;; function

(define-sub-results lambda (variables &rest forms)
  (append (loop for var in variables append var) forms))

#+sbcl
(define-sub-results sb-int:named-lambda (name variables &rest forms)
  (append (list name) (loop for var in variables append var) forms))

(define-sub-results go (label)
  (list label))

(define-sub-results if (test then &optional else)
  (if else
      (list test then else)
      (list test then)))

(define-sub-results labels (names values &rest forms)
  (append names values forms))

(define-sub-results let (names values &rest forms)
  (append names values forms))

(define-sub-results let* (names values &rest forms)
  (append names values forms))

(define-sub-results load-time-value (form)
  (list form))

(define-sub-results locally (&rest forms)
  forms)

(define-sub-results macrolet (names definitions &rest forms)
  (append names definitions forms))

(define-sub-results multiple-value-call (function &rest arguments)
  (list* function arguments))

(define-sub-results multiple-value-prog1 (form &rest body)
  (list* form body))

(define-sub-results progn (&rest forms)
  forms)

(define-sub-results progv (symbols values &rest forms)
  (list* symbols values forms))

(define-sub-results return-from (name value)
  (list name value))

(define-sub-results setq (places values)
  (append places values))

(define-sub-results symbol-macrolet (names expansions &rest forms)
  (append names expansions forms))

(define-sub-results tagbody (labels abstractions)
  (append labels abstractions))

(define-sub-results the (type form)
  (list type form))

(define-sub-results throw (tag results)
  (list tag results))

(define-sub-results unwind-protect (protected &rest cleanup)
  (list* protected cleanup))

(define-sub-results :macro (name expansion)
  (list name expansion))

(define-sub-results :call (name &rest arguments)
  (list* name arguments))

(defun parse-result->definition-list (result)
  (let ((results ()))
    (labels ((traverse (type location &rest args)
               (when location
                 (dolist (def (find-definitions type location args))
                   ;; Only keep results for different locations or different types.
                   (unless (or (null (second def))
                               (loop for (pdef ploc) in results
                                     thereis (and (equal (second def) ploc)
                                                  (eql (type-of (first def)) (type-of pdef)))))
                     (pushnew def results :key #'second :test #'equal))))
               (loop for form in (sub-results type args)
                     do (apply #'traverse form))))
      (loop for form in result
            do (apply #'traverse form))
      results)))
