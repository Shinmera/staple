#|
 This file is a part of Staple
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.staple.code-parser)

(defclass client (eclector.concrete-syntax-tree::cst-client)
  ())

(defmethod eclector.reader:evaluate-expression ((client client) expression)
  (eval expression))

(defmethod walk-lambda-list (cst environment)
  (walk-lambda-list (cst:parse-ordinary-lambda-list T cst) environment))

(defmethod walk-lambda-list ((cst cst:ordinary-lambda-list) environment)
  (flet ((parameter (type)
           (when-let ((group (find-if (of-type type) (cst:children cst))))
             (walk (cst:parameter group) environment)))
         (parameters (type)
           (when-let ((group (find-if (of-type type) (cst:children cst))))
             (mapcar (rcurry #'walk environment) (cst:parameters group)))))
    (append (parameters 'cst:ordinary-required-parameter-group)
            (parameters 'cst:ordinary-optional-parameter-group)
            (parameter  'cst:ordinary-rest-parameter-group)
            (parameters 'cst:key-parameter-group))))

(defmethod walk ((cst cst:simple-variable) environment)
  (list (walk (cst:name cst) environment)))

(defmethod walk ((cst cst:ordinary-optional-parameter) environment)
  (list* (walk (cst:name cst) environment)
         (when-let ((supplied-p (cst:supplied-p cst)))
           (list (walk supplied-p environment)))))

(defmethod walk ((cst cst:ordinary-key-parameter) environment)
  (list* (walk (cst:name cst) environment)
         (when-let ((supplied-p (cst:supplied-p cst)))
           (list (walk supplied-p environment)))))

(defun walk-bindings (bindings environment)
  (loop until (cst:null bindings)
        for binding = (cst:first bindings)
        collect (etypecase binding
                  (cst:atom-cst
                   (cons (walk binding environment) NIL))
                  (cst:cons-cst
                   (cons (walk (cst:first binding) environment)
                         (etypecase (cst:rest binding)
                           (cst:atom-cst
                            (if (cst:null (cst:rest binding))
                                (walk (cst:rest binding) environment)
                                (error "~@<Invalid binding spec: ~s~@:>"
                                       (cst:raw binding))))
                           (cst:cons-cst
                            (walk (cst:second binding) environment))))))
        do (setf bindings (cst:rest bindings))))

(defun walk-implicit-progn (cst environment)
  (loop until (or (null cst) (cst:null cst))
        collect (walk (cst:first cst) environment)
        do (setf cst (cst:rest cst))))

(defun walk-body (cst environment)
  (multiple-value-bind (declarations forms)
      (cst:separate-ordinary-body cst :listify-body nil)
    (declare (ignore declarations))
    (walk-implicit-progn forms environment)))

(defun walk-lambda-like (cst environment)
  (cst:db source (operator-or-name lambda-list . body) cst
    (declare (ignore source operator-or-name))
    (let ((variables (walk-lambda-list lambda-list environment)))
      (multiple-value-bind (declarations documentation forms)
          (cst:separate-function-body body :listify-body nil)
        ;; FIXME: parse declarations
        (declare (ignore declarations documentation))
        `(:lambda ,(cst:source cst)
           ,variables
           ,@(walk-implicit-progn forms environment))))))

(defmethod walk ((cst cst:atom-cst) environment)
  (destructuring-bind (type . args) (walk-atom (cst:raw cst) environment)
    `(,type ,(cst:source cst)
            ,@args)))

(defmethod walk ((cst cst:cons-cst) environment)
  (walk-form (cst:raw (cst:first cst)) cst environment))

(defmethod walk-atom ((atom symbol) environment)
  (list (if (typep atom '(or keyword boolean))
            :literal
            :variable)
        atom))

(defmethod walk-atom (atom environment)
  (list :literal
        atom))

(defmacro define-walk-compound-form (operator (cst-var &optional (environment-var (gensym "ENVIRONMENT"))) &body body)
  (with-unique-names (operator-var)
    `(defmethod walk-form ((,operator-var ,(if (eql T operator) T `(eql ',operator)))
                           (,cst-var cst:cons-cst)
                           ,environment-var)
       (flet ((walk (cst &optional (environment ,environment-var))
                (walk cst environment))
              (walk-implicit-progn (cst &optional (environment ,environment-var))
                (walk-implicit-progn cst environment)))
         (declare (ignorable #'walk #'walk-implicit-progn))
         ,@body))))

(defmethod read-toplevel ((input string))
  (with-input-from-string (stream input)
    (read-toplevel stream)))

(defmethod read-toplevel ((input pathname))
  (with-open-file (stream input :direction :input)
    (read-toplevel stream)))

(defmethod read-toplevel ((input stream))
  (let ((eclector.reader:*client* (make-instance 'client)))
    (loop for top-level-form = (restart-case (eclector.parse-result:read input nil '#1=#.(make-symbol "EOF"))
                                 (continue ()
                                   :report "Skip the top-level form"
                                   nil))
          until (eq top-level-form '#1#)
          collect top-level-form)))

(defmethod parse (input)
  (loop with environment = (make-instance 'environment)
        for form in (read-toplevel input)
        collect (walk form environment)))
