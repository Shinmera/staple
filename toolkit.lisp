#|
 This file is a part of Staple
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.staple)

(defun read-value ()
  (format *query-io* "~&> Enter a new value: ~%")
  (multiple-value-list (eval (read))))

(defun with-value-restart (place &body body)
  (let ((value (gensym "VALUE")))
    `(loop (restart-case
               (return
                 (progn ,@body))
             (set-value (,value)
               :report "Set a new value."
               :interactive read-value
               (setf ,place ,value))))))

(defmethod system-name ((system asdf:system))
  (intern (string-upcase (asdf:component-name system)) "KEYWORD"))

(defmethod system-name (name)
  (system-name (asdf:find-system name T)))

(defun compact (node)
  (typecase node
    (plump:text-node
     (setf (plump:text node) (cl-ppcre:regex-replace-all "(^\\s+)|(\\s+$)" (plump:text node) " ")))
    (plump:element
     (unless (string-equal "pre" (plump:tag-name node))
       (loop for child across (plump:children node)
             do (compact child))))
    (plump:nesting-node
     (loop for child across (plump:children node)
           do (compact child))))
  node)

(defmacro case* (test value &body clauses)
  (let ((valg (gensym "VALUE")))
    `(let ((,valg ,value))
       (cond ,@(loop for (clause-value . body) in clauses
                     collect (cond ((listp clause-value)
                                    `((or ,@(loop for v in clause-value
                                                  collect `(,test ,valg ',v)))
                                      ,@body))
                                   ((find clause-value '(T otherwise))
                                    `(T ,@body))
                                   (T
                                    `((,test ,valg ',clause-value)
                                      ,@body))))))))
