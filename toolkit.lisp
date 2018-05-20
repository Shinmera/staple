#|
 This file is a part of Staple
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.staple)

(defvar *language-code-map* (make-hash-table :test 'equalp))

(defun load-language-codes (file table)
  (with-open-file (stream file)
    (loop for line = (read-line stream NIL)
          while line
          do (let* ((tab (position #\Tab line))
                    (code (subseq line 0 tab)))
               (when tab
                 (setf (gethash code table) code))))
    table))

(load-language-codes
 (asdf:system-relative-pathname :staple "data/iso-639-1.csv")
 *language-code-map*)

(load-language-codes
 (asdf:system-relative-pathname :staple "data/iso-639-3.csv")
 *language-code-map*)

(defun read-value ()
  (format *query-io* "~&> Enter a new value: ~%")
  (multiple-value-list (eval (read))))

(defmacro with-value-restart (place &body body)
  (let ((value (gensym "VALUE")))
    `(loop (restart-case
               (return
                 (progn ,@body))
             (set-value (,value)
               :report "Set a new value."
               :interactive read-value
               (setf ,place ,value))))))

(defun ensure-system (system-ish)
  (typecase system-ish
    (asdf:system system-ish)
    (T (asdf:find-system system-ish T))))

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

(defun map-directory-tree (function directory)
  (let ((to-scan (list directory)))
    (loop for directory = (pop to-scan)
          while directory
          do (dolist (file (directory (merge-pathnames "*.*" directory)))
               (funcall function file))
             (dolist (dir (directory (merge-pathnames "*/" directory)))
               (push dir to-scan)))))

(defmacro do-directory-tree ((file directory &optional result) &body body)
  `(progn (map-directory-tree (lambda (,file) ,@body) ,directory)
          ,result))

(defun read-file (path)
  (with-open-file (in path)
    (with-output-to-string (out)
      (loop with buffer = (make-array 4096 :element-type 'character)
            for read = (read-sequence buffer in)
            while (< 0 read)
            do (write-sequence buffer out)))))

(defgeneric definition-id (definition))

(defmethod definition-id ((definition definitions:package))
  (format NIL "~a-~a"
          (definitions:type definition)
          (definitions:name definition)))

(defmethod definition-id ((definition definitions:global-definition))
  (format NIL "~a-~a:~a"
          (definitions:type definition)
          (package-name (definitions:package definition))
          (definitions:name definition)))

(defgeneric definition-order (definition)
  (:method ((_ definitions:package))            200)
  (:method ((_ definitions:constant))           190)
  (:method ((_ definitions:symbol-macro))       180)
  (:method ((_ definitions:special-variable))   170)
  (:method ((_ definitions:variable))           160)
  (:method ((_ definitions:class))              150)
  (:method ((_ definitions:condition))          140)
  (:method ((_ definitions:structure))          130)
  (:method ((_ definitions:type-definition))    120)
  (:method ((_ definitions:type))               110)
  ;;(:method ((_ definitions:accessor))           100)
  (:method ((_ definitions:function))           90)
  (:method ((_ definitions:generic-function))   80)
  (:method ((_ definitions:method))             70)
  (:method ((_ definitions:compiler-macro))     60)
  (:method ((_ definitions:macro))              50)
  (:method ((_ definitions:setf-expander))      40)
  (:method ((_ definitions:callable))           30)
  (:method ((_ definitions:method-combination)) 20)
  (:method ((_ definitions:global-definition))  10)
  (:method ((_ definitions:definition))         0))

(defun sort-definitions (definitions)
  (flet ((sorter (a b)
           (if (eql (type-of a) (type-of b))
               (string> (definitions:name a)
                        (definitions:name b))
               (> (definition-order a)
                  (definition-order b)))))
    (stable-sort definitions #'sorter)))

(defgeneric definition-importance (definition)
  (:method ((_ definitions:callable)) 30)
  (:method ((_ definitions:type)) 20)
  (:method ((_ definitions:variable)) 10)
  (:method ((_ definitions:definition)) 0)
  (:method ((_ definitions:method)) -10))

(defun preferred-definition (definitions)
  (first (stable-sort definitions #'> :key #'definition-importance)))

(defun url-encode (thing &optional (external-format :utf-8))
  (with-output-to-string (out)
    (loop for octet across (babel:string-to-octets thing :encoding external-format)
          for char = (code-char octet)
          do (cond ((or (char<= #\0 char #\9)
                        (char<= #\a char #\z)
                        (char<= #\A char #\Z)
                        (find char "-._~" :test #'char=))
                    (write-char char out))
                   (T (format out "%~2,'0x" (char-code char)))))))

(defun ensure-package-defs (packages)
  (loop for package in packages
        collect (etypecase package
                  (string (make-instance 'definitions:package :designator package))
                  (symbol (make-instance 'definitions:package :designator (symbol-name package)))
                  (package (make-instance 'definitions:package :designator (package-name package)))
                  (definitions:package package))))
