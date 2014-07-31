#|
 This file is a part of Staple
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.staple)

(defun anchor (object)
  (format NIL "#~a" object))

(defun stext (node object)
  (lquery-funcs:text node (princ-to-string (or object ""))))

(define-tag-processor documentate (node)
  )

(defmethod clip ((component asdf:component) field)
  (unless (symbolp field)
    (setf field (find-symbol (string field) :STAPLE)))
  (case field
    (name (asdf:component-name component))
    (parent (asdf:component-parent component))
    (system (asdf:component-system component))
    (version (asdf:component-version component))
    (children (asdf:component-children component))
    (encoding (asdf:component-encoding component))
    (loaded-p (asdf:component-loaded-p component))
    (pathname (asdf:component-pathname component))
    (relative-pathname(asdf:component-relative-pathname component))
    (find-path (asdf:component-find-path component))
    (external-format (asdf:component-external-format component))
    (children-by-name (asdf:component-children-by-name component))
    (load-dependencies (asdf:component-load-dependencies component))
    (sideway-dependencies (asdf:component-sideway-dependencies component))
    (T (call-next-method))))

(defmethod clip ((system asdf:system) field)
  (unless (symbolp field)
    (setf field (find-symbol (string field) :STAPLE)))
  (case field
    (author (asdf:system-author system))
    (mailto (asdf:system-mailto system))
    (licence (asdf:system-licence system))
    (license (asdf:system-license system))
    (homepage (asdf:system-homepage system))
    (long-name (asdf:system-long-name system))
    (maintainer (asdf:system-maintainer system))
    (bug-tracker (asdf:system-bug-tracker system))
    (description (asdf:system-description system))
    (source-file (asdf:system-source-file system))
    (registered-p (asdf:system-registered-p system))
    (source-control (asdf:system-source-control system))
    (long-description (asdf:system-long-description system))
    (source-directory (asdf:system-source-directory system))
    (definition-pathname (asdf:system-definition-pathname system))
    (T (call-next-method))))

(define-attribute-processor asdf (node value)
  (setf *clipboard* (asdf:find-system value))
  (plump:remove-attribute node "asdf"))

(define-attribute-processor package (node value)
  (let ((package (find-package (resolve-value (read-from-string value)))))
    (when package
      (setf *clipboard* (make-clipboard `(name ,(package-name package) package ,package)))))
  (plump:remove-attribute node "package"))

(defmethod clip ((symb symb-object) field)
  (case field
    (full-name (format NIL "~a:~a" (package-name (symb-package symb)) (symb-name symb)))
    (name (symb-name symb))
    (type (symb-type symb))
    (scope (symb-scope symb))
    (qualifiers (symb-qualifiers symb))
    (arguments (symb-arguments symb))
    (documentation (symb-documentation symb))))

(define-attribute-processor symbols (node value)
  (let ((package (resolve-value (read-from-string value))))
    (setf (clipboard 'symbols)
          (package-symbol-objects package))
    (process-attribute :iterate "symbols"))
  (plump:remove-attribute node "symbols"))

(define-tag-processor do-symbols (node)
  (let ((package (plump:attribute node "package"))
        (exclude (cl-ppcre:split "\\s+" (plump:attribute node "exclude"))))
    (plump:remove-attribute node "package")
    (plump:remove-attribute node "exclude")
    (process-attributes node)
    (let ((package (resolve-value (read-from-string package))))
      (setf (clipboard 'symbols)
            (remove-if #'(lambda (symb)
                           (loop for ex in exclude thereis (symb-is symb (find-symbol (string-upcase ex) "KEYWORD"))))
                       (package-symbol-objects package)))
      (process-attribute :iterate "symbols"))))
