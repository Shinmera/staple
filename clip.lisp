#|
 This file is a part of Staple
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.staple)

(defvar *current-packages* ()
  "List of packages being processed by the template.
This is to used to figure out whether a symbol can be found on the 
documentation page when referring to it through RESOLVE-SYMBOL-DOCUMENTATION.")

(defun year ()
  (nth-value 5 (get-decoded-time)))

(defun month ()
  (nth-value 4 (get-decoded-time)))

(defun day ()
  (nth-value 3 (get-decoded-time)))

(defun licenselink (license)
  "Returns an A tag linked to a TLDRLegal.com search on the license name."
  (format NIL "<a href=\"https://tldrlegal.com/search?q=~a\">~a</a>" license license))

(defun string-starts-with (string sub)
  "Returns T if the string starts with sub, NIL otherwise."
  (and (<= (length sub) (length string))
       (string-equal (subseq string 0 (length sub)) sub)))

(defun resolve-symbol-documentation (symbol)
  "Attempts to resolve the (string) symbol to either an URL or an anchor.
This works by first testing against the package. If it is known (such as the
sb-*, mop, cl and *current-packages*) a link/anchor is returned. If nothing
can be found, NIL is returned instead. If no package designator is given,
the symbol is attempted to be automatically found in either the
*current-packages* or in CL."
  (let ((name) (package) (symbol (string-upcase symbol)))
    (let ((colonpos (position #\: symbol)))
      (if colonpos
          (if (= 0 colonpos)
              (return-from resolve-symbol-documentation NIL)
              (setf package (subseq symbol 0 colonpos)
                    name (subseq symbol (1+ colonpos))))
          (setf package "" name symbol)))
    (cond
      ((string-starts-with package "sb-")
       (format NIL "http://l1sp.org/sbcl/~a:~a" package name))
      ((string-equal package "mop")
       (format NIL "http://l1sp.org/mop/~a" name))
      ((or (string-equal package "cl")
           (string-equal package "common-lisp"))
       (format NIL "http://l1sp.org/cl/~a" name))
      ((find package *current-packages* :test #'string-equal)
       (format NIL "#~a:~a" (string-upcase package) name))
      (T
       (dolist (package *current-packages*)
         (multiple-value-bind (found scope) (find-symbol name (find-package (string-upcase package)))
           (when (and found (eql scope :external))
             (return-from resolve-symbol-documentation
               (format NIL "#~a:~a" (string-upcase package) name)))))
       (when (find-symbol name "CL")
         (format NIL "http://l1sp.org/cl/~a" name))))))

(defun anchor (object)
  "Returns a href-anchor."
  (format NIL "#~a" object))

(defun stext (node object)
  "Same as lQuery's TEXT, but calls PRINC-TO-STRING on the object or uses an empty string on NIL."
  (lquery-funcs:text node (princ-to-string (or object ""))))

(defun parse-block-symbols (html)
  (cl-ppcre:regex-replace-all
   "\\([^\\s)'`]+" html
   #'(lambda (target start end match-start match-end reg-starts reg-ends)
       (declare (ignore start end reg-starts reg-ends))
       (let* ((name (subseq target (1+ match-start) match-end))
              (href (resolve-symbol-documentation name)))
         (if href
             (format NIL "(<a href=\"~a\">~a</a>" href name)
             (format NIL "(~a"  name))))))

(defun parse-lone-symbols (html)
  (cl-ppcre:regex-replace-all
   "^[^:][^\\s]+$" html
   #'(lambda (target &rest rest)
       (declare (ignore rest))
       (let* ((href (resolve-symbol-documentation target)))
         (if href
             (format NIL "<a href=\"~a\">~a</a>" href target)
             (format NIL "~a"  target))))))

(define-tag-processor documentate (node)
  (process-attributes node)
  (process-children node)
  ($ node "code"
    (combine (node) (html))
    (map-apply #'(lambda (node html)
                   ($ node (html (parse-lone-symbols (parse-block-symbols html))))))))

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

(define-tag-processor asdf (node)
  (let ((*clipboard* (asdf:find-system (resolve-attribute node "system"))))
    (plump:remove-attribute node "system")
    (process-attributes node)
    (process-children node)))

(define-tag-processor package (node)
  (let ((*clipboard* (find-package (resolve-attribute node "name"))))
    (plump:remove-attribute node "name")
    (process-attributes node)
    (process-children node)))

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
    (process-attribute "iterate" "symbols"))
  (plump:remove-attribute node "symbols"))

(defun %is-excluded (symb exclude)
  (loop for ex in exclude thereis (symb-is symb (find-symbol (string-upcase ex) "KEYWORD"))))

(define-tag-processor do-symbols (node)
  (let ((package (plump:attribute node "package"))
        (sort (or (plump:attribute node "sort") "#'symb-type<"))
        (exclude (cl-ppcre:split "\\s+" (plump:attribute node "exclude"))))
    (plump:remove-attribute node "package")
    (plump:remove-attribute node "sort")
    (plump:remove-attribute node "exclude")
    (process-attributes node)
    (let ((package (resolve-value (read-from-string package)))
          (*clipboard* (make-clipboard)))
      (setf (clipboard 'symbols)
            (sort
             (remove-if #'(lambda (symb) (%is-excluded symb exclude))
                        (package-symbol-objects package))
             (resolve-value (read-from-string sort))))
      (process-attribute "iterate" "symbols"))))
