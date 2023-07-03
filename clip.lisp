(in-package #:org.shirakumo.staple)

(defmethod clip:clip ((component asdf:component) field)
  (case* string-equal field
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
    (load-dependencies (asdf:component-sideway-dependencies component))
    (sideway-dependencies (asdf:component-sideway-dependencies component))
    (T (call-next-method))))

(defmethod clip:clip ((system asdf:system) field)
  (case* string-equal field
    (author (system-field 'author system))
    (mailto (system-field 'mailto system))
    (licence (system-field 'licence system))
    (license (system-field 'licence system))
    (homepage (system-field 'homepage system))
    (long-name (system-field 'long-name system))
    (maintainer (system-field 'maintainer system))
    (bug-tracker (system-field 'bug-tracker system))
    (description (system-field 'description system))
    (source-file (system-field 'source-file system))
    (source-control (if (listp (system-field 'source-control system))
                        (second (system-field 'source-control system))
                        (system-field 'source-control system)))
    (long-description (system-field 'long-description system))
    (source-directory (system-field 'source-directory system))
    (definition-pathname (system-field 'source-file system))
    (defsystem-depends-on (system-field 'defsystem-depends-on system))
    (depends-on (system-field 'depends-on system))
    (weakly-depends-on (system-field 'weakly-depends-on system))
    (dependencies (loop for entry in (append (system-field 'defsystem-depends-on system)
                                             (system-field 'depends-on system)
                                             (system-field 'weakly-depends-on system))
                        for system = (ensure-system entry)
                        when system collect system))
    (license-link
     (let ((in-output (find-files (output (project *page*)) '("LICENCE" "LICENSE") :max-depth 1))
           (in-project (find-files (asdf:system-source-directory system) '("LICENCE" "LICENSE"))))
       (cond (in-output
              (relative-path (first in-output) *page*))
             (in-project
              (resolve-source-link (list :file (first in-project)) *page*))
             (T
              (format NIL "https://tldrlegal.com/search?q=~a" (system-field 'license system))))))
    (T (call-next-method))))

(defmethod clip:clip ((package package) field)
  (case* string-equal field
    (name (package-name package))
    (nicknames (package-nicknames package))
    (shadowing-symbols (package-shadowing-symbols package))
    (use-list (package-use-list package))
    (used-by-list (package-used-by-list package))
    (symbols
     (loop for symbol being the symbols of package
           collect symbol))
    (external-symbols
     (loop for symbol being the external-symbols of package
           collect symbol))
    (T (call-next-method))))

(defmethod clip:clip ((symbol symbol) field)
  (case* string-equal field
    (name (symbol-name symbol))
    (package (symbol-package symbol))
    (value (symbol-value symbol))
    (function (symbol-function symbol))
    (plist (symbol-plist symbol))
    (T (call-next-method))))

(defmethod clip:clip ((definition definitions:definition) field)
  (case* string-equal field
    (xref (xref definition))
    (id (definition-id definition))
    (designator (definitions:designator definition))
    (object (definitions:object definition))
    (symbol (definitions:symbol definition))
    (name (definitions:name definition))
    (full-name (format NIL "~a:~a"
                       (package-name (definitions:package definition))
                       (definitions:designator definition)))
    (package (definitions:package definition))
    (type (type-of definition))
    (kind (definitions:type definition))
    (visibility (definitions:visibility definition))
    (documentation (maybe-lang-docstring definition (language *page*)))
    (source-location (definitions:source-location definition))
    (arguments ())
    (qualifiers ())
    (source-link (resolve-source-link definition *page*))
    (formatted-documentation
     (or (format-documentation definition *page*)
         "<i>No documentation provided.</i>"))
    (T (call-next-method))))

(defmethod clip:clip ((definition definitions:callable) field)
  (case* string-equal field
    (arguments (definitions:arguments definition))
    (T (call-next-method))))

(defmethod clip:clip ((definition definitions:method) field)
  (case* string-equal field
    (qualifiers (definitions:qualifiers definition))
    (T (call-next-method))))

(defmethod clip:clip ((definition definitions:package) field)
  (case* string-equal field
    (nicknames (package-nicknames (definitions:object definition)))
    (definitions
     (definitions *page* (definitions:object definition)))
    (T (call-next-method))))
