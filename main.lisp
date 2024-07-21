(in-package #:org.shirakumo.staple)
(defun asdf:upgrade-asdf () NIL)

(defun ensure-dependencies-exist (system)
  (labels ((r (system)
             (let* ((system (etypecase system
                              ((or string symbol) (asdf:find-system system))
                              (asdf:system system)))
                    (name (asdf:component-name system)))
               (cond #+quicklisp
                     ((ql-dist:find-system name)
                      (ql-dist:ensure-installed (ql-dist:find-system name)))
                     ((not (probe-file (asdf:system-source-file system)))
                      (error "Source for system ~s seems to be unavailable!" name)))
               (mapc #'r (asdf:system-defsystem-depends-on system))
               (mapc #'r (asdf:system-depends-on system)))))
    (r system)))

(defun main ()
  (destructuring-bind (&optional system &rest args) uiop:*command-line-arguments*
    (handler-bind ((error
                     (lambda (e)
                       (format *error-output* "[ERROR] ~a~%" e)
                       (uiop:print-condition-backtrace e)
                       (uiop:quit 1)))
                   (sb-sys:interactive-interrupt
                     (lambda (e)
                       (declare (ignore e))
                       (uiop:quit 2))))
        (cond (system
               (let ((here (uiop/os:getcwd)))
                 (setf ql:*local-project-directories* ())
                 (asdf:clear-configuration)
                 (asdf:initialize-source-registry)
                 (asdf:initialize-source-registry `(:source-registry (:tree ,here) :inherit-configuration)))
               (let ((kargs ()))
                 (loop for (key val) on args by #'cddr
                       do (flet ((argp (short long)
                                   (or (and short (string-equal key (format NIL "-~a" short)))
                                       (and long (string-equal key (format NIL "--~a" long))))))
                            (when (and val (string/= "" val))
                              (cond ((argp "o" "output")
                                     (setf (getf kargs :output-directory) (pathname-utils:parse-native-namestring val)))
                                    ((argp "i" "image")
                                     (push (pathname-utils:parse-native-namestring val) (getf kargs :images)))
                                    ((argp "d" "document")
                                     (push (pathname-utils:parse-native-namestring val) (getf kargs :documents)))
                                    ((argp "p" "page-type")
                                     (setf (getf kargs :page-type) (read-from-string val)))
                                    ((argp "t" "template")
                                     (setf (getf kargs :template) (pathname-utils:parse-native-namestring val)))
                                    ((argp "k" "package")
                                     (push val (getf kargs :packages)))
                                    ((argp "s" "subsystem")
                                     (push val (getf kargs :subsystems)))
                                    (T
                                     (error "Unknown argument: ~a" key))))))
                 (ensure-dependencies-exist system)
                 (apply #'staple:generate system :if-exists :supersede kargs)))
              (T
               (format *query-io* "~&Staple documentation generation tool

Usage: staple system [arg...]

  system
       The name of the project/system to generate documentation for.

  -o --output
       The directory into which to output the documentation.

  -i --image
       An image to include in the inferred project. Can be specified
       multiple times.

  -d --document
       A document to include in the inferred project. Can be specified
       multiple times.

  -p --page-type
       The page type to use for inferred documents.

  -t --template
       The Clip template file to use for templated documents.

  -k  --package
       A package to include in the symbol index. Can be specified
       multiple times. The name is READ to convert it to native case.

  -s --subsystem
       The name of a system to include as a subsystem. Can be
       specified multiple times.

While you can specify the project inference properties right here on
the command line, it is recommended to instead rely on a
staple.ext.lisp file in your source directory to persist these
preferences. For more information, please see the Staple
documentation.

Prior to invoking GENERATE, ASDF is updated to search for ASD files
within the current working directory. Thus, for it to find your
system, you should invoke staple from the project root in which all
necessary systems are contained.~&"))))))
