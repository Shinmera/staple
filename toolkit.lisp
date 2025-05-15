(in-package #:org.shirakumo.staple)

(defun read-value ()
  (format *query-io* "~&> Enter a new value: ~%")
  (multiple-value-list (eval (read))))

(defun split (split string)
  (let ((items ()) (out (make-string-output-stream)))
    (flet ((push-item ()
             (let ((string (get-output-stream-string out)))
               (when (string/= "" string)
                 (push string items)))))
      (loop for char across string
            do (if (char= char split)
                   (push-item)
                   (write-char char out))
            finally (push-item))
      (nreverse items))))

(defmacro with-value-restart (place &body body)
  (let ((value (gensym "VALUE"))
        (condition (gensym "CONDITION")))
    `(loop (restart-case
               (return
                 (progn ,@body))
             (store-value (,value &optional ,condition)
               :report "Set a new value."
               :interactive read-value
               (declare (ignore ,condition))
               (setf ,place ,value))))))

(defun ensure-system (system-ish)
  (typecase system-ish
    (asdf:system
     system-ish)
    ((or string symbol)
     (asdf:find-system system-ish T))
    (cons
     (asdf/find-component:resolve-dependency-spec
      (asdf:find-system :staple) system-ish))))

(defmethod system-name ((system asdf:system))
  (intern (string-upcase (asdf:component-name system)) "KEYWORD"))

(defmethod system-name (name)
  (system-name (asdf:find-system name T)))

(defun system-field (field system)
  ;; Stupid motherfucking workaround for ASDF bugs
  (let ((system (ensure-system system))
        (field (find-symbol (string field) '#:asdf/system))
        (function (find-symbol (let ((*print-case* #. (readtable-case *readtable*)))
                                 (format NIL "~a-~a" 'system field))
                               '#:asdf)))
    (or (when function (ignore-errors (funcall function system)))
        (when field (ignore-errors (asdf/system::system-virtual-slot-value system field))))))

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

(defun map-directory-tree (function directory &key (max-depth most-positive-fixnum) exclude)
  (flet ((unspecific-p (path)
           (and (pathname-utils:unspecific-p (pathname-name path))
                (pathname-utils:unspecific-p (pathname-type path))
                (equal '(:relative) (pathname-directory path)))))
    (dolist (file (directory (merge-pathnames "*.*" directory)))
      (unless (find (file-namestring file) exclude :test #'pathname-utils:pathname=)
        (funcall function file)))
    (dolist (dir (directory (merge-pathnames "*/" directory)))
      (let ((exclude (loop for path in exclude
                           for directory = (pathname-directory path)
                           when (and directory (string= (pathname-utils::directory-name dir) (second directory)))
                           collect (make-pathname :directory (list* (car directory) (cddr directory)) :defaults path))))
        (unless (some #'unspecific-p exclude)
          (map-directory-tree function dir :max-depth (1- max-depth) :exclude exclude))))))

(defmacro do-directory-tree ((file directory &optional result &rest args) &body body)
  `(progn (map-directory-tree (lambda (,file) ,@body) ,directory ,@args)
          ,result))

(defun find-files (directory patterns &key (max-depth most-positive-fixnum) exclude)
  (let ((files ()))
    (do-directory-tree (file directory (nreverse files) :max-depth max-depth :exclude exclude)
      (when (loop for pattern in patterns
                  thereis (cl-ppcre:scan pattern (file-namestring file)))
        (push file files)))))

(defun read-file (path)
  (with-open-file (in path)
    (with-output-to-string (out)
      (loop with buffer = (make-array 4096 :element-type 'character)
            for read = (read-sequence buffer in)
            while (< 0 read)
            do (write-sequence buffer out :end read)))))

(defgeneric definition-id (definition))

(defmethod definition-id ((definition definitions:package))
  (format NIL "~a ~a"
          (definitions:type definition)
          (definitions:name definition)))

(defmethod definition-id ((definition definitions:global-definition))
  (format NIL "~a ~a:~a"
          (definitions:type definition)
          (package-name (definitions:package definition))
          (definitions:designator definition)))

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
  (:method ((_ definitions:function))            90)
  (:method ((_ definitions:generic-function))    80)
  (:method ((_ definitions:method))              70)
  (:method ((_ definitions:compiler-macro))      60)
  (:method ((_ definitions:macro))               50)
  (:method ((_ definitions:setf-expander))       40)
  (:method ((_ definitions:callable))            30)
  (:method ((_ definitions:method-combination))  20)
  (:method ((_ definitions:global-definition))   10)
  (:method ((_ definitions:definition))           0))

(defun sort-definitions (definitions)
  (flet ((sorter (a b)
           (if (eql (type-of a) (type-of b))
               (string< (definitions:name a)
                        (definitions:name b))
               (> (definition-order a)
                  (definition-order b)))))
    (stable-sort definitions #'sorter)))

(defgeneric definition-importance (definition)
  (:method ((_ definitions:callable))           30)
  (:method ((_ definitions:type))               20)
  (:method ((_ definitions:variable))           10)
  (:method ((_ definitions:definition))          0)
  (:method ((_ definitions:method))            -10))

(defun preferred-definition (definitions)
  (stable-sort definitions #'> :key #'definition-importance))

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

(defun ensure-package-definition (package)
  (etypecase package
    (string (make-instance 'definitions:package :designator package))
    (symbol (make-instance 'definitions:package :designator (symbol-name package)))
    (package (make-instance 'definitions:package :designator (package-name package)))
    (definitions:package package)))

(defun ensure-package (package)
  (or (etypecase package
        (string (find-package package))
        (symbol (find-package package))
        (package package)
        (definitions:package (definitions:object package)))
      (error "No such package ~s." package)))

(defun skip-to-source-form (stream)
  (loop for char = (peek-char T stream)
        do (case char
             (#\; (loop for char = (read-char stream NIL)
                        until (or (not char) (char= char #\Linefeed))))
             (#\#
              (read-char stream)
              (if (char= #\| (peek-char T stream))
                  (funcall (get-dispatch-macro-character #\# #\|) stream #\| NIL)
                  (return stream)))
             (T
              (return stream)))))

(defun absolute-source-location (source-location)
  (destructuring-bind (&key file form offset) source-location
    (when file
      ;; Translate form to file-position
      (when form
        (ignore-errors
         (let ((*package* *package*))
           (with-open-file (stream file :direction :input)
             (loop repeat form
                   for read = (read stream NIL)
                   do (ignore-errors
                       (when (and (listp read) (find 'cl:in-package read))
                         (eval read))))
             (skip-to-source-form stream)
             (setf offset (+ (or offset 0) (file-position stream)))))))
      ;; Count row + col
      (when offset
        (ignore-errors
         (with-open-file (stream file :direction :input)
           (loop with row = 1 with col = 0
                 repeat offset
                 for char = (read-char stream)
                 do (if (char= char #\Linefeed)
                        (setf row (1+ row) col 0)
                        (setf col (1+ col)))
                 finally (return (list :file file
                                       :offset offset
                                       :row row
                                       :col col)))))))))

(defun extract-language (string)
  (cl-ppcre:do-matches-as-strings (code "\\b\\w{2,3}\\b" string)
    (let* ((code (find-symbol (string-upcase code) "KEYWORD"))
           (found (when code (language-codes:names code))))
      (when found
        (return (values code found))))))

(defun maybe-lang-docstring (definition language)
  (or (when (find-package "ORG.SHIRAKUMO.MULTILANG-DOCUMENTATION")
        (or
         (multiple-value-bind (object unknown-p) (definitions:object definition)
           (unless (eql :unknown unknown-p)
             (funcall (find-symbol "DOCUMENTATION" "ORG.SHIRAKUMO.MULTILANG-DOCUMENTATION")
                      object T :lang language)))
         (funcall (find-symbol "DOCUMENTATION" "ORG.SHIRAKUMO.MULTILANG-DOCUMENTATION")
                  (definitions:designator definition) (definitions:type definition) :lang language)))
      (definitions:documentation definition)))

(defun ensure-stream (designator &key (direction :input) (if-exists :error) (element-type 'character))
  (etypecase designator
    (stream
     designator)
    ((or string pathname)
     (open designator :direction direction
                      :if-exists if-exists
                      :element-type element-type))
    (null
     ;; FIXME: support for byte streams
     (make-string-output-stream :element-type element-type))
    ((eql T)
     *standard-output*)))

(defun stream-value (stream)
  (etypecase stream
    (file-stream
     (pathname stream))
    (string-stream
     (if (output-stream-p stream)
         (get-output-stream-string stream)
         stream))
    (T stream)))

(defmacro with-stream ((stream designator &rest args) &body body)
  `(let ((,stream (ensure-stream ,designator ,@args)))
     (block ,stream
       (unwind-protect
            (progn ,@body
                   (stream-value ,stream))
         (close ,stream)))))

(deftype stream-designator ()
  '(or null (eql T) stream string pathname))

(defun merge-plists (plist default)
  (let ((result (copy-list plist)))
    (loop for (key val) on default by #'cddr
          do (when (eq #1='#.(gensym "no-value") (getf result key #1#))
               (setf (getf result key) val)))
    result))

(defun prefix-p (prefix string)
  (and (<= (length prefix) (length string))
       (string-equal prefix string :end2 (length prefix))))

(defun titleize (thing)
  (with-output-to-string (out)
    (loop for char across (string thing)
          do (case char
               (#\- (write-char #\Space out))
               (T (write-char char out))))))

(defun extract-author-name (author)
  (or (cl-ppcre:register-groups-bind (name) ("([^<]+)" author)
        (string-trim " " name))
      author))

(defun extract-author-email (author)
  (cl-ppcre:register-groups-bind (email) ("(?:[^<]+)<(.*)>" author)
    email))

(defgeneric relative-path (to from))

(defmethod relative-path ((to pathname) (from pathname))
  (pathname-utils:relative-pathname from to))

(defun load-system-quietly (system)
  (let ((*standard-output* (make-broadcast-stream))
        (*trace-output* (make-broadcast-stream))
        (*load-verbose* NIL)
        (*load-print* NIL)
        (*compile-verbose* NIL)
        (*compile-print* NIL))
    (handler-bind ((warning #'muffle-warning)
                   #+sbcl (sb-ext:compiler-note #'muffle-warning))
      (if (find-package '#:ql)
          (funcall (find-symbol (string '#:quickload) '#:ql)
                   (etypecase system
                     (asdf:system (asdf:component-name system))
                     ((or string symbol) system)))
          (asdf:load-system system)))))

(defun unlist (listish)
  (if (listp listish) (first listish) listish))

(defun ensure-parsed (thing)
  (etypecase thing
    (plump-dom:node thing)
    ((or string pathname) (plump:parse thing))))

(defun purify-arglist (arglist)
  (loop with part = '&required
        for cons on arglist
        for arg = (car cons)
        do (cond ((find arg lambda-list-keywords)
                  (setf part arg)))
        collect (case part
                  (&required
                   (if (listp arg)
                       (purify-arglist arg)
                       arg))
                  (&optional (unlist arg))
                  (&key (unlist (unlist arg)))
                  ((&whole &environment &aux))
                  (T arg))
        unless (or (null (cdr cons)) (consp (cdr cons)))
        collect '&rest))

