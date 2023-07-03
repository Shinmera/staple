(in-package #:org.shirakumo.staple.code-parser)

(defclass client (eclector.concrete-syntax-tree::cst-client)
  ())

(defclass placeholder ()
  ((name :initarg :name :reader placeholder-name)
   (package :initarg :package :reader placeholder-package)
   (intern :initarg :intern :reader placeholder-intern))
  (:default-initargs
   :name (error "NAME required")
   :package NIL
   :intern NIL))

(defmethod print-object ((placeholder placeholder) stream)
  (let ((as-is (format NIL "~a~:[:~;::~]~a"
                       (placeholder-package placeholder)
                       (placeholder-intern placeholder)
                       (placeholder-name placeholder))))
    (ecase (readtable-case *readtable*)
      (:upcase
       (format stream "~:@(~a~)" as-is))
      (:downcase
       (format stream "~(~a~)" as-is))
      (:preserve
       (format stream "~a" as-is))
      (:invert
       (cond ((loop for char across as-is
                    always (lower-case-p char))
              (format stream "~:@(~a~)" as-is))
             ((loop for char across as-is
                    always (upper-case-p char))
              (format stream "~(~a~)" as-is))
             (T
              (format stream "~a" as-is)))))))

(defmethod eclector.reader:interpret-symbol ((client client) in package symbol intern)
  (handler-case (call-next-method)
    (eclector.reader:package-does-not-exist (e)
      (declare (ignore e))
      (make-instance 'placeholder :package package :name symbol :intern intern))
    (eclector.reader:symbol-does-not-exist (e)
      (declare (ignore e))
      (make-instance 'placeholder :package package :name symbol :intern intern))
    (eclector.reader:symbol-is-not-external (e)
      (declare (ignore e))
      (find-symbol symbol package))))

(defmethod eclector.reader:evaluate-expression ((client client) expression)
  (eval expression))

(defmethod walk ((cst cst:ordinary-lambda-list) environment)
  (flet ((parameter (type)
           (when-let ((group (find-if (of-type type) (cst:children cst))))
             (list (walk (cst:parameter group) environment))))
         (parameters (type)
           (when-let ((group (find-if (of-type type) (cst:children cst))))
             (mapcar (rcurry #'walk environment) (cst:parameters group)))))
    (append (parameters 'cst:ordinary-required-parameter-group)
            (parameters 'cst:ordinary-optional-parameter-group)
            (parameter  'cst:ordinary-rest-parameter-group)
            (parameters 'cst:key-parameter-group))))

(defmethod walk ((cst cst:macro-lambda-list) environment)
  (flet ((parameter (type)
           (when-let ((group (find-if (of-type type) (cst:children cst))))
             (list (walk (cst:parameter group) environment))))
         (parameters (type)
           (when-let ((group (find-if (of-type type) (cst:children cst))))
             (mapcar (rcurry #'walk environment) (cst:parameters group)))))
    (append (parameter  'cst:ordinary-whole-parameter-group)
            (parameters 'cst:destructuring-required-parameter-group)
            (parameters 'cst:ordinary-optional-parameter-group)
            (parameter  'cst:destructuring-rest-parameter-group)
            (parameters 'cst:key-parameter-group)
            (parameter  'cst:environment-parameter-group))))

(defmethod walk ((cst cst:specialized-lambda-list) environment)
  (flet ((parameter (type)
           (when-let ((group (find-if (of-type type) (cst:children cst))))
             (list (walk (cst:parameter group) environment))))
         (parameters (type)
           (when-let ((group (find-if (of-type type) (cst:children cst))))
             (mapcar (rcurry #'walk environment) (cst:parameters group)))))
    (append (parameters 'cst:specialized-required-parameter-group)
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

(defmethod walk ((cst cst:specialized-required-parameter) environment)
  (let ((specializer (cst:specializer cst)))
    (list (walk (cst:name cst) environment)
          (if (cst:atom specializer)
              `(:type ,(cst:source specializer) ,(cst:raw specializer))
              (walk specializer environment)))))

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
      (cst:separate-ordinary-body cst)
    (declare (ignore declarations))
    (walk-implicit-progn forms environment)))

(defun walk-lambda-like (cst environment &optional (lambda-list-parser #'cst:parse-ordinary-lambda-list))
  (cst:db source (operator-or-name lambda-list . body) cst
    (declare (ignore operator-or-name))
    (let ((variables (walk (funcall lambda-list-parser T lambda-list) environment)))
      (multiple-value-bind (declarations documentation forms)
          (cst:separate-function-body body)
        ;; FIXME: parse declarations
        (declare (ignore declarations documentation))
        `(lambda ,source
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

(defmacro define-walker-form (form (cst-var &optional (environment-var (gensym "ENVIRONMENT")) (source (gensym "SOURCE"))) &body body)
  (let ((operator (first form))
        (thunk (gensym "THUNK")))
    `(define-walk-compound-form ,operator (,cst-var ,environment-var)
       (cst:db ,source ,form ,cst-var
         (declare (ignore ,operator))
         (flet ((,thunk ()
                  ,@body))
           (list* ',operator ,source
                  (,thunk)))))))

(defmethod read-toplevel ((input string))
  (with-input-from-string (stream input)
    (read-toplevel stream)))

(defmethod read-toplevel ((input pathname))
  (with-open-file (stream input :direction :input)
    (read-toplevel stream)))

(defmethod read-toplevel ((input stream))
  (let ((eclector.reader:*client* (make-instance 'client)))
    (loop for top-level-form = (restart-case (eclector.parse-result:read
                                              eclector.reader:*client* input nil '#1=#.(make-symbol "EOF"))
                                 (continue ()
                                   :report "Skip the top-level form"
                                   nil))
          until (eq top-level-form '#1#)
          collect top-level-form)))

(defmethod parse (input)
  (loop with environment = (make-instance 'environment)
        for form in (read-toplevel input)
        collect (walk form environment)))
