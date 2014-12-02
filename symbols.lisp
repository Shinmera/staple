#|
 This file is a part of Staple
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.staple)

(defclass symb-object ()
  ((symbol :initarg :symbol :initform (error "SYMBOL required") :accessor symb-symbol))
  (:documentation "Base class for symbol representation."))
(defclass symb-type (symb-object) ()
  (:documentation "Object representing a type."))
(defclass symb-variable (symb-object) ()
  (:documentation "Object representing a variable."))
(defclass symb-function (symb-object) ()
  (:documentation "Object representing a function."))
(defclass symb-macro (symb-function) ()
  (:documentation "Object representing a macro."))
(defclass symb-generic (symb-function) ()
  (:documentation "Object representing a generic function."))
(defclass symb-method (symb-function)
  ((method :initarg :method :initform (error "METHOD required") :accessor symb-method))
  (:documentation "Object representing a generic function method."))
(defclass symb-class (symb-type) ()
  (:documentation "Object representing a class."))
(defclass symb-special (symb-variable) ()
  (:documentation "Object representing a special variable."))
(defclass symb-constant (symb-variable) ()
  (:documentation "Object representing a constant."))

(defmethod print-object ((symb symb-object) stream)
  (print-unreadable-object (symb stream :type T)
    (format stream "~a" (symb-symbol symb)))
  symb)

(defmethod print-object ((symb symb-method) stream)
  (print-unreadable-object (symb stream :type T)
    (format stream "~a ~{~s ~}~s"
            (symb-symbol symb)
            (method-qualifiers (symb-method symb))
            (symb-arguments symb)))
  symb)

(defgeneric symb-name (symb-object)
  (:documentation "Returns the symbol-name of the symbol.")
  (:method ((symb symb-object))
    (symbol-name (symb-symbol symb))))

(defgeneric symb-package (symb-object)
  (:documentation "Returns the symbol-package of the symbol.")
  (:method ((symb symb-object))
    (symbol-package (symb-symbol symb))))

(defgeneric symb-function (symb-object)
  (:documentation "Returns the symbol-function of the symbol.")
  (:method ((symb symb-object))
    (symbol-function (symb-symbol symb))))

(defgeneric symb-type (symb-object)
  (:documentation "Returns the string-name of the kind of object it represents.")
  (:method ((symb symb-object))
    (subseq (string-upcase (class-name (class-of symb))) 5)))

(defgeneric symb-scope (symb-object)
  (:documentation "Returns whether the symbol is :INHERITED, :EXTERNAL or :INTERNAL.")
  (:method ((symb symb-object))
    (let ((symbol (symb-symbol symb)))
      (nth-value 1 (find-symbol (symbol-name symbol) (symbol-package symbol))))))

(defgeneric symb-qualifiers (symb-object)
  (:documentation "Returns the qualifiers of the method or NIL.")
  (:method ((symb symb-method))
    (method-qualifiers (symb-method symb)))
  (:method ((symb symb-object))
    NIL))

(defgeneric symb-arguments (symb-object)
  (:documentation "Returns the arguments of the function or NIL.")
  (:method ((symb symb-object))
    NIL)
  (:method ((symb symb-function))
    (arg:arglist (symb-symbol symb)))
  (:method ((symb symb-generic))
    ;; Apparently f.e. SBCL reports things from methods too? (???)
    (let ((args (call-next-method)))
      (remove-if #'listp args)))
  (:method ((symb symb-method))
    (loop with args = (call-next-method)
          for specializer in (method-specializers (symb-method symb))
          for i from 0
          do (setf (nth i args)
                   (list (nth i args) (etypecase specializer
                                        (eql-specializer `(eql ,(eql-specializer-object specializer)))
                                        (class (class-name specializer)))))
          finally (return args))))

(defgeneric symb-documentation (symb-object)
  (:documentation "Returns the documentation-string.")
  (:method ((symb symb-function))
    (documentation (symb-symbol symb) 'function))
  (:method ((symb symb-method))
    (documentation (symb-method symb) T))
  (:method ((symb symb-variable))
    (documentation (symb-symbol symb) 'variable))
  (:method ((symb symb-type))
    (documentation (symb-symbol symb) 'type)))

(defgeneric symb-is (symb-object mask)
  (:documentation "Checks if the symbol matches the mask.
The mask should be a keyword of either :INHERITED, :INTERNAL, :EXTERNAL
or one of the symb-object types.")
  (:method (symb mask) NIL)
  (:method ((symb symb-object) (mask (eql :inherited)))
    (eql (symb-scope symb) :inherited))
  (:method ((symb symb-object) (mask (eql :internal)))
    (eql (symb-scope symb) :internal))
  (:method ((symb symb-object) (mask (eql :external)))
    (eql (symb-scope symb) :external))
  (:method ((symb symb-type) (mask (eql :type))) T)
  (:method ((symb symb-variable) (mask (eql :variable))) T)
  (:method ((symb symb-function) (mask (eql :function))) T)
  (:method ((symb symb-macro) (mask (eql :macro))) T)
  (:method ((symb symb-generic) (mask (eql :generic))) T)
  (:method ((symb symb-method) (mask (eql :method))) T)
  (:method ((symb symb-class) (mask (eql :class))) T)
  (:method ((symb symb-special) (mask (eql :special))) T)
  (:method ((symb symb-constant) (mask (eql :constant))) T))

(defgeneric symb< (a b)
  (:documentation "Used to sort symbols alphabetically.
Special treatment is done so that generic functions should
always appear before their methods.")
  (:method ((a symb-method) (b symb-generic))
    (if (eql (symb-symbol a) (symb-symbol b))
        T
        (call-next-method)))
  (:method ((a symb-generic) (b symb-method))
    (if (eql (symb-symbol a) (symb-symbol b))
        NIL
        (call-next-method)))
  (:method ((a symb-object) (b symb-object))
    (string< (symb-symbol a) (symb-symbol b))))

(defun symb-type< (a b)
  "Used to sort symbols alphabetically, grouped by their type."
  (if (string-equal (symb-type a) (symb-type b))
      (symb< a b)
      (string< (symb-type a) (symb-type b))))

(defun symbol-function-p (symbol)
  "Returns T if the symbol is a pure function."
  (and (fboundp symbol)
       (or (listp symbol)
           (not (macro-function symbol)))
       (not (typep (fdefinition symbol) 'standard-generic-function))))

(defun symbol-macro-p (symbol)
  "Returns T if the symbol is a macro."
  (and (fboundp symbol)
       (macro-function symbol)))

(defun symbol-generic-p (symbol)
  "Returns T if the symbol is a generic function."
  (and (fboundp symbol)
       (typep (fdefinition symbol) 'standard-generic-function)))

(defun symbol-constant-p (symbol)
  "Returns T if the symbol is a constant."
  #+:lispworks (sys:symbol-constant-p symbol)
  #-:lispworks (constantp symbol))

(defun symbol-special-p (symbol)
  "REturns T if the symbol is a special variable."
  (and (not (symbol-constant-p symbol))
       ;; Oh gross.
       #+:ccl (ccl::%ilogbitp ccl::$sym_vbit_special (ccl::%symbol-bits 'staple:generate))
       ;; Nice.
       #+:lispworks (sys:declared-special-p symbol)
       #+:sbcl (eql :special (sb-int:info :variable :kind symbol))
       #+:allegro (eq (sys:variable-information symbol) :special)
       ;; Welp.
       #-(or :ccl :lispworks :sbcl :allegro) NIL))

(defun symbol-class-p (symbol)
  "Returns T if the symbol is a class."
  (if (find-class symbol nil) T NIL))

(defun package-symbols (package)
  "Gets all symbols within a package."
  (let ((lst ())
        (package (find-package package)))
    (do-symbols (s package lst)
      (when (eq (symbol-package s) package)
        (push s lst)))))

(defun symbol-objects (&rest symbols)
  "Gathers all possible symbol-objects out of the list of passed symbols."
  (let ((objs ()))
    (dolist (symbol symbols objs)
      (when (symbol-function-p symbol)
        (push (make-instance 'symb-function :symbol symbol) objs))
      (when (symbol-macro-p symbol)
        (push (make-instance 'symb-macro :symbol symbol) objs))
      (when (symbol-generic-p symbol)
        (push (make-instance 'symb-generic :symbol symbol) objs)
        (dolist (method (generic-function-methods (symbol-function symbol)))
          (push (make-instance 'symb-method :symbol symbol :method method) objs)))
      (when (symbol-special-p symbol)
        (push (make-instance 'symb-special :symbol symbol) objs))
      (when (symbol-constant-p symbol)
        (push (make-instance 'symb-constant :symbol symbol) objs))
      (when (symbol-class-p symbol)
        (push (make-instance 'symb-class :symbol symbol) objs)))))

(defun package-symbol-objects (package)
  "Gathers all possible symbol-objects of the given package."
  (apply #'symbol-objects (package-symbols package)))
