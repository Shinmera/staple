#|
 This file is a part of Staple
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.staple)

(defvar *symbol-object-transformers* (make-hash-table))

(defmacro define-symbol-transformer (identifier (symbolvar) &body body)
  `(setf (gethash ',identifier *symbol-object-transformers*)
         #'(lambda (,symbolvar) ,@body)))

(defun define-easy-symbol-transformer (symb-object-type test-function)
  (define-symbol-transformer symb-object-type (symbolvar)
    (when (funcall test-function symbolvar)
      (make-instance symb-object-type :symbol symbolvar))))


(defclass symb-object ()
  ((symbol :initarg :symbol :initform (error "SYMBOL required") :accessor symb-symbol)))
(defclass symb-type (symb-object) ())
(defclass symb-variable (symb-object) ())
(defclass symb-function (symb-object) ())
(defclass symb-macro (symb-function) ())
(defclass symb-generic (symb-function) ())
(defclass symb-method (symb-function)
  ((method :initarg :method :initform (error "METHOD required") :accessor symb-method)))
(defclass symb-class (symb-type) ())
(defclass symb-special (symb-variable) ())
(defclass symb-constant (symb-variable) ())

(defmethod print-object ((symb symb-object) stream)
  (print-unreadable-object (symb stream :type T)
    (format stream "~a" (symb-symbol symb)))
  symb)

(defmethod print-object ((symb symb-method) stream)
  (print-unreadable-object (symb stream :type T)
    (format stream "~a ~{~a ~}~a"
            (symb-symbol symb)
            (method-qualifiers (symb-method symb))
            (symb-argslist symb)))
  symb)

(define-easy-symbol-transformer 'symb-function #'symbol-function-p)
(define-easy-symbol-transformer 'symb-macro #'symbol-macro-p)
(define-easy-symbol-transformer 'symb-generic #'symbol-generic-p)
(define-easy-symbol-transformer 'symb-class #'symbol-class-p)
(define-easy-symbol-transformer 'symb-special #'symbol-special-p)
(define-easy-symbol-transformer 'symb-constant #'symbol-constant-p)

(define-symbol-transformer symb-method (symbol)
  (when (symbol-generic-p symbol)
    (loop for method in (generic-function-methods (fdefinition symbol))
          collect (make-instance 'symb-method :symbol symbol :method method))))

(defun symbol-objects (symbol)
  (flatten
   (loop for transformer being the hash-values of *symbol-object-transformers*
         collect (funcall transformer symbol))))

(defgeneric symb-scope (symb-object)
  (:documentation "")
  (:method ((symb symb-object))
    (let ((symbol (symb-symbol symb)))
      (nth-value 1 (find-symbol (symbol-name symbol) (symbol-package symbol))))))

(defgeneric symb-argslist (symb-object)
  (:documentation "")
  (:method ((symb symb-object))
    NIL)
  (:method ((symb symb-function))
    #+:sbcl (sb-introspect:function-lambda-list (symb-symbol symb))
    #+:allegro (excl:arglist (symb-symbol symb)))
  (:method ((symb symb-method))
    (loop with args = (call-next-method)
          for specializer in (method-specializers (symb-method symb))
          for i from 0
          do (setf (nth i args)
                   (list (nth i args) (etypecase specializer
                                        (eql-specializer (eql-specializer-object specializer))
                                        (class (class-name specializer)))))
          finally (return args))))

(defgeneric symb-docstring (symb-object)
  (:documentation "")
  (:method ((symb symb-function))
    (documentation (symb-symbol symb) 'function))
  (:method ((symb symb-method))
    (documentation (symb-method symb) T))
  (:method ((symb symb-variable))
    (documentation (symb-symbol symb) 'variable))
  (:method ((symb symb-type))
    (documentation (symb-symbol symb) 'type)))

(defun symbol-function-p (symbol)
  (and (fboundp symbol)
       (or (listp symbol)
           (not (macro-function symbol)))
       (not (typep (fdefinition symbol) 'standard-generic-function))))

(defun symbol-macro-p (symbol)
  (and (fboundp symbol)
       (macro-function symbol)))

(defun symbol-generic-p (symbol)
  (and (fboundp symbol)
       (typep (fdefinition symbol) 'standard-generic-function)))

(defun symbol-constant-p (symbol)
  #+:lispworks (sys:symbol-constant-p symbol)
  #-:lispworks (constantp symbol))

(defun symbol-special-p (symbol)
  (and (not (symbol-constant-p symbol))
       #+:lispworks (sys:declared-special-p symbol)
       #+:sbcl (eql :special (sb-int:info :variable :kind symbol))
       #+:allegro (eq (sys:variable-information symbol) :special)))

(defun symbol-class-p (symbol)
  (if (find-class symbol nil) T NIL))

(defun package-symbols (&optional package)
  "Gets all symbols within a package."
  (let ((lst ())
        (package (find-package package)))
    (do-all-symbols (s lst)
      (if package
          (when (eql (symbol-package s) package)
            (push s lst))
          (push s lst)))
    lst))
