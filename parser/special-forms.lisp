#|
 This file is a part of Staple
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.staple.code-parser)

(define-walker-form (block name . body) (cst environment)
  (let ((name/raw (cst:raw name)))
    (unless (symbolp name/raw)
      (error "~@<Block name must be a symbol, not ~S.~@:>" name/raw))
    (let ((environment (augmented-environment environment `((,name/raw . :block)) '(t))))
      (list* `(:block ,(cst:source name) ,(cst:raw name))
             (walk-implicit-progn body environment)))))

(define-walker-form (catch tag . body) (cst)
  (list* `(:tag ,(cst:source tag) ,(cst:raw tag))
         (walk-implicit-progn body)))

(define-walker-form (eval-when situations . body) (cst)
  (declare (ignore situations))
  (walk-implicit-progn body))

(define-walker-form (flet definitions . body) (cst environment)
  (let* ((definitions (cst:listify definitions))
         (names       (map 'list #'cst:first definitions)))
    (list* (mapcar #'walk names)
           (mapcar (lambda (definition)
                     (walk-lambda-like definition environment))
                   definitions)
           (walk-body body environment))))

(define-walk-compound-form function (cst environment)
  (cst:db source (operator function-designator) cst
    (declare (ignore operator))
    (if (and (cst:consp function-designator)
             (find (cst:raw (cst:first function-designator))
                   '(lambda #+sbcl sb-int:named-lambda)))
        (walk function-designator)
        `(function ,source
                   ,(cst:raw function-designator)))))

(define-walk-compound-form lambda (cst environment)
  (walk-lambda-like cst environment))

#+sbcl
(define-walker-form (sb-int:named-lambda name . lambda-like) (cst environment)
  (list* `(:variable ,(cst:source name) ,(cst:raw name))
         (cddr (walk-lambda-like (cst:cons name lambda-like) environment))))

(define-walker-form (go tag) (cst environment)
  (let ((tag/raw (cst:raw tag)))
    (unless (typep tag/raw '(or symbol integer))
      (error "~@<Tag must be a symbol or integer, not ~S.~@:>" tag/raw))
    (if-let ((tag* (lookup tag/raw :tag environment))) ; TODO store the variable in the environment?
      (list `(:label ,(cst:source cst) ,tag/raw))
      (error "~@<Unknown tag: ~S.~@:>" tag/raw))))

(define-walker-form (if test then . else) (cst)
  (unless (or (cst:null else)
              (cst:null (cst:rest else)))
    (error "~@<IF only accepts one otherwise-form, but ~s was given in addition.~@:>"
           (cst:raw (cst:rest else))))
  (list* (walk test)
         (walk then)
         (unless (cst:null else)
           (list (walk (cst:first else))))))

(define-walker-form (labels definitions . body) (cst environment)
  (let* ((definitions (cst:listify definitions))
         (names       (map 'list #'cst:first definitions)))
    (list (mapcar #'walk names)
          (mapcar (lambda (definition)
                    (walk-lambda-like definition environment)) ; TODO wrong environment
                  definitions)
          (walk-body body environment))))

(define-walker-form (let definitions . body) (cst environment)
  (let ((definitions (walk-bindings definitions environment)))
    (multiple-value-bind (declarations forms)
        (cst:separate-ordinary-body body :listify-body nil)
      (declare (ignore declarations))
      ;; FIXME: parse declarations
      (list* (mapcar #'car definitions)
             (mapcar #'cdr definitions)
             (walk-implicit-progn forms)))))

(define-walker-form (let* definitions . body) (cst environment)
  (let ((definitions (walk-bindings definitions environment)))
    (multiple-value-bind (declarations forms)
        (cst:separate-ordinary-body body :listify-body nil)
      (declare (ignore declarations))
      ;; FIXME: parse declarations
      (list* (mapcar #'car definitions)
             (mapcar #'cdr definitions)
             (walk-implicit-progn forms)))))

(define-walker-form (load-time-value form) (cst)
  (list (walk form)))

(define-walker-form (locally . body) (cst environment)
  ;; FIXME: parse declarations
  (walk-body body environment))

(define-walker-form (macrolet definitions . body) (cst environment)
  ;; FIXME: Handle expansions of local macros
  (let* ((definitions (cst:listify definitions))
         (names       (map 'list #'cst:first definitions)))
    (list* (map 'list #'walk names)
           (map 'list (lambda (definition)
                        (walk-lambda-like definition environment #'cst:parse-macro-lambda-list))
                definitions)
           (walk-body body environment))))

(define-walker-form (multiple-value-call function . arguments) (cst)
  (list* (walk function)
         (mapcar #'walk (cst:listify arguments))))

(define-walker-form (multiple-value-prog1 form . body) (cst)
  (list* (walk form)
         (walk-implicit-progn body)))

(define-walk-compound-form progn (cst)
  (unless (cst:null (cst:rest cst))
    (cst:db source (operator . body) cst
      (declare (ignore operator))
      (if (cst:null (cst:rest body))
          (walk (cst:first body))
          `(progn ,source
                  ,@(walk-implicit-progn body))))))

(define-walker-form (progv symbols values . forms) (cst environment)
  (list* (walk symbols)
         (walk values)
         (walk-implicit-progn forms environment)))

(define-walk-compound-form quote (cst)
  `(:literal ,(cst:source cst) ,(cst:raw (cst:second cst))))

(define-walker-form (return-from name . value) (cst environment)
  (let ((name/raw (cst:raw name)))
    (unless (symbolp name/raw)
      (error "~@<Block name must be a symbol, not ~S.~@:>" name/raw))
    (if-let ((name* (lookup name/raw :block environment)))
      (list `(:block ,(cst:source name*) ,(cst:raw name*))
            (when value (walk value)))
      (error "~@<Unknown block name: ~S.~@:>" name/raw))))

(define-walker-form (setq . pairs) (cst)
  (let ((pairs (cst:listify pairs)))
    (multiple-value-bind (vars vals)
        (loop for (var val) on pairs by #'cddr
              collect var into vars
              collect val into vals
              finally (return (values vars vals)))
      (list (mapcar #'walk vars)
            (mapcar #'walk vals)))))

(define-walker-form (symbol-macrolet definitions . body) (cst environment)
  (let* ((definitions (cst:listify definitions))
         (names      (map 'list #'cst:first definitions))
         (expansions (map 'list #'cst:second definitions)))
    (list* (map 'list #'walk names)
           (loop for expansion in expansions collect `(:literal ,expansion))
           (walk-body body environment))))

(define-walker-form (tagbody . body) (cst environment)
  (let ((sections ()))
    (loop for section = (cons NIL ())
          while (cst:consp body)
          for item = (cst:first body)
          do (cond ((cst:atom item)
                    ;; Finish section
                    (unless (equal section '(().()))
                      (setf (cdr section) (nreverse (cdr section)))
                      (push section sections))
                    (setf section (cons item ()))
                    (let ((tag (cst:raw item)))
                      (unless (typep tag '(or symbol integer))
                        (error "~@<Tag must be a symbol or integer, not ~S.~@:>" tag))
                      (when (find tag sections :key (compose #'cst:raw #'car))
                        (error "~@<Repeated tag: ~S.~@:>" tag))))
                   (T
                    (push item (cdr section))))
             (setf body (cst:rest body)))
    (setf sections (nreverse sections))
    (let* ((names (loop for section in sections
                        when (car section)
                        collect (cons (cst:raw (car section)) :tag)))
           (env (augmented-environment environment names (mapcar (constantly T) names))))
      (list (loop for section in sections
                  when (car section)
                  collect `(:label ,(cst:source cst)
                                   ,(cst:raw (car section))))
            (loop for section in sections
                  collect `(:abstraction ,(cst:source cst)
                                         ()
                                         ,(walk (first (cdr section)) env)))))))

(define-walker-form (the type form) (cst)
  (list `(:type ,(cst:source type) ,(cst:raw type))
        (walk form)))

(define-walker-form (throw tag result) (cst)
  (list `(:tag ,(cst:source tag) ,(cst:raw tag))
        (walk result)))

(define-walker-form (unwind-protect protected . cleanup) (cst)
  (list* (walk protected)
         (walk-implicit-progn cleanup)))

