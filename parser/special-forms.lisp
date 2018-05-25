#|
 This file is a part of Staple
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.staple.code-parser)

(define-walk-compound-form eval-when (cst)
  (cst:db source (operator situations . body) cst
    (declare (ignore source operator situations))
    `(:eval-when ,(cst:source cst)
       ,@(walk-implicit-progn body))))

(define-walk-compound-form quote (cst)
  (let ((material (cst:second cst)))
    `(:literal ,(cst:source cst)
               ,(cst:raw material))))

(define-walk-compound-form the (cst)
  (cst:db source (operator type form) cst
    (declare (ignore source operator))
    `(:the ,(cst:source cst)
           (:type ,(cst:source type) ,(cst:raw type))
           ,(walk form))))

(define-walk-compound-form locally (cst environment)
  (cst:db source (operator . body) cst
    (declare (ignore source operator))
    ;; FIXME: parse declarations
    `(:locally ,(cst:source cst)
       ,@(walk-body body environment))))

(define-walk-compound-form function (cst)
  (cst:db source (operator function-designator) cst
    (declare (ignore source operator))
    (if (and (cst:consp function-designator)
             (find (cst:raw (cst:first function-designator))
                   '(lambda #+sbcl sb-int:named-lambda)))
        (walk function-designator)
        `(:function ,(cst:source cst)
          ,(cst:raw function-designator)))))

(define-walk-compound-form lambda (cst environment)
  (walk-lambda-like cst environment))

#+sbcl
(define-walk-compound-form sb-int:named-lambda (cst environment)
  `(:named-lambda ,(cst:source cst)
       (:variable ,(cst:source (cst:second cst)) ,(cst:raw (cst:second cst)))
     ,@(cddr (walk-lambda-like (cst:rest cst) environment))))

(define-walk-compound-form labels (cst environment)
  (cst:db source (operator definitions . body) cst
    (declare (ignore source operator))
    (let* ((definitions (cst:listify definitions))
           (names       (map 'list #'cst:first definitions)))
      `(:labels ,(cst:source cst)
         ,(map 'list #'walk names)
         ,(map 'list (lambda (definition)
                       (walk-lambda-like definition environment)) ; TODO wrong environment
               definitions)
         ,@(walk-body body environment)))))

(define-walk-compound-form flet (cst environment) ; TODO same as LABELS
  (cst:db source (operator definitions . body) cst
    (declare (ignore source operator))
    (let* ((definitions (cst:listify definitions))
           (names       (map 'list #'cst:first definitions)))
      `(:flet ,(cst:source cst)
         ,(map 'list #'walk names)
         ,(map 'list (lambda (definition)
                       (walk-lambda-like definition environment))
               definitions)
         ,@(walk-body body environment)))))

(define-walk-compound-form macrolet (cst environment) ; TODO same as LABELS
  (cst:db source (operator definitions . body) cst
    (declare (ignore source operator))
    (let* ((definitions (cst:listify definitions))
           (names       (map 'list #'cst:first definitions)))
      `(:macrolet ,(cst:source cst)
         ,(map 'list #'walk names)
         ,(map 'list (lambda (definition)
                       (walk-lambda-like definition environment))
               definitions)
         ,@(walk-body body environment)))))

(define-walk-compound-form symbol-macrolet (cst environment) ; TODO same as LABELS
  (cst:db source (operator definitions . body) cst
    (declare (ignore source operator))
    (let* ((definitions (cst:listify definitions))
           (names      (map 'list #'cst:first definitions))
           (expansions (map 'list #'cst:second definitions)))
      `(:symbol-macrolet ,(cst:source cst)
         ,(map 'list #'walk names)
         ,(loop for expansion in expansions collect `(:literal ,expansion))
         ,@(walk-body body environment)))))

(define-walk-compound-form if (cst)
  (cst:db source (operator test then . else) cst
    (declare (ignore source operator))
    (unless (cst:null (cst:rest else))
      (error "~@<IF only accepts one otherwise-form, but ~s was given in addition.~@:>"
             (cst:raw (cst:rest else))))
    `(:if ,(cst:source cst)
          ,(walk test)
          ,(walk then)
          ,@(unless (cst:null else)
              (list (walk (cst:first else)))))))

(define-walk-compound-form let (cst environment)
  (cst:db source (operator definitions . body) cst
    (declare (ignore source operator))
    (let ((definitions (walk-bindings definitions environment)))
      (multiple-value-bind (declarations forms)
          (cst:separate-ordinary-body body :listify-body nil)
        (declare (ignore declarations))
        ;; FIXME: parse declarations
        `(:let ,(cst:source cst)
           ,(mapcar #'car definitions)
           ,(mapcar #'cdr definitions)
           ,@(walk-implicit-progn forms))))))

(define-walk-compound-form let* (cst environment)
  (cst:db source (operator definitions . body) cst
    (declare (ignore source operator))
    (let ((definitions (walk-bindings definitions environment)))
      (multiple-value-bind (declarations forms)
          (cst:separate-ordinary-body body :listify-body nil)
        (declare (ignore declarations))
        ;; FIXME: parse declarations
        `(:let* ,(cst:source cst)
           ,(mapcar #'car definitions)
           ,(mapcar #'cdr definitions)
           ,@(walk-implicit-progn forms))))))

(define-walk-compound-form block (cst environment)
  (cst:db source (operator name . body) cst
    (declare (ignore source operator))
    (let ((name/raw (cst:raw name)))
      (unless (symbolp name/raw)
        (error "~@<Block name must be a symbol, not ~S.~@:>" name/raw))
      (let ((environment (augmented-environment environment `((,name/raw . :block-name)) '(t))))
        `(:block ,(cst:source cst)
           (:block-name ,(cst:source name) ,(cst:raw name))
           ,@(walk-implicit-progn body environment))))))

(define-walk-compound-form return-from (cst environment)
  (cst:db source (operator name value) cst
    (declare (ignore source operator))
    (let ((name/raw (cst:raw name)))
      (unless (symbolp name/raw)
        (error "~@<Block name must be a symbol, not ~S.~@:>" name/raw))
      (if-let ((name* (lookup name/raw :block-name environment)))
        `(:return-from (cst:raw cst)
           (:block-name ,(cst:source name*) ,(cst:raw name*))
           ,(walk value))
        (error "~@<Unknown block name: ~S.~@:>" name/raw)))))

(define-walk-compound-form tagbody (cst environment)
  (cst:db source (operator . body) cst
    (declare (ignore source operator))
    (let* ((sections (list (cons nil '())))
           (section  (first sections)))
      (labels ((visit (first rest)
                 (cond
                   ((cst:atom first)
                    (when (equal sections '((nil . nil)))
                      (setf sections '()))
                    (let ((tag/raw (cst:raw first)))
                      (unless (typep tag/raw '(or symbol integer))
                        (error "~@<Tag must be a symbol or integer, not ~S.~@:>" tag/raw))
                      (when-let ((other (car (find tag/raw sections
                                                   :key (compose #'cst:raw #'car)))))
                        (error "~@<Repeated tag: ~S.~@:>" tag/raw))
                      (setf (cdr section) (nreverse (cdr section))
                            section       (cons first '()))
                      (push section sections)))
                   (t
                    (push first (cdr section))))
                 (when (cst:consp rest)
                   (visit (cst:first rest) (cst:rest rest)))))
        (if (cst:consp body)
            (visit (cst:first body) (cst:rest body))
            (error "not implemented")))
      (setf (cdr section) (nreverse (cdr section)))
      (let* ((names (loop for section in sections
                          when (car section)
                          collect (cons (cst:raw (car section)) :tag)))
             (env (augmented-environment environment names (mapcar (constantly T) names))))
        `(:tagbody ,(cst:source cst)
           ,(loop for section in sections
                  when (car section)
                  collect `(:label ,(cst:source cst)
                                   ,(cst:raw (car section))))
           ,(loop for section in sections
                  collect `(:abstraction ,(cst:source cst)
                                         ()
                                         ,(walk (first (cdr section)) env))))))))

(define-walk-compound-form go (cst environment)
  (cst:db source (operator tag) cst
    (declare (ignore source operator))
    (let ((tag/raw (cst:raw tag)))
      (unless (typep tag/raw '(or symbol integer))
        (error "~@<Tag must be a symbol or integer, not ~S.~@:>" tag/raw))
      (if-let ((tag* (lookup tag/raw :tag environment))) ; TODO store the variable in the environment?
        `(:go ,(cst:source cst)
              (:label ,(cst:source cst) ,tag/raw))
        (error "~@<Unknown tag: ~S.~@:>" tag/raw)))))

(define-walk-compound-form progn (cst)
  (let ((rest (cst:rest cst)))
    (if (cst:null (cst:rest rest))
        (walk (cst:first rest))
        `(:progn ,(cst:source cst)
                 ,@(walk-implicit-progn rest)))))
