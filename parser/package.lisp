(in-package #:cl-user)
(defpackage #:staple-code-parser
  (:nicknames #:org.shirakumo.staple.code-parser)
  (:use #:cl #:alexandria)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree))
  ;; environment.lisp
  (:export
   #:environment
   #:lookup
   #:augment-environment!
   #:augmented-environment)
  ;; to-definitions.lisp
  (:export
   #:find-definitions
   #:define-definition-resolver
   #:tie-to-source
   #:sub-results
   #:define-sub-results
   #:parse-result->definition-list)
  ;; walker.lisp
  (:export
   #:placeholder
   #:placeholder-name
   #:placeholder-package
   #:placeholder-intern
   #:walk
   #:walk-bindings
   #:walk-implicit-progn
   #:walk-body
   #:walk-lambda-like
   #:walk-atom
   #:walk-form
   #:define-walk-compound-form
   #:define-walker-form
   #:read-toplevel
   #:parse))
