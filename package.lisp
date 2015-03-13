#|
 This file is a part of Staple
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:staple
  (:nicknames #:org.tymoonnext.staple)
  (:use #:cl #:lquery #:closer-mop #:clip)
  (:shadowing-import-from
   #:cl #:defmethod #:defgeneric #:standard-generic-function
        #:standard-method #:standard-class)
  ;; clip.lisp
  (:export
   #:year
   #:month
   #:day
   #:licenselink
   #:resolve-symbol-documentation
   #:anchor
   #:stext)
  ;; documentation.lisp
  (:export
   #:*documentation-names*
   #:*documentation-types*
   #:parse-documentation-file
   #:find-documentation-file
   #:prepare-documentation)
  ;; stapler.lisp
  (:export
   #:*default-template*
   #:to-out
   #:system-out
   #:staple
   #:generate)
  ;; symbols.lisp
  (:export
   #:symb-object
   #:symb-symbol
   #:symb-type
   #:symb-variable
   #:symb-function
   #:symb-accessor
   #:symb-macro
   #:symb-generic
   #:symb-method
   #:symb-class
   #:symb-structure
   #:symb-condition
   #:symb-special
   #:symb-constant
   
   #:symb-type
   #:symb-scope
   #:symb-qualifiers
   #:symb-arguments
   #:symb-documentation
   #:symb<
   #:symb-type<
   
   #:symbol-function-p
   #:symbol-setf-function-p
   #:smybol-macro-p
   #:symbol-generic-p
   #:symbol-constant-p
   #:smybol-special-p
   #:symbol-class-p
   #:symbol-structure-p
   #:symbol-condition-p

   #:package-symbols
   #:symbol-objects
   #:package-symbol-objects))
