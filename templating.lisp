#|
 This file is a part of Staple
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.staple)

(defvar *recognized-blocks* '())

(defgeneric process-block (name)
  (:documentation "Process a documentation block."))

(defun generate (in &optional out)
  (let ((*lquery-master-document*))
    (setf in
          (etypecase in
            (keyword (merge-pathnames "about.html" (asdf:system-source-directory in)))
            (list (merge-pathnames (second in) (asdf:system-source-directory (first in))))
            (pathname in)))
    ($ (initialize in))
    (clip:scan-element *lquery-master-document*)
    (let ((out (or out
                   (when ($ "html" (attr :data-output))
                     (merge-pathnames ($ "html" (attr :data-output) (node)) in))
                   in)))
      ($ (write-to-file out))
      out)))

(define-fill-processor asdf (element system) (asdf:find-system system)
  (name (element system) (asdf:component-name system))
  (version (element system) (asdf:component-version system))
  (license (element system) (asdf:system-license system))
  (author (element system) (asdf:system-author system))
  (homepage (element system) (asdf:system-homepage system))
  (description (element system) (asdf:system-description system)))

(define-iterating-fill-processor package (element package exclude)
    (let ((symbols (sort (package-symbols (string-upcase package)) #'string> :key #'string))
          (exclude (split-sequence #\Space exclude)))
      (loop with objects = ()
            for symbol in symbols
            do (loop for object in (symbol-objects symbol)
                     do (unless (or (member (string (symb-scope object)) exclude :test #'string-equal)
                                    (member (string (symb-type object)) exclude :test #'string-equal))
                          (push object objects)))
            finally (return objects)))
  (name (element symbol) (symb-symbol symbol))
  (scope (element symbol) (symb-scope symbol))
  (type (element symbol) (symb-type symbol))
  (qualifiers (element symbol) (format NIL "~@[~{~a~^ ~}~]" (symb-qualifiers symbol)))
  (arguments (element symbol) (or (symb-arguments symbol) ""))
  (documentation (element symbol) (or (symb-docstring symbol) "")))

(define-block-processor documentation (element package)
  ($ element "code"
    (each #'(lambda (element)
              ($ element (html (let ((html ($ element (html) (node))))
                                 (cl-ppcre:do-matches (start end (format NIL "~a:([^\\s]+)" package) html)
                                   (let ((name (subseq html (+ (length package) 1 start) end)))
                                     (setf html (concatenate
                                                 'string (subseq html 0 start)
                                                 (format NIL "<a href=\"#~a\">~a:~a</a>" (string-upcase name) package name)
                                                 (subseq html end)))))
                                 html))))))
  )
