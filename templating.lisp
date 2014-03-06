#|
 This file is a part of Staple
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.staple)

(defvar *recognized-blocks* '())

(defgeneric process-block (name element)
  (:documentation "Process a documentation block."))

(defmacro define-block-processor (name (elementvar &rest data-attributes) &body body)
  (let ((namekey (intern (string-upcase name) "KEYWORD"))
        (namegens (gensym)))
    `(progn
       (pushnew ,namekey *recognized-blocks*)
       (defmethod process-block ((,namegens (eql ,namekey)) ,elementvar)
         (let ,(loop for attr in data-attributes
                     collect `(,attr ($ ,elementvar (attr ,(intern (format NIL "DATA-~a" attr) "KEYWORD")) (node))))
           ,@body)))))

(defun generate (in &optional out)
  (let ((*lquery-master-document*))
    ($ (initialize in))
    (process)
    (let ((out (or out
                   (when ($ "html" (attr :data-output))
                     (merge-pathnames ($ "html" (attr :data-output) (node)) in))
                   in)))
      ($ (write-to-file out))
      out)))

(defun process (&optional (document *lquery-master-document*))
  (let ((*lquery-master-document* document))
    ;; Sorted in order of depth to avoid clashing.
    (loop for element in (sort ($ "*[data-block]") #'> :key #'(lambda (el) (length (lquery-funcs:nodefun-parents el))))
          do (when-let ((type ($ element (attr :data-block) (node))))
               (unless (string= "" type)
                 (when-let ((type (find-symbol (string-upcase type) "KEYWORD")))
                   (process-block type element)))))))

(define-block-processor asdf (element name)
  (when (and name (not (string= "" name)))
    (when-let ((system (asdf:find-system name)))
      ($ element ".name" (text (asdf:component-name system)))
      ($ element ".version" (text (asdf:component-version system)))
      ($ element ".license" (text (asdf:system-licence system)))
      ($ element ".author" (text (asdf:system-author system)))
      ($ element ".homepage" (text (asdf:system-homepage system)))
      ($ element ".description" (text (asdf:system-description system))))))

(define-block-processor package (element name exclude)
  (let* ((template ($ element ".symbol" (node)))
         (container ($ template (parent))))
    ($ template (remove))
    ($ container (append (generate-symbol-entries template name exclude)))))

(define-block-processor attribution (element attribution-system)
  )

(define-block-processor documentation (element name)
  ($ element "code"
    (each #'(lambda (element)
              ($ element (html (cl-ppcre:regex-replace-all
                                (format NIL "~a:([^\\s]+)" name)
                                ($ element (text) (node))
                                (format NIL "<a href=\"#\\1\">~a:\\1</a>" name)))))))
  )

(defun generate-symbol-entries (template package exclude)
  (let ((data ())
        (exclude (split-sequence #\Space exclude)))
    (loop for symbol in (sort (package-symbols (string-upcase package)) #'string> :key #'string)
          do (loop for symb-object in (symbol-objects symbol)
                   do (unless (or (member (string (symb-scope symb-object)) exclude :test #'string-equal)
                                  (member (string (symb-type symb-object)) exclude :test #'string-equal))
                        (let ((template ($ template (clone) (node))))
                          (process-symbol-object template symb-object)
                          (push template data)))))
    data))

(defgeneric process-symbol-object (template symbol-object)
  (:documentation ""))

(defun html-escape (text)
  (flet ((sr (search replace target)
           (cl-ppcre:regex-replace-all search target replace)))
    (sr "&" "&amp;"
        (sr "<" "&lt;"
            (sr ">" "&gt;"
                (princ-to-string text))))))

(defmethod process-symbol-object (template symb)
  ($ template ".anchor" (attr :name (string-downcase (symb-symbol symb))))
  ($ template ".anchor-link" (attr :href (format NIL "#~a" (string-downcase (symb-symbol symb)))))
  ($ template ".name" (text (symb-symbol symb)))
  ($ template ".scope" (text (symb-scope symb)))
  ($ template ".type" (text (symb-type symb)))
  ($ template ".qualifiers" (text (format NIL "~@[~{~a~^ ~}~]" (symb-qualifiers symb))))
  ($ template ".arguments" (text (or (symb-argslist symb) "")))
  ($ template ".documentation" (text (or (symb-docstring symb) ""))))
