#|
 This file is a part of Staple
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.staple)

(defun markup-code-snippets (html)
  (let ((root (etypecase html
                (plump:node html)
                ((or string pathname) (plump:parse html)))))
    (flet ((markup (node)
             (restart-case
                 (if (string= "pre" (plump:tag-name (plump:parent node)))
                     (markup-code-block node)
                     (markup-code-reference node))
               (skip-tag ()
                 :report "Skip marking up the current tag."))))
      (lquery:$ root "code" (each #'markup))
      (etypecase html
        (plump:node root)
        ((or string pathname) (plump:serialize root NIL))))))

(defun make-xref-link (parent href content)
  (let ((link (plump:make-element parent "a")))
    (setf (plump:attribute link "href") href)
    (setf (plump:attribute link "class") "xref")
    (plump:make-text-node link content)
    link))

(defun markup-code-block (node)
  (let* ((text (plump:text node))
         (parse-result (staple-code-parser:parse text))
         (definitions (staple-code-parser:parse-result->definition-list parse-result))
         (definitions (remove-duplicates definitions :key #'cdr :test #'equal)))
    (plump:clear node)
    (loop for prev = 0 then end
          for (def loc) in (sort definitions #'< :key #'caadr)
          for (start . end) = loc
          for xref = (xref def)
          do (cond (xref
                    (plump:make-text-node node (subseq text prev start))
                    (make-xref-link node xref (subseq text start end)))
                   (T
                    (plump:make-text-node node (subseq text prev end))))
          finally (plump:make-text-node node (subseq text prev)))
    node))

(defun markup-code-reference (node)
  (let* ((content (plump:text node))
         (xref (xref content)))
    (when xref
      (plump:clear node)
      (make-xref-link node xref content))
    node))
