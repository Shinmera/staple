(in-package #:org.shirakumo.staple)

(defun markup-code-snippets-ignoring-errors (html)
  (handler-bind ((error (lambda (e)
                          (format *debug-io* "~&WARN: Error during code markup: ~a" e)
                          (when (find-restart 'skip-tag)
                            (invoke-restart 'skip-tag)))))
    (markup-code-snippets html)))

(defun markup-code-snippets (html)
  (let ((root (etypecase html
                (plump:node html)
                ((or string pathname) (plump:parse html)))))
    (flet ((markup (node)
             (restart-case
                 (cond ((string= "pre" (plump:tag-name (plump:parent node)))
                        (markup-code-block node))
                       ((and (plump:first-element node)
                             (string= "pre" (plump:tag-name (plump:first-element node))))
                        (markup-code-block (plump:first-child node)))
                       (T
                        (markup-code-reference node)))
               (skip-tag ()
                 :report "Skip marking up the current tag."))
             T))
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

;; FIXME: this solves the issue of overlaps, but it's... not great for
;;        obvious reasons. We should be able to handle nested definitions!
(defun remove-overlaps (definitions)
  (let ((definitions (sort (copy-seq definitions) #'< :key #'caadr)))
    (loop for (definition . others) on definitions
          do (dolist (other others)
               (when (and (not (eq other definition))
                          (< (car (second other)) (cdr (second definition))))
                 (setf (cdr (second definition)) (car (second other)))))
             (when (<= (cdr (second definition)) (car (second definition)))
               (setf definitions (remove definition definitions))))
    definitions))

(defun markup-code-block (node)
  (let* ((text (plump:text node))
         (parse-result (staple-code-parser:parse text))
         (definitions (remove-overlaps (staple-code-parser:parse-result->definition-list parse-result))))
    (plump:clear node)
    (loop for prev = 0 then end
          for (def loc) in definitions
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
