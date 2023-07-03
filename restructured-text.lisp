(in-package #:org.shirakumo.staple)

(defclass docutils-writer (docutils.writer.html:html-writer)
  ()
  (:default-initargs :parts '(docutils.writer.html:body-pre-docinfo body)))

(defmethod docutils:visit-node ((writer docutils-writer) (document docutils:document))
  (setf (slot-value writer 'docutils:parts)
        '(docutils.writer.html:body-pre-docinfo body)))

(define-source-compiler (:restructured-text "rst") (input)
  (docutils:register-settings-spec '((:generator NIL)
                                     (:datestamp NIL)))
  (docutils:write-document
   (make-instance 'docutils-writer)
   (docutils:read-rst input)
   'string))
