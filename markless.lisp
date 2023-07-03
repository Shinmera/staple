(in-package #:org.shirakumo.staple)

(define-source-compiler (:markless "mess") (input)
  (cl-markless:output (cl-markless:parse input T)
                      :target (plump-dom:make-root)
                      :format 'cl-markless-plump:plump))
