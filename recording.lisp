#|
 This file is a part of Staple
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:staple-package-recording
  (:nicknames #:org.shirakumo.staple.recording)
  (:use #:cl)
  (:export
   #:system-packages))

(in-package #:org.shirakumo.staple.recording)

(defvar *before-load-packages* (make-hash-table :test 'eql))
(defvar *system-packages* (make-hash-table :test 'eql))

(defun efind-package (name)
  (or (find-package name)
      (find-package (string-upcase name))
      (error "No package with name ~s could be found." name)))

(defmethod system-packages ((system asdf:system))
  (let ((packages (gethash system *system-packages* :not-recorded)))
    (cond ((eql :not-recorded packages)
           ;; Heuristic, ech.
           (let ((pkg (find-package (asdf:component-name system))))
             (when pkg (list pkg))))
          (T
           packages))))

(defmethod system-packages ((system symbol))
  (system-packages (asdf:find-system system T)))

(defun (setf system-packages) (packages system)
  (let ((system (etypecase system
                  (asdf:system system)
                  ((or symbol string) (asdf:find-system system T))))
        (packages (loop for package in packages
                        collect (etypecase package
                                  (package package)
                                  ((or string symbol) (efind-package package))))))
    (setf (gethash system *system-packages*) packages)))

;; Record all packages before system load
(defmethod asdf:perform :after ((o asdf:prepare-op) (s asdf:system))
  (setf (gethash s *before-load-packages*) (list-all-packages)))

;; Difference recorded list against current list to get all packages defined.
(defmethod asdf:perform :after ((o asdf:load-op) (s asdf:system))
  (let ((old-packages (gethash s *before-load-packages* :not-recorded)))
    (unless (eql old-packages :not-recorded)
      (let ((new-packages (set-difference (list-all-packages) old-packages)))
        ;; Combine with previous ones to account for potential package addition
        ;; after later reloading of the system.
        (setf (system-packages s)
              (union (system-packages s) (reverse new-packages)))))))
