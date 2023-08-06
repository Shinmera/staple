(defpackage #:staple-package-recording
  (:nicknames #:org.shirakumo.staple.recording)
  (:use #:cl)
  (:export
   #:packages
   #:package-system))

(in-package #:org.shirakumo.staple.recording)

(defvar *before-load-packages* (make-hash-table :test 'eql))
(defvar *system-packages* (make-hash-table :test 'eql))

(defun efind-package (name)
  (or (find-package name)
      (find-package (string-upcase name))
      (error "No package with name ~s could be found." name)))

(defmethod packages ((system asdf:system))
  (let ((packages (gethash system *system-packages* :not-recorded)))
    (cond ((eql :not-recorded packages)
           ;; Heuristic, ech.
           (let ((pkg (find-package (asdf:component-name system))))
             (when pkg (list pkg))))
          (T
           packages))))

(defmethod packages ((system symbol))
  (packages (asdf:find-system system T)))

(defmethod (setf packages) (packages (system asdf:system))
  (let ((packages (loop for package in packages
                        collect (etypecase package
                                  (package package)
                                  ((or string symbol) (efind-package package))))))
    (setf (gethash system *system-packages*) packages)))

(defmethod (setf packages) (packages system-ish)
  (setf (packages (asdf:find-system system-ish T)) packages))

(defmethod package-system ((package package))
  (loop for system being the hash-keys of *system-packages*
        for packages being the hash-values of *system-packages*
        when (find package packages) return system))

(defmethod package-system (thing)
  (package-system (or (find-package thing)
                      (error "No such package ~s." thing))))

;; Record all packages before system load
(defmethod asdf:perform :after ((o asdf:prepare-op) (s asdf:system))
  (when (eql :not-recorded (gethash s *before-load-packages* :not-recorded))
    (setf (gethash s *before-load-packages*) (list-all-packages))))

;; Difference recorded list against current list to get all packages defined.
(defmethod asdf:perform :after ((o asdf:load-op) (s asdf:system))
  (let ((old-packages (gethash s *before-load-packages* :not-recorded)))
    (when (and (not (eql :not-recorded old-packages))
               (eql :not-recorded (gethash s *system-packages* :not-recorded)))
      (let ((new-packages (set-difference (list-all-packages) old-packages)))
        ;; Combine with previous ones to account for potential package addition
        ;; after later reloading of the system.
        (setf (packages s)
              (union (packages s) (reverse new-packages)))))))
