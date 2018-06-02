(staple:load-system-quietly :staple-server)

(defmethod staple:packages ((system (eql (asdf:find-system :staple))))
  (list (find-package :staple)))

(defmethod staple:packages ((system (eql (asdf:find-system :staple-markdown))))
  (list))

(defmethod staple:packages ((system (eql (asdf:find-system :staple-package-recording))))
  (list (find-package :staple-package-recording)))

(defmethod staple:packages ((system (eql (asdf:find-system :staple-server))))
  (list (find-package :staple-server)))

(defmethod staple:packages ((system (eql (asdf:find-system :staple-code-parser))))
  (list (find-package :staple-code-parser)))

(defmethod staple:documents ((system (eql (asdf:find-system :staple-server))))
  (list (asdf:system-relative-pathname system "README.md")))

(defmethod staple:documents ((system (eql (asdf:find-system :staple))))
  (list (asdf:system-relative-pathname system "README.md")))
