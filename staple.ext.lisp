(staple:load-system-quietly :staple-server)

(defmethod staple:packages ((system (eql (asdf:find-system :staple))))
  (list (find-package :staple)))

(defmethod staple:packages ((system (eql (asdf:find-system :staple-markdown))))
  (list))

(defmethod staple:packages ((system (eql (asdf:find-system :staple-markless))))
  (list))

(defmethod staple:packages ((system (eql (asdf:find-system :staple-restructured-text))))
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

(defmethod staple:documents ((system (eql (asdf:find-system :staple-markdown))))
  (list (asdf:system-relative-pathname system "README.markdown.md")))

(defmethod staple:documents ((system (eql (asdf:find-system :staple-markless))))
  (list (asdf:system-relative-pathname system "README.markless.md")))

(defmethod staple:documents ((system (eql (asdf:find-system :staple-restructured-text))))
  (list (asdf:system-relative-pathname system "README.restructured-text.md")))
