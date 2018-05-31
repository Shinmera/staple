#|
 This file is a part of Staple
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:staple-server
  (:nicknames #:org.shirakumo.staple.server)
  (:use #:cl)
  (:export
   #:*tmpdir*
   #:cache-system
   #:clear-cache
   #:start
   #:stop))
