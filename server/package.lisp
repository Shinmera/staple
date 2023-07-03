(defpackage #:staple-server
  (:nicknames #:org.shirakumo.staple.server)
  (:use #:cl)
  (:export
   #:*tmpdir*
   #:cache-system
   #:clear-cache
   #:start
   #:stop))
