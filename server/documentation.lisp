(in-package #:org.shirakumo.staple.server)

(docs:define-docs
  (varaible *server-build*
    "Set to T around a generation that the server performs.

Used to make sure that the xref resolver of the server doesn't
interfere with generations that aren't for the server.")
  
  (variable *acceptor*
    "Holds the acceptor instance while the server is started.

See START
See STOP
See ACCEPTOR")

  (variable *tmpdir*
    "Pathname to the temporary file directory the server uses to store documentation output.

By default a subdirectory called \"staple-server\" in 
UIOP:TEMPORARY-DIRECTORY.

See SYSTEM-PATH")

  (function all-systems
    "Returns a sorted list of all ASDF:SYSTEMs.")

  (function data-file
    "Returns the pathname to a file in the server's data dir.")

  (function system-link
    "Returns the relative URL to an ASDF:SYSTEM's documentation.")

  (function system-path
    "Returns the pathname to output directory for the documentation of the given ASDF:SYSTEM.

See *TMPDIR*")

  (function find-system-in-path
    "Finds the matching ASDF:SYSTEM for the given URL path.

For cases where a system's name might be a prefix of the given path,
the system with the longest matching name is returned.")

  (function safe-prin1
    "PRIN1s the given thing to a string or returns a placeholder string if there is an error during printing.")

  (function or*
    "Same as CL:OR except that empty strings are treated as NIL.")

  (function cache-system
    "Creates the cache for the given system.

Unless otherwise specified, the output is placed in the directory
returned by SYSTEM-PATH. The system is generated normally otherwise,
but supplying :if-exists :supersede. After generation, each HTML file
in the output directory is modified such that links to file:// urls
are replaced by ones that the server can handle, and each page gets
the nav.ctml contents injected at the bottom of its body.

This should ensure that the documentation can be customised heavily
by the owner of the projects, but still work within a server setting.

See SYSTEM-PATH
See STAPLE:GENERATE")

  (function clear-cache
    "Clears the cache by deleting all files in *TMPDIR*

See *TMPDIR*")

  (type acceptor
    "Hunchentoot acceptor to implement the Staple server.

See HUNCHENTOOT:ACCEPTOR")

  (function start
    "Starts the Staple server.

If *ACCEPTOR* is already set, an error is signalled. Otherwise, a new
ACCEPTOR instance is created and started.

See *ACCEPTOR*
See ACCEPTOR")

  (function stop
    "Stops the Staple server.

If *ACCEPTOR* is not set, an error is signalled. Otherwise the server
is stopped and unbound.

See *ACCEPTOR*")

  (function serve-system-list
    "Returns the HTML for the systems list page.")

  (function serve-system-docs
    "Returns the HTML for the given documentation path under the system.")

  (function serve-source
    "Returns the HTML for the file browser to the given path.")

  (function serve-error
    "Returns the HTML for the error display page."))
