#|
This file is a part of Staple
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.staple)

;; clip.lisp
(docs:define-docs
  (variable *current-packages*
    "List of packages being processed by the template.
This is to used to figure out whether a symbol can be found on the 
documentation page when referring to it through RESOLVE-SYMBOL-DOCUMENTATION.")

  (function year
    "Return the current year")

  (function month
    "Return the current month")

  (function day
    "Return the current day of the month")

  (function licenselink
    "Returns an A tag linked to a TLDRLegal.com search on the license name.")

  (function present-improper-list
    "Present a list, even an improper one.

See PRESENT")

  (function present
    "Return a string representation of the thing that is appropriate for use in the documentation output.")

  (function present-qualifiers
    "Present the list of qualifiers in an appropriate way.

See PRESENT")

  (function present-arguments
    "Present the list of arguments in an appropriate way.

See PRESENT")

  (function string-starts-with
    "Returns T if the string starts with sub, NIL otherwise.")

  (function resolve-symbol-documentation
    "Attempts to resolve the (string) symbol to either an URL or an anchor.
This works by first testing against the package. If it is known (such as the
sb-*, mop, cl and *current-packages*) a link/anchor is returned. If nothing
can be found, NIL is returned instead. If no package designator is given,
the symbol is attempted to be automatically found in either the
*current-packages* or in CL.")

  (function anchor
    "Returns a href-anchor.")

  (function stext
    "Same as lQuery's TEXT, but calls PRINC-TO-STRING on the object or uses an empty string on NIL.")

  (function parse-block-symbols
    "Attempt to find all relevant function call symbols in the HTML block and replace them with links to the corresponding documentation anchor.")

  (function parse-lone-symbols
    "Attempt to find all symbols that refer to a documented symbol and replace them with links to the corresponding documentation anchor. ")

  (function render-docstring-see-also
    "Renders the docstring plainly, turning 'See X' lines into links.")

  (function render-docstring-markdown
    "Renders the docstring as a markdown highlighted string.")

  (function render-docstring
    "Render the docstring appropriately for the given system.

See RENDER-DOCSTRING-SEE-ALSO
See RENDER-DOCSTRING-MARKDOWN")

  (function %is-excluded
    "Return T if the given symb is included in the list of excluded symbol types.

See SYMB-IS"))

;; fulltext.lisp
(docs:define-docs
  (variable *documentation-names*
    "A list of strings denoting common file names (without extension) for documentation files.
If you have your own file name, push it onto this list.

See *DOCUMENTATION-TYPES*
See FIND-DOCUMENTATION-FILE")

  (variable *documentation-types*
    "A list of strings denoting common file types/extensions for documentation files.
If you have your own file type, push it onto this list.

See *DOCUMENTATION-NAMES*
See FIND-DOCUMENTATION-FILE")

  (variable *logo-names*
    "A list of strings denoting commong file names (without extension) for logos.
If you have your own file name, push it onto this list.

See *LOGO-TYPES*
See FIND-LOGO-FILE")

  (variable *logo-types*
    "A list of strings denoting commong image types/extensions for logos.
If you have your own file type, push it onto this list.

See *LOGO-NAMES*
See FIND-LOGO-FILE")

  (function parse-documentation-file
    "Used to perform special parsing on certain documentation files (such as Markdown).
The type should be an EQL-specializer to a keyword of the file-type/extension.

By default only .md files are specially handled, everything else is simply read as a string.")

  (function find-documentation-file
    "Attempts to find a documentation file in the given asdf system's source directory.
This relies on *DOCUMENTATION-NAMES* and *DOCUMENTATION-TYPES* to find an appropriate file.

See *DOCUMENTATION-NAMES*
See *DOCUMENTATION-TYPES*")

  (function prepare-documentation
    "Attempts to prepare the documentation for the given system.
In the case of a pathname, PARSE-DOCUMENTATION-FILE is called.
If the doc is NIL, a matching documentation file is attempted to be found through
FIND-DOCUMENTATION-FILE. If nothing is foudn for that as well, an empty string is
returned instead.")

  (function find-logo-file
    "Attempts to find a logo file in the given asdf system's source directory.
See *LOGO-NAMES* and *LOGO-TYPES*. The system will also try to find files by 
prepending or appending the system name to the logo names.

See *LOGO-NAMES*
See *LOGO-TYPES*"))

;; stapler.lisp
(docs:define-docs
  (variable *extension-file*
    "Pathname describing the filename for Staple extension files.")
  
  (variable *modern-template*
    "Pathname to a simple, yet modern preset template.")

  (variable *legacy-template*
    "Pathname to a rather plain and simple preset template that was used before the modern one.")

  (variable *default-template*
    "Pathname to the default template to use in GENERATE.")

  (variable *root-clipboard*
    "Side-storage in order to allow accessing of the lower-level clipboard 
from within different clipboard environments.

See ROOT")

  (variable *before-load-packages*
    "Map associating ASDF:SYSTEM objects to the package list recorded before the first time the system was loaded.")

  (variable *system-packages*
    "Map associating ASDF:SYSTEM objects to recorded packages for the system.

See SYSTEM-PACKAGES")

  (function efind-package
    "Attempts to find a package of the given name.

Unlike FIND-PACKAGE, this also tries the name in all uppercase, and if
both attempts with the regular name and uppercased name fail, it signals
an error.")

  (function system-packages
    "Accessor to the list of packages associated to the system.

If the loading process of the system happened before staple was loaded, a
heuristic is used where a package name is returned that corresponds to the
system's name. If the system was loaded after staple, this should return
the precise list of packages that the system defined.

If no corresponding package name could be found, an error is signalled.

See *SYSTEM-PACKAGES*")

  (function root
    "Shorthand for (CLIP *ROOT-CLIPBOARD* FIELD)

See CLIP:CLIP
See *ROOT-CLIPBOARD*")

  (function to-out
    "Returns a pathname whose file-name (not extension) is postfixed by .out .")

  (function system-out
    "Returns a pathname to 'about.html' within the given system's source-directory.")

  (function compact
    "Compact the given plump node by stripping away as much potentially useless whitespace in inner text nodes as possible.")

  (function staple
    "Performs stapling actions/clip processing on the IN document.

IN is parsed by PLUMP:PARSE and the results are written to OUT.
Through CLIP-ARGS additional arguments can be passed to CLIP:GENERATE.
These will also appear in the *ROOT-CLIPBOARD*.")

  (function generate
    "Generates documentation for the given asdf-system.

If the system is not already loaded, it is loaded.
If there is an extension file within the system's root
directory, that file is also loaded. See *EXTENSION-FILE*

ASDF-SYSTEM    --- The name or object of the ASDF system to write documentation for.
PACKAGES       --- A list of package names to documentate.
NAME           --- The name of the project.
DOCUMENTATION  --- A string or pathname that contains additional documentation info.
LOGO           --- A string or URL to the logo to use in the template. If not supplied
                   no logo image is inserted.
OUT            --- The file to write the resulting documentation to.
TEMPLATE       --- Pathname to the clip template file to process.
COMPACT        --- Whether to strip leading and trailing whitespace where sensible.
IF-EXISTS      --- Argument for WITH-OPEN-FILE."))

;; symbols.lisp
(docs:define-docs
  (function symb-symbol
    "Returns the actual symbol backed by the symbol object.")
  
  (function symb-package
    "Returns the symbol-package of the symbol.")

  (type symb-object
    "Base class for symbol representation.")

  (type symb-type
    "Object representing a type.")
  
  (type symb-variable
    "Object representing a variable.")
  
  (type symb-function
    "Object representing a function.")
  
  (type symb-accessor
    "Object representing an accessor.")

  (type symb-macro
    "Object representing a macro.")

  (type symb-generic
    "Object representing a generic function.")

  (type symb-method
    "Object representing a generic function method.")

  (type symb-condition
    "Object representing a condition.")

  (type symb-class
    "Object representing a class.")

  (type symb-structure
    "Object representing a structure.")

  (type symb-special
    "Object representing a special variable.")

  (type symb-constant
    "Object representing a constant.")

  (function symb-true-symbol
    "Returns the the true symbol of the symbol.
Preferable over SYMB-SYMBOL as it takes SETF-function names into account.")

  (function symb-name
    "Returns the symbol-name of the symbol.")

  (function symb-function
    "Returns the symbol-function of the symbol.")

  (function symb-type
    "Returns the string-name of the kind of object it represents.")

  (function symb-scope
    "Returns whether the symbol is :INHERITED, :EXTERNAL or :INTERNAL.")

  (function symb-qualifiers
    "Returns the qualifiers of the method or NIL.")

  (function symb-arguments
    "Returns the arguments of the function or NIL.")

  (function symb-documentation
    "Returns the documentation-string.")

  (function symb-is
    "Checks if the symbol matches the mask.
The mask should be a keyword of either :INHERITED, :INTERNAL, :EXTERNAL
or one of the symb-object types.")

  (function symb<
    "Used to sort symbols alphabetically.
Special treatment is done so that generic functions should
always appear before their methods.")

  (function symb-type-order
    "For a given symbol type name, returns an integer representing the priority of the type in an ordering.")

  (function symb-type<
    "Used to sort symbols alphabetically, grouped by their type.")

  (function symbol-function-p
    "Returns T if the symbol is a pure function.")

  (function symbol-setf-function-p
    "Returns T if the symbol is a setf function.")

  (function symbol-accessor-p
    "Returns T if the symbol is a function and setter.")

  (function symbol-macro-p
    "Returns T if the symbol is a macro.")

  (function symbol-generic-p
    "Returns T if the symbol is a generic function.")

  (function symbol-constant-p
    "Returns T if the symbol is a constant.")

  (function symbol-special-p
    "REturns T if the symbol is a special variable.")

  (function symbol-structure-p
    "Returns T if the symbol is a structure.")

  (function symbol-condition-p
    "Returns T if the symbol is a condition.")

  (function symbol-class-p
    "Returns T if the symbol is a class.")

  (variable *converters*
    "Hash table to contain the converter functions.")

  (function converter
    "Accessor to the converter function associated with the name.
Each converter function takes two arguments, a symbol and a package,
and must return a list of symb-object instances.")

  (function remove-converter
    "Remove the named converter.

See CONVERTER")

  (function define-converter
    "Shorthand to easily define a converter function.

See CONVERTER")

  (function define-simple-converter
    "Shorthand for the most common definitions.
If TEST function passes, a single symbol object constructed from OBJECT-CLASS
is returned as a list.")

  (function package-symbols
    "Gets all symbols within a package.")

  (function symbol-objects
    "Gathers all possible symbol-objects out of the list of passed symbols.")

  (function package-symbol-objects
    "Gathers all possible symbol-objects of the given package."))
