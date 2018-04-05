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

  (function system-options
    "Returns options to use when stapling the given system.

The following options are accepted:

:asdf           --- The ASDF System object to generate. If this differs
                    from the argument to SYSTEM-OPTIONS, GENERATE is
                    called anew with the same arguments, but the system
                    being updated to the one in the options.
                      Defaults to the input system.
:compact        --- Whether to compact (remove whitespace) from the
                    resulting documentation file.
                      Defaults to T.
:documentation  --- The pathname to the primary documentation file to
                    output into the generated file's documentation
                    section.
                      Defaults to FIND-DOCUMENTATION-FILE
:logo           --- The relative path to a logo to include in the
                    generated file's header.
                      Defaults to FIND-LOGO-FILE relative to the source
                      directory of the system.
:name           --- The name to use for the system.
                      Defaults to ASDF:COMPONENT-NAME
:out            --- The pathname for the generated file.
                      Defaults to \"about.html\" relative to the source
                      directory of the system.
:packages       --- The list of packages to output to the symbol index
                    in the generated file.
                      Defaults to SYSTEM-PACKAGES
:template       --- The Clip template file to use to generate the
                    documentation file with.
                      Defaults to *DEFAULT-TEMPLATE*
:if-exists      --- What to do if the generated file already exists on
                    the file system.
                      Defaults to :ERROR

These options may be overridden by the arguments that are passed to
GENERATE.

The methods are combined by APPEND, with the most specific method coming
first. This means that you can override certain options by creating a
method for your system and outputting a plist with the option you want
to override.

For example, the following method would specify an alternate template
file to use, and a different exists behaviour.

  (defmethod staple:system-options append ((system (eql (asdf:find-system :my-sys))))
    (list :template (system-relative-pathname system \"my-clip-template.ctml\")
          :if-exists :supersede))

You may specify additional arguments that are not shown above. They will
all be passed to Clip's template generation, and may thus be accessed
from within the template.

See GENERATE
See FIND-DOCUMENTATION-FILE
See FIND-LOGO-FILE
See ASDF:COMPONENT-NAME
See ASDF:SYSTEM-SOURCE-DIRECTORY
See SYSTEM-PACKAGES
See *DEFAULT-TEMPLATE*
See CL:OPEN")

  (function system-package-symbols
    "Returns the applicable set of symb objects for the package under the given system.

This allows customising which symbs should be emitted for a given package
and system under construction. By default this simply includes all of the
symb objects for the package, produced by PACKAGE-SYMBOL-OBJECTS.

Note that the emitted symbols in the document may be filtered further still
by the use of the exclude attribute.

See SYMB-OBJECT
See PACKAGE-SYMBOL-OBJECTS
See DO-SYMBOLS")

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
directory, that file is also loaded. The extension files
of the transitive dependencies of the system are
loaded as well, before that of your specified system. 
This ensures that potential symbol classes they may
provide are also available to your system. For this to
work however, the transitive systems must use the default
location for their extension files.

The system may specify default arguments in its options.
The arguments specified for GENERATE will always override
those, however. The arguments' effects are explained in
SYSTEM-OPTIONS.

See SYSTEM-OPTIONS
See *EXTENSION-FILE*"))

;; symbols.lisp
(docs:define-docs
  (function symb-symbol
    "Returns the actual symbol backed by the symbol object.")
  
  (function symb-package
    "Returns the symbol-package of the symbol.")

  (type symb-object
    "Base class for symbol representation.

See SYMB-SYMBOL
See SYMB-PACKAGE
See SYMB-NAME
See SYMB-TYPE
See SYMB-ID
See SYMB-SCOPE
See SYMB-DOCUMENTATION
See SYMB-IS
See SYMB<
See SYMB-TYPE-ORDER
See SYMB-TYPE<")

  (type symb-type
    "Object representing a type.

See SYMB-OBJECT")
  
  (type symb-variable
    "Object representing a variable.

See SYMB-OBJECT")
  
  (type symb-function
    "Object representing a function.

See SYMB-FUNCTION
See SYMB-ARGUMENTS
See SYMB-OBJECT")
  
  (type symb-accessor
    "Object representing an accessor.

See SYMB-FUNCTION")

  (type symb-macro
    "Object representing a macro.

See SYMB-FUNCTION")

  (type symb-generic
    "Object representing a generic function.

See SYMB-FUNCTION")

  (type symb-method
    "Object representing a generic function method.

See SYMB-FUNCTION")

  (function symb-method
    "Accesses the method object that this symb-method represents.

See SYMB-QUALIFIERS
See SYMB-METHOD")

  (type symb-condition
    "Object representing a condition.

See SYMB-TYPE")

  (type symb-class
    "Object representing a class.

See SYMB-TYPE")

  (type symb-structure
    "Object representing a structure.

See SYMB-TYPE")

  (type symb-special
    "Object representing a special variable.

See SYMB-VARIABLE")

  (type symb-constant
    "Object representing a constant.

See SYMB-VARIABLE")

  (function symb-true-symbol
    "Returns the the true symbol of the symbol.
Preferable over SYMB-SYMBOL as it takes SETF-function names into account.

See SYMB-OBJECT")

  (function symb-name
    "Returns the symbol-name of the symbol.

See SYMB-OBJECT")

  (function symb-function
    "Returns the symbol-function of the symbol.

See SYMB-FUNCTION")

  (function symb-type
    "Returns the string-name of the kind of object it represents.

See SYMB-OBJECT")

  (function symb-id
    "Returns a string representing the symbol uniquely.

See SYMB-OBJECT")

  (function symb-scope
    "Returns whether the symbol is :INHERITED, :EXTERNAL or :INTERNAL.

See SYMB-OBJECT")

  (function symb-qualifiers
    "Returns the qualifiers of the method or NIL.

See SYMB-METHOD")

  (function symb-arguments
    "Returns the arguments of the function or NIL.

See SYMB-FUNCTION")

  (function symb-documentation
    "Returns the documentation-string.

See SYMB-OBJECT")

  (function symb-is
    "Checks if the symbol matches the mask.
The mask should be a keyword of either :INHERITED, :INTERNAL, :EXTERNAL
or one of the symb-object types.

See SYMB-OBJECT")

  (function symb<
    "Used to sort symbols alphabetically.
Special treatment is done so that generic functions should
always appear before their methods.

See SYMB-OBJECT")

  (function symb-type-order
    "For a given symbol type name, returns an integer representing the priority of the type in an ordering.

See SYMB-OBJECT")

  (function symb-type<
    "Used to sort symbols alphabetically, grouped by their type.

See SYMB-OBJECT")

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
    "Gathers all possible symbol-objects out of the list of passed symbols.

See SYMB-OBJECT")

  (function package-symbol-objects
    "Gathers all possible symbol-objects of the given package.

See SYMB-OBJECT"))
