#|
This file is a part of Staple
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.staple)

;; code-format.lisp
(docs:define-docs
  (function markup-code-snippets-ignoring-errors
    "Marks up the code snippets ignoring parts that fail during markup.

See MARKUP-CODE-SNIPPETS")
  
  (function markup-code-snippets
    "Attempts to mark up the code snippets in the given HTML text.

This looks for <code> tags within the given HTML and will try to
automatically insert xref links. It performs transformations as
follows: Each <code> tag that is direct child to a <pre> tag is
transformed using MARKUP-CODE-BLOCK, with the intention that the
<code> element contains full code snippets. <code> tags without a
<pre> parent will be transformed using MARKUP-CODE-REFERENCE,
with the idea that these will only contain single symbol names that
represent direct references to definitions.

If an error occurs during the markup of a tag, a SKIP-TAG restart
will be available to allow skipping that particular tag.

The return value of this function is a PLUMP:NODE if the argument
is also a PLUMP:NODE, and a STRING if the argument is either a
STRING or a PATHNAME.

See MARKUP-CODE-BLOCK
See MARKUP-CODE-REFERENCE")

  (function markup-code-block
    "Transforms the node's content treating it as a code block.

Only the textual contents of the node are inspected, any other kinds
of tag that may be a child to the block will be removed by this.

All definitions that are recognised in this that have a result for
XREF will be marked up by an <a> tag with the class \"xref\".

Note that the value of *PACKAGE* matters for the parsing of the
code blocks, as a full READ and walk is performed on it to find
symbols and their usage in the code snippet.

Note also that if invalid code or non-lisp code is encountered an
error may be signalled by the reader.

See XREF
See STAPLE-CODE-PARSER:PARSE
See STAPLE-CODE-PARSER:PARSE-RESULT->DEFINITION-LIST")

  (function markup-code-reference
    "Transforms the node's content treating it as a definition reference.

Only the textual contents of the node are inspected, any other kinds
of tag that may be a child to the block will be removed by this.

This simply uses XREF on the textual content and if a result is
returned, inserts an <a> tag with the class \"xref\" to provide the
link.

See XREF"))

;; inference.lisp
(docs:define-docs
  (variable *document-patterns*
    "A list of regular expression patterns that recognise document files.

An expression in this list should match the filename of a file that
denotes a documentation body file.")

  (variable *image-patterns*
    "A list of regular expression patterns that recognise image files.

An expression in this list should match the filename of a file that
denotes an image file.")

  (variable *default-template*
    "Pathname to the default Clip template used for simple pages.

See SIMPLE-PAGE")

  (type simple-page
    "A simple page to base documentation on.

Simple-pages are the preferred pages to use for inferred systems.
They provide a convenient all-in-one package for a definitions index
and a documentation body.

A single \"document\" makes up the main body of the page and should
provide the primary documentation of the system or project. The
document is transformed by COMPILE-SOURCE, which by default will
automatically parse it from file and mark up code snippets within it.

If the OUTPUT pathname passed as initarg to this class is missing the
name and type, then the pathname is augmented using FILENAME of the
page.

See *DEFAULT-TEMPLATE*
See DOCUMENT
See IMAGES
See FILENAME
See SYSTEM-PAGE
See COMPILE-SOURCE")

  (function document
    "Accessor for the document the simple-page will include in its body.

This should be a pathname to a file that can be parsed by
COMPILE-SOURCE.

See SIMPLE-PAGE
See COMPILE-SOURCE")

  (function filename
    "Returns a suitable pathname making up the filename of the page.

By default for simple-pages this is the name \"index\" followed by
the language code of the page if the language is not \"en\" or \"eng\"
and the type \"html\".

See SIMPLE-PAGE")

  (function documents
    "Returns a list of pathnames to documents relevant for the given system.

By default this will attempt a heuristic by searching for files that
can be parsed by PATHNAME-TYPE->TYPE, and match one of the
*DOCUMENT-PATTERNS* within the system's source directory.

You may add a method specialising on a particular system to change
which documents are used for an inferred project.

See PATHNAME-TYPE->TYPE
See *DOCUMENT-PATTERNS*
See INFER-PROJECT")

  (function images
    "Returns a list of pathnames to images relevant for the given system.

By default this will attempt a heuristic by searching for files that
match one of the *IMAGE-PATTERNS* within the system's source directory.

You may add a method specialising on a particular system to change
which images are used for an inferred project.

See *IMAGE-PATTERNS*
See INFER-PROJECT")

  (function subsystems
    "Returns a list of systems that are related to the given system.

You may add a method specialising on a particular system to change
which subsystems are used for an inferred project.

See INFER-PROJECT")

  (function template
    "Returns the pathname to a Clip template suitable for the given system.

You may add a method specialising on a particular system to change
which template is used for an inferred project.

See INFER-PROJECT")

  (function page-type
    "Returns the type of the page that should be used for the given system's inferred project.

By default this returns 'SIMPLE-PAGE

You may add a method specialising on a particular system to change
which page-type is used for an inferred project.

See SIMPLE-PAGE
See INFER-PROJECT")

  (function output-directory
    "Returns the output directory to which documentation for the given system should be output.

By default this returns the \"doc/\" subdirectory within the system's
source directory.

You may add a method specialising on a particular system to change
where the resulting content is stored for an inferred project.

See ASDF:SYSTEM-SOURCE-DIRECTORY
See INFER-PROJECT")

  (type no-known-output-directory
    "Error signalled when no known output directory is available for a system.

See SYSTEM
See INFER-PROJECT")

  (function system
    "Accessor to the system the object is associated with.

See SYSTEM-PAGE
See NO-KNOWN-OUTPUT-DIRECTORY"))

;; page.lisp
(docs:define-docs
  (variable *page*
    "Variable bound to the current page during generation.

See PAGE
See GENERATE")

  (type page
    "Base class for all pages that can be generated as part of a project.

A page represents a single, well, page within the documentation for a
particular project. It should only produce a single output, which
is typically a file. A page should, if possible, only contain text in
a single language. This primary language is indicated by the page's
LANGUAGE slot and defaults to \"en\" for English.

If the output is a pathname, the behaviour on existing output may be
specified through the :IF-EXISTS argument to GENERATE, by default set
to :ERROR.

See TITLE
See LANGUAGE
See OUTPTU")

  (function title
    "Accessor to the title of a page.

The title should be a very short, unique identifier for the page
within the project. Pages that represent the same content but in
different languages should have the same titles. The title of a page
may be used as the name for a link to that page.

See PAGE")

  (function language
    "Accessor to the language of a page.

The language should be a two or three-letter short-code that uniquely
identifies the language. See the ISO-639 language codes for all
available options.

See PAGE")

  (function output
    "Accessor to the output of a page.

The output should be a STREAM-DESIGNATOR, meaning that it can be
resolved to a stream via ENSURE-STREAM. Typically it will be a
pathname pointing to the file into which the page's contents should be
stored.

See PAGE")

  (function generate
    "Generate the outputs of the given object.

The value returned by this function should be some kind of identifier
of the outputs that were generated by the call to this function.
In the case of a page, this will be the output file or result produced
by the page. In the case of a project, this will typically be the
project itself, and a list of the results from the pages it generated.

If this function is called with something that isn't a project or page
then the argument is treated as a potential ASDF:SYSTEM and is coerced
to one by ASDF:FIND-SYSTEM. It will then try to find a project for the
system by first consulting FIND-PROJECT, and then INFER-PROJECT. If
both FIND-PROJECT and INFER-PROJECT fail, an error is signalled.
Otherwise, GENERATE is called on the newly retrieved project.

See PAGE
See PROJECT
See *PAGE*
See FIND-PROJECT
See INFER-PROJECT")

  (type input-page
    "Superclass for pages that are generated using some kind of input.

See PAGE
See INPUT")

  (function input
    "Accessor to the input of the page.

The input should be a STREAM-DESIGNATOR, meaning that it can be
resolved to a stream via ENSURE-STREAM. Typically it will be a
pathname pointing to the file from which the page's input should be
ready.

See INPUT-PAGE")

  (type static-page
    "A static page that simply copies its input to its output.

This is useful for static files such as images and other resources.

See INPUT-PAGE")

  (type compiled-page
    "A compiled page that is created by translating some input file.

In order to handle the translation, COMPILE-SOURCE is used.
The output of COMPILE-SOURCE may be a PLUMP:NODE, a STRING, or an
\(unsigned-byte 8) vector. In the case of a PLUMP:NODE, the node is
first compressed using COMPACT if the :COMPACT argument to GENERATE
is non-NIL (default).

See INPUT-PAGE
See COMPILE-SOURCE")

  (type templated-page
    "Superclass for pages that are templated using Clip.

The template that Clip is run on is the INPUT of the page. The
template arguments are computed using the TEMPLATE-DATA generic
function.

The output of the page is compressed using COMPACT if the :COMPACT
argument to GENERATE is non-NIL (default).

See INPUT-PAGE
See TEMPLATE-DATA")

  (function template-data
    "Returns the arguments to CLIP:PROCESS that should be used for the page.

This should be a plistt containing the necessary data to compile the
template. Note that this generic function uses the APPEND method-
combination, meaning that you may add new keys by simply adding a new
method to this function. The method combination uses
:MOST-SPECIFIC-FIRST, and since plists short-circuit, you may also use
a method to override keys that less-specific methods may have set.

See TEMPLATED-PAGE")

  (function definitions-index-page
    "Superclass for pages that include a definitions index.

See PACKAGES
See FORMAT-DOCUMENTATION
See RESOLVE-SOURCE-LINK
See DEFINITION-WANTED_P
See DEFINITIONS")

  (function packages
    "Accessor to the list of packages associated with the instance.

This will always return a list of PACKAGE instances, not package
designators. If passed an ASDF:SYSTEM instance, it will return the
list of packages that were either recorded explicitly for the system
during loading, or were either inferred or explicitly set for the
system. If passed a DEFINITIONS-INDEX-PAGE instance, it will return
the list of packages that should be put into the index. For anything
else it will try to coerce the argument to an ASDF:SYSTEM via
ASDF:FIND-SYSTEM.

See ASDF:SYSTEM
See DEFINITIONS-INDEX-PAGE")

  (function format-documentation
    "Formats the definition according to the page's preferences.

This function should be called to retrieve fully formatted HTML to
use as the documentation for a given definition on a page.

By default this will call MAYBE-LANG-DOCSTRING on the definition and
the page's language to retrieve the raw docstring, and then call
FORMAT-DOCUMENTATION again with the docstring and the page.

If a string is passed, it will by default parse it in the \"See\"
style wherein each Line beginning with \"See\" is treated as a line
indicating a source cross-reference. The rest of the line is
interpreted as a designator for another definition and is turned into
an xref link if possible.

If you would prefer other documentation styles, you should add a
method specialising on a custom page type, then use that page type in
your project.

See DEFINITIONS-INDEX-PAGE
See MAYBE-LANG-DOCSTRING
See XREF")

  (function resolve-source-link
    "Resolve the link to a source file to a URI.

The source should be either a definition or a source spec. In case of
a definition, the function is called again with the source spec as
computed by ABSOLUTE-SOURCE-LOCATION on DEFINITIONS:SOURCE-LOCATION.
A source spec should be a plist of the following possible keys:

  :FILE  --- An absolute pathname to the source file. Required.
  :ROW   --- An optional row/line to which to point within the file.
  :COL   --- An optional col/char to which to point within the line.

By default this will try a \"best effort\" resolution, meaning
relative links if the file's path is a subpath of the page's output.
Otherwise it will fall back to a \"file://\" link.

See DEFINITIONS:SOURCE-LOCATION
See ABSOLUTE-SOURCE-LOCATION")

  (function definition-wanted-p
    "This function should return T if the definition should be included in the page's definitions index.

See DEFINITIONS-INDEX-PAGE")

  (function definitions
    "This function should return a list of applicable definitions for the given page and package.

By default this will simply compute /all/ definitions in the package
and only keeping wanted ones by DEFINITION-WANTED-P.

The returned list of definitions is always sorted in the natural order
as described by SORT-DEFINITIONS.

See DEFINITION-WANTED-P
See SORT-DEFINITIONS")

  (type system-page
    "Superclass for pages that represent and ASDF system.

This system will compute several properties automatically by using the
ASDF metadata: if the :PACKAGES are not given, they are computed from
calling PACKAGES on the system object. When a source link is resolved
and the project's homepage resides on GitHub, it will try to guess a
link to the GitHub repository's viewer of the source file.

See SYSTEM
See DEFINITIONS-INDEX-PAGE"))

;; project.lisp
(docs:define-docs
  (variable *load-prohibited-systems*
    "A list of ASDF:SYSTEM instances that should not be loaded for extensions.

This is a curated list of special systems that cause problems when
being loaded as part of the LOAD-EXTENSION mechanism.

See LOAD-EXTENSION")

  (type project
    "Superclass for a documentation project.

A project encapsulates all documentation for a library or program.
Typically this is expressed by a number of PAGEs that will create the
expected documentation files when the project is GENERATEd.

See PAGE
See PAGES
See GENERATE")

  (function pages
    "Returns the list of pages that the project generates.

See PAGE
See PROJECT")

  (type simple-project
    "A simple project.

This class simply stores a list of page instances and generates them
when GENERATE is called on the project instance.

See PROJECT
See PAGES")

  (function extension-file
    "Returns the Staple extension source file for the ASDF:SYSTEM.

By default this is a file called \"staple.ext.lisp\" within the
system's source directory.

This function may return NIL to indicate that the system has no
extension file.

See ASDF:SYSTEM-SOURCE-DIRECTORY")

  (function find-project
    "Find and return the project for the given ASDF:SYSTEM.

If you want to define a custom project for your system, you should
add a method specialising on your system instance to this function
and have it return the appropriately filled out project instance.

By default this will first call LOAD-EXTENSION on the system, and will
then call itself again if it can now find new methods specialising
either on the system's instance or its name as a keyword. Otherwise
it simply returns NIL.

See LOAD-EXTENSION")

  (function load-extension
    "Loads the extension file of the system.

This ensures that all Staple extensions and customisations that the
system might need are present and loaded when the documentation is
generated.

It proceeds as follows:
1. The argument is coerced to an ASDF:SYSTEM
2. If the system was already involved in a LOAD-EXTENSION call within
   this call tree, NIL is returned immediately.
3. Otherwise the system is loaded via ASDF:LOAD-SYSTEM, with warnings
   and standard-output muffled.
4. For each dependency registered for the system, LOAD-EXTENSION is
   called again to ensure dependant extensions are loaded first.
5. The EXTENSION-FILE, if present, is LOADed.
6. The system is returned.

See ASDF:LOAD-SYSTEM
See CL:LOAD")

  (function infer-project
    "Attempts to infer a project suitable for the given ASDF:SYSTEM.

By default this consults a variety of functions and attempts to build
a suitable SIMPLE-PROJECT instance that should document the system.
If you want to control how the documentation is generated, you may
either specialise the functions INFER-PROJECT uses, or construct your
own project entirely by specialising on FIND-PROJECT instead.

INFER-PROJECT proceeds as follows:
1. If no :OUTPUT-DIRECTORY is given, it is found via OUTPUT-DIRECTORY.
2. If no :DOCUMENTS are given, they are found via DOCUMENTS.
3. If no :IMAGES are given, they are found via IMAGES.
4. If no :PAGE-TYPE is given, it is found via PAGE-TYPE.
5. If no :TEMPLATE is given, it is found via TEMPLATE.
6. If no :PACKAGES are given, they are found via PACKAGES.
7. If no output directory is known, a recoverable error of type
   NO-KNOWN-OUTPUT-DIRECTORY is signalled. You may use the USE-VALUE
   restart to provide a new output directory.
8. For each pathname in the documents list a page of page-type is
   constructed, passing the template as :INPUT, the output directory
   as :OUTPUT, the system as :SYSTEM, the document's pathname as
   :DOCUMENT, and the list of images as :IMAGES.
9. If the documents list is empty, a single page of page-type is
   constructed with the same arguments as before, except the :DOCUMENT
   being NIL.
10. For each pathname in the images list a page of type STATIC-PAGE is
    constructed that will copy the image file into the OUTPUT-DIRECTORY
    while preserving pathname name and type.
11. A SIMPLE-PROJECT instance is constructed and returned with those
    pages as the :PAGES argument.

See OUTPUT-DIRECTORY
See DOCUMENTS
See IMAGES
See PACKAGES
See PAGE-TYPE
See TEMPLATE
See *DEFAULT-TEMPLATE*
See NO-KNOWN-OUTPUT-DIRECTORY
See CL:USE-VALUE
See STATIC-PAGE
See SIMPLE-PROJECT"))

;; transform.lisp
(docs:define-docs
  (function pathname-type->type
    "Returns a keyword for the given pathname-type, if it is known.

If ERRORP is non-NIL and no type can be found, an error is signalled.

When used as a setf-place, the value should be a list of pathname-type
strings that should be associated with the given type. Note that this
is overriding, meaning that previous associations to the given type
are removed.

See COMPILE-SOURCE
See DEFINE-SOURCE-COMPILER")

  (function compile-source
    "Compiles the source to a usable format, interpreting it as the given type.

The following argument combinations have specifically defined
behaviour:

  Source:  Type:        Explanation:
  --------------------------------------------------------------------
  PATHNAME (EQL T)  --- COMPILE-SOURCE is called again using the
                        pathname's pathname-type as type argument.
  PATHNAME       T  --- COMPILE-SOURCE is called again using the
                        contents of the file in string form as source
                        argument.
         T  STRING  --- COMPILE-SOURCE is called again using the type
                        returned by PATHNAME-TYPE->TYPE as the type
                        argument.

You should add methods to this function specialising on a particular
source type to handle the translation appropriately.

See PATHNAME-TYPE->TYPE
See DEFINE-SOURCE-COMPILER")

  (function define-source-compiler
    "Defines a new source compiler variant.

This is a shorthand that sets the PATHNAME-TYPE->TYPE association and
defines a new method on COMPILE-SOURCE to handle the conversion.
Thus, the TYPE should be a keyword identifying the source type, and
PATHNAME-TYPES should be an enumeration of known file types that are
used for this kind of conversion. You may also leave this empty if you
are defining a source type conversion that is not usually backed by
explicit files.

See PATHNAME-TYPE->TYPE
See COMPILE-SOURCE"))

;; toolkit.lisp
(docs:define-docs
  (function read-value
    "Reads a value from *query-io*.

To be used with interactive restarts.")

  (function split
    "Splits the given string by the given splitting character.

Returns the list of non-empty substrings. The split strings are not
shared with the original string.")

  (function with-value-restart
    "Ensures PLACE has a valid value.

This establishes a STORE-VALUE restart around BODY. If the restart is
called, the PLACE is set to the value the restart was called with and
the BODY is executed again.

Typically the BODY will be some kind of check to ensure the validity
of the value stored in PLACE. In combination with this macro, that
will ensure that the code only ever continues with a valid value in
PLACE.")

  (function ensure-system
    "Ensures to return an ASDF:SYSTEM.

If the argument is not already an ASDF:SYSTEM, it is passed to
ASDF:FIND-SYSTEM, and errors if no system can be found.

See ASDF:SYSTEM
See ASDF:FIND-SYSTEM")

  (function system-name
    "Returns a canonical keyword for the name of the ASDF:SYSTEM.")

  (function compact
    "Compacts the given PLUMP:NODE by removing extraneous whitespace.

This will take care not to trim whitespace from <pre> tags, but will
replace any duplicate whitespace with a single space anywhere else.
Note that this will leave a single space between nodes, as it cannot
determine whether the effects of the whitespace in that case are
intended or not.")

  (function case*
    "Like CL:CASE, but takes a test function.

TEST must be a function designator or a lambda expression.

See CL:CASE")

  (function map-directory-tree
    "Calls the given function for all files in the directory tree.

If MAX-DEPTH is given, it denotes the maximum number of directories
into which it recurses. Specifically, if MAX-DEPTH is 0, only the
files in the given directory are mapped to the function. Otherwise
each directory is visited recursively with MAX-DEPTH reduced by one.")

  (function do-directory-tree
    "Executes body for each file in the directory tree.

Evaluates RESULT last and returns its value.

See MAP-DIRECTORY-TREE")
  
  (function find-files
    "Find all files in the directory tree that match one of the patterns.

The patterns should be regular expressions suitable for CL-PPCRE. They
are matched against the file-namestrings of the files in the directory
tree.

See DO-DIRECTORY-TREE")

  (function read-file
    "Reads the given file to a string and returns it.")

  (function definition-id
    "Returns a string representing a unique ID for the given definition.

This is useful for creating links and anchors for definitions in a
document.")

  (function definition-order
    "Returns a number for the given definition, used for sorting.

The higher the number, the earlier the definition should appear in the
sorting.

By default, the following sorting is applied:

  DEFINITIONS:PACKAGE            200
  DEFINITIONS:CONSTANT           190
  DEFINITIONS:SYMBOL-MACRO       180
  DEFINITIONS:SPECIAL-VARIABLE   170
  DEFINITIONS:VARIABLE           160
  DEFINITIONS:CLASS              150
  DEFINITIONS:CONDITION          140
  DEFINITIONS:STRUCTURE          130
  DEFINITIONS:TYPE-DEFINITION    120
  DEFINITIONS:TYPE               110
  DEFINITIONS:ACCESSOR           100
  DEFINITIONS:FUNCTION            90
  DEFINITIONS:GENERIC-FUNCTION    80
  DEFINITIONS:METHOD              70
  DEFINITIONS:COMPILER-MACRO      60
  DEFINITIONS:MACRO               50
  DEFINITIONS:SETF-EXPANDER       40
  DEFINITIONS:CALLABLE            30
  DEFINITIONS:METHOD-COMBINATION  20
  DEFINITIONS:GLOBAL-DEFINITION   10
  DEFINITIONS:DEFINITION           0

See SORT-DEFINITIONS")

  (function sort-definitions
    "Sorts the list of definitions into a natural order.

Definitions of different type are sorted by DEFINITION-ORDER.
Definitions of the same type are sorted by STRING< using
DEFINITIONS:NAME as the key.

The sort is performed stably.

See DEFINITION-ORDER")

  (function definition-importance
    "Returns a number for the given definition, used to determine if one definition should receive precedence over another.

By default, the following sorting is applied:

  DEFINITIONS:CALLABLE           30
  DEFINITIONS:TYPE               20
  DEFINITIONS:VARIABLE           10
  DEFINITIONS:DEFINITION          0
  DEFINITIONS:METHOD            -10

See PREFERRED-DEFINITION")

  (function preferred-definition
    "Returns the list sorted such that the most important, or preferred definitions, come first.

See DEFINITION-IMPORTANCE")

  (function url-encode
    "Performs percent, or url-encoding of the string.")

  (function ensure-package-definition
    "Turns the given thing into a DEFINITIONS:PACKAGE.

If the thing cannot be coerced, an error is signalled.")

  (function ensure-package
    "Turns the given thing into a CL:PACKAGE.

If the thing cannot be coerced, an error is signalled.")

  (function skip-to-source-form
    "Continues reading from the stream until a valid lisp source form is encountered.

This skips both whitespace and comments.")

  (function absolute-source-location
    "Translates the given Definitions source-location into an absolute one.

This will read the source file to determine the absolute row/line and
col/char pointed at by the source-location. It returns another plist
of the following keys:

   :FILE    --- The same as in the input.
   :OFFSET  --- The absolute file-position offset.
   :ROW     --- The row/line of the offset.
   :COL     --- The col/char of the offset.

Returns NIL if the file cannot be found, the source-location is NIL,
or the file cannot be parsed.

See SKIP-TO-SOURCE-FORM")

  (function extract-language
    "Attempts to find a valid two or three-letter language code in the string.

If a code can be found, two values are returned: the code itself and
the list of names for the language the code points to.

See LANGUAGE-CODES:NAMES")

  (function maybe-lang-docstring
    "Attempts to find a docstring for the given definition in the given language.

If the multilang-documentation system is loaded, then this consults
MULTILANG-DOCUMENTATION:DOCUMENTATION using the DEFINITIONS:OBJECT and T as arguments, and alternatively the DEFINITIONS:DESIGNATOR and
DEFINITIONS:TYPE. If either the system is not loaded, or it fails to
return anything for both queries, it falls back to just returning the
DEFINITIONS:DOCUMENTATION.

See MULTILANG-DOCUMENTATION:DOCUMENTATION
See DEFINITIONS:DOCUMENTATION")

  (function ensure-stream
    "Attempts to coerce the given designator to a stream object.

The following are handled:

  STREAM           --- The designator is returned verbatim.
  STRING/PATHNAME  --- OPEN is called on the designator using the
                       given keyword arguments as appropriate.
  NULL             --- A string-output-stream is returned.
  T                --- *STANDARD-OUTPUT* is returned.

See WITH-STREAM
See STREAM-DESIGNATOR")

  (function finish-stream
    "Returns the stream's \"value\"

The following is returned:

  FILE-STREAM   --- The stream's pathname is returned.
  STRING-STREAM --- If the stream is an output stream, the stream's
                    string is returned. Otherwise the stream itself.
  T             --- The stream is returned.

See WITH-STREAM
See STREAM-DESIGNATOR")

  (function with-stream
    "Handles stream opening and closing, returning a useful value.

Essentially this calls ENSURE-STREAM on the designator and the args.
Upon unwinding, CLOSE is called on the stream. On successful exit of
the body, STREAM-VALUE is returned.

See ENSURE-STREAM
See STREAM-VALUE")

  (type stream-designator
    "A type representing all possible values to be used with ENSURE-STREAM.

See ENSURE-STREAM"))

;; xref.lisp
(docs:define-docs
  (function xref-resolver
    "Accessor to the cross-reference resolver of the given name.

The resolver should be a function of one argument, a definition
instance. It should return either NIL, or a URL string.

Default xref-resolvers for the current page that's being generated,
and the common-lisp package, are defined.

It is useful to define addition resolvers if you have some kind of
source of documentation that you would like to be able to link to.

See REMOVE-XREF-RESOLVER
See DEFINE-XREF-RESOLVER
See RESOLVE-XREF")

  (function remove-xref-resolver
    "Removes the cross-reference resolver of the given name.

See XREF-RESOLVER")

  (function define-xref-resolver
    "Defines a new cross-reference resolver function.

The lambda-list should accept one required argument, the definition
instance to find a cross-reference for.

See XREF-RESOLVER
See REMOVE-XREF-RESOLVER
See RESOLVE-XREF")

  (function resolve-xref
    "Calls each cross-reference resolver with the definition until one returns a valid reference.

See XREF-RESOLVER
See DEFINE-XREF-RESOLVER")

  (function parse-lisp-token
    "Parses a lisp symbol token, meaning it will read from string by properly interpreting backslashes and vertical bar escapes.")

  (function parse-symbol
    "Parses a symbol from the given string identifier.

Explicit packages, keywords, and package-less symbols are handled.

Returns two values, the symbol's name and its package as strings. 
If the symbol is a gensym, the returned package name is :GENSYM, 
rather than an actual package name string.")
  
  (function find-definitions-for-identifier
    "Attempts to find all definitions for the given symbol identifier.

The symbol is given in two parts -- as its name, and package.
The list of returned definitions may optionally be filtered by the
given type argument.

If no package is given, the definitions are attempted to be found in
the packages stored in *PAGE*, or the CL package.

See *PAGE*
See PACKAGES
See DEFINITIONS:FIND-DEFINITIONS")

  (function xref
    "Attempts to find a cross-reference URL for the given thing.

The following default cases are handled:

  DEFINITIONS:DEFINITION --- RESOLVE-XREF is called directly.
  STRING                 --- The string is parsed by PARSE-SYMBOL, and
                             if it represents a valid symbol, matching
                             definitions are attempted to be found via
                             FIND-DEFINITIONS-FOR-IDENTIFIER. The
                             definitions are ranked according to
                             PREFERRED-DEFINITION, and the first one
                             cross-reference via RESOLVE-XREF that can
                             be found is returned.

See RESOLVE-XREF
See PARSE-SYMBOL
See FIND-DEFINITIONS-FOR-IDENTIFIER
See PREFERRED-DEFINITION"))
