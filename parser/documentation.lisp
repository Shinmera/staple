#|
 This file is a part of Staple
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.staple.code-parser)

;; environment.lisp
(docs:define-docs
  (type environment
    "Container for environment information used during walking.

See PARENT
See NAMESPACES
See LOOKUP
See AUGMENT-ENVIRONMENT!
See AUGMENTED-ENVIRONMENT")

  (function parent
    "Returns the parent of the environment, if any.

See ENVIRONMENT")

  (function namespaces
    "Returns the hash-table of namespaces in the environment.

See ENVIRONMENT
See NAMESPACE")

  (function namespace
    "Accesses the namespace of the given name from the environment.

See NAMESPACES
See ENVIRONMENT
See LOOKUP")

  (function ensure-namespace
    "Makes sure the namespace of the given name exists in the environment.

See ENVIRONMENT
See NAMESPACES")

  (function lookup
    "Looks up the name in the namespace of the environment.

This will traverse the environment chain upwards until no parent can
be found anymore in case the current environment's namespace does not
contain the value.

When used as a setf place the value is always stored in the given
environment's namespace.

See NAMESPACE
See ENVIRONMENT")

  (function augment-environment!
    "Augments the given environment with the new values for the given names.

Returns the modified environment.

See LOOKUP
See ENVIRONMENT")

  (function augmented-environment
    "Returns a new environment with the changed values in place.

The old environment is a parent to the new one.

See AUGMENT-ENVIRONMENT!
See ENVIRONMENT"))

;; to-definitions.lisp
(docs:define-docs
  (function find-definitions
    "Returns any matching definitions for the given parse result.

All parse results have the structure of (TYPE SOURCE . ARGS).
Thus you can simply destructure it and pass the arguments to this
function to retrieve its definitions.

See DEFINE-DEFINITION-RESOLVER")

  (function define-definition-resolver
    "Shorthand to define a find-definitions method and destructure the arguments of the parse result.

See FIND-DEFINITIONS")

  (function tie-to-source
    "Turns each def into a list of source and def.")

  (function sub-results
    "Returns all parse results that are sub-results of this parse result.

All parse results have the structure of (TYPE SOURCE . ARGS).
Thus you can simply destructure it and pass the arguments to this
function to retrieve its definitions.

See DEFINE-SUB-RESULTS")

  (function define-sub-results
    "Shorthand to define a sub-results method and destructure the arguments of the parse result.

See SUB-RESULTS")

  (function parse-result->definition-list
    "Turn the parse-result into a list of definitions and source locations.

For instance:
  ((:CALL (0 . 10) (:VARIABLE (1 . 5) NULL) (:LITERAL (6 . 9) NIL)))
  => ((#<DEFINITIONS:FUNCTION NULL> (1 . 5)))

This uses FIND-DEFINITIONS to find suitable definitions for a parse
result, as well as SUB-RESULTS to traverse the parse result tree.

See FIND-DEFINITIONS
See SUB-RESULTS"))

;; walker.lisp
(docs:define-docs
  (type client
    "Our subclass of the eclector cst-client.

Uses the host lisp's EVAL.

See ECLECTOR.CONCRETE-SYNTAX-TREE::CST-CLIENT")

  (type placeholder
    "This class represents symbols that are not present in the host.

They are emitted in parsed code snippets in place of symbols that
cannot be read properly.

See PLACEHOLDER-NAME
See PLACEHOLDER-PACKAGE
See PLACEHOLDER-INTERN")

  (function placeholder-name
    "Returns the symbol-name of the symbol this is a placeholder for.

See PLACEHOLDER")

  (function placeholder-package
    "Returns the symbol-package name of the symbol this is a placeholder for.

See PLACEHOLDER")

  (function placeholder-intern
    "Returns whether the symbol being read is an internal or external symbol.

See PLACEHOLDER")

  (function walk
    "Walks the given CST in the environment.

Should return a parse result structure.
Parse results are lists of the following form:

  PARSE-RESULT ::= (TYPE SOURCE . ARGS)
  TYPE         --- The type of the form we've walked. Typically this
                   is a symbol of the form itself, like LAMBDA, or a
                   keyword if a generic variant is encountered like
                   for :CALLs and :MACROs.
  SOURCE       ::= (START . END)
  ARGS         --- Additional arguments for the parse result,
                   including additional parse-results.

Generally see the overall concrete-syntax-tree system for explanations
on how to use this.

Note that you probably want to define a method on WALK-FORM instead,
as that is called automatically as appropriate for each CST:CONST-CST,
and WALK-ATOM is called for each CST:ATOM-CST.

See ENVIRONMENT")

  (function walk-bindings
    "Walk the set of LET bindings in the environment.

Returns a list of cons cells where the CAR is the variable definition
of the binding and the cdr is the parse result of the value.

See WALK")

  (function walk-implicit-progn
    "Walks the CST as a list of forms and returns the list of parse-results for each form.

See WALK")

  (function walk-body
    "Same as WALK-IMPLICIT-PROGN, but filters out declarations from the cst.

See WALK-IMPLICIT-PROGN")

  (function walk-lambda-like
    "Walk a lambda-like structure.

Parses the lambda-list and body forms appropriately and returns a
parse-result for a lambda. The given parser is used to process the
lambda-list.

See WALK-IMPLICIT-PROGN")

  (function walk-atom
    "Walks an atom.

If the atom is a symbol, it returns a parse result of a literal for
keywords and booleans, or a variable for symbols. For everything else
it returns a parse result for a literal.")

  (function walk-form
    "Walks a form.

The form is identified by the car of the cons. The entirety of the
form as a CST, including the operator, are passed along as well.")

  (function define-walk-compound-form
    "Shorthand to define a WALK-FORM method.

Adds local functions for WALK and WALK-IMPLICIT-PROGN that
automatically pass the environment along so you don't need to repeat
it.

See WALK-FORM")

  (function define-walker-form
    "Shorthand to define simple walker forms.

The FORM should be a destructuring description of the kind of form to
walk. The return value of the BODY should be the list of additional
arguments for the parse result. The type and source of the parse
result are automatically added for you.

If you need control over the type or source, look at
DEFINE-WALK-COMPOUND-FORM instead.

See DEFINE-WALK-COMPOUND-FORM")

  (function read-toplevel
    "Reads the toplevel of an input.

The INPUT may be a string, pathname, or a stream (by default).
Returns a list of CSTs representing all toplevel forms that were read.")

  (function parse
    "Parses the input and returns a list of parse results, each for one toplevel.

First uses READ-TOPLEVEL to read all toplevel forms, then uses WALK
for each of the read CSTs to turn them into parse results.

See READ-TOPLEVEL
See WALK"))
