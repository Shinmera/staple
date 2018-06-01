## About Staple-Parser
This system implements a Lisp code parser to implement marking up definition references within code snippets.

## How To
You can parse a lisp source snippets using `parse`:

    (staple-code-parser:parse "(defun foo (a) (+ 1 a))")

This will return a list of "parse results". Parse results represent all information about the toplevel source form that was parsed. Typically you will want to pass this to `parse-result->definition-list`, which will return a list of definitions and their source locations that were found within the parse results.

    (staple-code-parser:parse-result->definition-list *)
    ; => ((#<DEFINITIONS:FUNCTION +> (16 . 17)) (#<DEFINITIONS:MACRO DEFUN> (1 . 6)))

The definitions objects are from the [Definitions](https://shinmera.github.io/definitions) library. Please see its documentation on how to handle these kinds of objects. This definition list is used in Staple to mark up the respective source parts with HTML links, but you could also use it for your own purposes.

## Extending Staple-Parser
Since the parser does not compile or evaluate the code, it is missing a lot of information about what each symbol could be, hampering the quality of definition retrieval. You can help this out by implementing custom walkers for known forms that expand to parse results that are more easily understood.

The way to do this is twofold. You can either use `define-walk-compound-form` an expand into known parse results, transforming the contents as appropriate, or you can use `define-walker-form` to define a new parse result type. In the latter case you will also need to add `define-sub-forms` and `define-definition-resolver` to handle the traversal and lookup.

Have a look at the source files [special-forms](https://github.com/Shinmera/staple/blob/master/special-forms.lisp), [standard-forms](https://github.com/Shinmera/staple/blob/master/standard-forms.lisp), and [to-definitions](https://github.com/Shinmera/staple/blob/master/to-definitions.lisp) for examples on how to use these.
