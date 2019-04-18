## About
This system adds support for [Markless](https://shirakumo.github.io/markless) syntax in both documents and docstrings. When this system is loaded, Staple will automatically scan for documents with the ending `mess` to use as primary documents on inferred pages. It also adds the source-compiler `:markless` that can be used to markup documentation source.

To use Markless for docstrings as well, use a customisation like this on your page type:

    (defmethod staple:format-documentation ((docstring string) (page my-page))
      (let ((*package* (first (staple:packages page))))
        (staple:markup-code-snippets-ignoring-errors
         (staple:compile-source docstring :markless))))
