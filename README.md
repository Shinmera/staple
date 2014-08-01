About Staple
------------
Staple is yet another Common Lisp documentation generation application. It uses [Clip](http://shinmera.github.io/clip) and [lQuery](http://shinmera.github.io/lquery) to do most of the work. The main features include automatic package symbol index processing, documentation gathering from related files and generation through expressive templates.

How To
------
Load Staple through ASDF or Quicklisp:

```(ql:quickload :staple)```

Generating documentation is a single command away:

```(staple:generate :my-asdf-system)```

This should create an about.html file within the source directory of the asdf-system you specified. By default staple uses the same name as the asdf system for the package index, but you may specify your package/s directly too if they differ using the `:PACKAGES` argument.

Staple automatically searches your source directory for documentation files (like README.md) to include into the finished file. You may however specify your own file or a string directly with the `:DOCUMENTATION` argument. Staple currently only parses Markdown files to HTML, but you may extend it with other formats by adding a `parse-documentation-file` method.

The template used to generate everything is a [Clip](http://shinmera.github.io/clip) document. Understanding how it works and throwing together your custom layout to use should not be hard. For a full understanding of the system however please read the Clip documentation.
