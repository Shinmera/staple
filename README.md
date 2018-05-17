## About Staple
Staple is yet another Common Lisp documentation generation application. It uses [Clip](http://shinmera.github.io/clip) and [lQuery](http://shinmera.github.io/lquery) to do most of the work. The main features include automatic package symbol index processing, documentation gathering from related files and generation through expressive templates.

## How To
Load Staple through ASDF or Quicklisp:

```(ql:quickload :staple)```

Generating documentation is a single command away:

```(staple:generate :my-asdf-system)```

This should create an about.html file within the source directory of the asdf-system you specified. By default staple uses the same name as the asdf system for the package index, but you may specify your package/s directly too if they differ using the `:PACKAGES` argument.

Staple automatically searches your source directory for documentation files (like README.md) to include into the finished file. You may however specify your own file or a string directly with the `:DOCUMENTATION` argument. Staple currently only parses Markdown files to HTML, but you may extend it with other formats by adding a `parse-documentation-file` method.

The template used to generate everything is a [Clip](http://shinmera.github.io/clip) document. Understanding how it works and throwing together your custom layout to use should not be hard. For a full understanding of the system however please read the Clip documentation.

## Extending Staple
Staple can be extended to handle other "symbol types" and to automatically behave different for specific systems. In order to do so you should create a `staple.ext.lisp` file within the root source directory of the system you want to have extensions for. This file will automatically be loaded when you `generate` documentation for your system.

In order to add a new type of symbol, you should subclass one of the `symb` classes. Depending on what kind of thing your type of symbol is, different classes may be appropriate. You should then add methods that specialise on your subclass to at least `symb-documentation` and `symb-type-order`. There are other methods that give you more fine-grained control over how the symbols are represented.

Finally you'll need to tell Staple how to find your new type of symbol when it scans over packages. To do so, you should use `define-converter` or `define-simple-converter`. Converters are functions that take a symbol and a package, and return a list of `symb` objects that apply for the given symbol and package combination.

This is all it takes for Staple to produce new symbol types in the index.

If you want to customise the default arguments to `generate`, or add additional properties for usage in the template document, you can add a method to `system-options` that specifies these properties.

    (defmethod staple:system-options append ((system (eql (asdf:find-system :my-sys))))
      (list :template (system-relative-pathname system \"my-clip-template.ctml\")
            :if-exists :supersede))

See the documentation of `system-options` for the default properties that Staple understands and supplies. If you need to customise which symbol objects are emitted into the documentation file for a given package and system, you can add a method to `system-package-symbols` to control this behaviour.

    (defmethod staple:system-package-symbols ((system (eql (asdf:find-system :my-sys))) package)
      (remove-if ... (staple:package-symbol-objects package)))

Note that symbols will still get filtered out depending on the value of the `exclude` attribute in the `do-symbols` tag in the template which, by default, excludes methods, internal symbols, and inherited symbols.

In general the `staple.ext.lisp` should allow you to lay down all the customisation of your system so that all you'll ever need to do to actually generate the docs is

    (ql:quickload '(staple my-system))
    (staple:generate :my-system)

and Staple will figure out everything automatically.

## Documentation Browser
Staple includes a server so that you can view the documentation and symbol index of all available ASDF systems on the fly.

    (ql:quickload :staple-server)
    (staple-server:start)

Depending on how many systems you have loaded, starting the server may take a while as it produces a cache of all documentation pages. Once it's done, visit the [url in the message](http://localhost:8080/). If you change the systems and want to view the updated documentation, use `staple-server:recache`.

## Git and Github config

As Staple produces a large html file, tools using [Linguist](https://github.com/github/linguist) such as Github may mark your repository as HTML, and not Lisp. You can ask Linguist to not count about.html in a `.gitattributes`:

    about.html linguist-vendored

