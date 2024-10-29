## About Staple
Staple is a documentation system. It provides you with a way to generate standalone documentation accumulated from various sources such as readmes, documentation files, and docstrings.

This project currently resides in [GitHub](https://github.com/Shinmera/staple)

## Quickstart

1. Simply load a REPL
2. Then load the projects

```common-lisp
(ql:quickload "staple")
(ql:quickload "my-system")
```
3. Finally

```lisp
(staple:generate :my-system)
```

And you are done!

You can make your own template for the docs. Here's an example of using the double-sidebar template:

```common-lisp
    (staple:generate :staple :if-exists :supersede :template  
        #P"./themes/double-sidebar/double-sidebar.ctml")
```

## How To
The most trivial manner in which to use staple is to simply run it and let its inference mechanism figure out how to document your system. You can do that like so:

```lisp
(staple:generate :my-system)
```

For best immediate results you should *not* load your system before you load Staple, so that Staple can record the packages the system defines as it is being loaded. If you later customise the documentation and set the packages explicitly, you don't have to watch out for that anymore, though. The documentation should land in a subdirectory called `doc` within the system's source directory. For ad-hoc usage you can change some of the behaviour through keyword arguments:

* `:output-directory` The base directory in which documentation files are stored.
* `:images` A list of paths to image files that might be used in the documentation. The first image is used as the project's logo.
* `:documents` A list of documents for which to generate documentation pages. This usually refers to things like `README.md` and such. Typically you would only have multiple of these if you have translated the documentation into different languages. In that case, Staple will create an index page for each language individually.
* `:page-type` The class to use for pages. See the sections below.
* `:template` The template file to use. If you want to customise what the documentation pages look like, you might want to change this to one of your own.
* `:if-exists` What to do if the output file already exists (see [open](http://clhs.lisp.se/Body/f_open.htm)).
* `:compact` Whether to compact the HTML files by trimming extraneous whitespace. Activated by default.
* `:packages` The packages that should be included in the definitions index.
* `:subsystems` A list of subsystems that are related to this primary system and should be included in the generation.

However, if you change any of these options, you will likely want to persist them somehow. The best way to do this is to use Staple's customisation mechanism. See the sections below for that.

You may also be interested in [Staple's server system](staple-server/), which gives you a live documentation browser for all systems currently loaded in your Lisp image.

## Templates 

Included templates currently reside in the `./themes/` directory.

To use one of the included templates simply pass the relevant file as an argument to the parameters of the `generate` function. Here's an example:

```common-lisp
    (staple:generate :my-system :if-exists :supersede :template  
        #P"./themes/double-sidebar/double-sidebar.ctml")
```

### Make Your Own Template 

To make your own template you can simply make a CTML file. See the existing templates for examples.

Note that the CSS Styles and JS can be included directly in the template itself.

### Included Templates 

#### Default

The default template will produce a left side panel with a Table Of Contents.

#### Double Left Side Panel

This template will produce a double left side panel where the left most side panel will have a list of systems, and the second side panel will have the regular Table Of Contents. 

In addition, the Table of Contents will list each available symbol under each package in the current system.

## Concepts
Staple organises itself around `project`s and `page`s. Every mechanism in it is an extension of those two concepts.

### Projects
Projects represent a, well, project. This is distinct from an ASDF system, as a project might encompass multiple systems. Similarly, an ASDF system fed to `generate` might generate documentation for multiple ASDF systems at once. While a project might represent multiple systems, it is always identified by at least one "primary" system.

In order to get the project for a primary system, you can use `find-project`. This will return NIL if no specific project is defined for a system. In that case you may also use `infer-project` to let Staple try and figure out a project for the system automatically.

Each project is composed of a number of pages. When a project is `generate`d, each of its `pages` are generated as well, producing the actual output of the project.

### Pages
A page represents an output that should be generated as part of a project. Typically this will be some kind of file, like an HTML document. A page has a specific `language` and a `title`. Pages with the same title should represent the same page, but in different languages. This allows you to write multilingual documentation. More on that later.

All you can do with a page, aside from inspecting its language, title, and `output`, is to `generate` it. For anything more advanced, you should have a look at its subclasses:

#### `input-page`
This is a primitive subclass of the page that denotes some kind of `input` that is being transformed when the page is generated. It should be used for anything that bases its output on some kind of input file or stream.

#### `static-page`
This page simply copies the input to the output verbatim, providing a way to define static files such as images and so forth. Since images and such resources are not really "pages" per se, this might be a strange fit, but by simply leaving the title `NIL`, you can use the same mechanisms regardless.

#### `compiled-page`
Compiled pages use Staple's `compile-source` mechanism, which translates source in some other format like Markdown into HTML. By default only text and HTML itself is supported, but you can trivially add other formats, or use the `staple-markdown` system to add Markdown support automatically.

#### `templated-page`
This kind of page uses the Clip system to perform a template expansion. If you want to use this kind of page, you should subclass it and add a method on `template-data` to supply the necessary data to the Clip template. See [Clip](https://shinmera.github.io/clip) for further information.

#### `definitions-index-page`
Often times you'll want to include a definitions index alongside the main documentation content. THis page adds additional support for this by including a list of `packages` to define, and several methods to aid in formatting the definitions, such as `format-documentation`, `resolve-source-link`, `definition-wanted-p`, and `definitions`. Note that this is a subclass of `templated-page`, meaning that if you want a definitions index, but don't want to use Clip, you'll need to do some work of your own.

#### `system-page`
This page adds some additional convenience when dealing with pages that document a specific ASDF system.

#### `simple-page`
Finally, the simple page is used for inferred projects and offers a base page for easy customisation. It provides sensible defaults for definition inclusion, template data, and so forth.

## Customisation
Customising Staple should happen through a file called `staple.ext.lisp` within your primary system's root source directory. This file is automatically loaded by Staple when the system is generated, making it convenient to add extra functionality.

There's two ways in which to customise how Staple generates the documentation for your system. You can either define your own project manually for maximum control, or influence the inference mechanism for some quick and easy changes.

### Custom Projects
Customising projects is easy to explain, as it simply involves adding a method to `find-project` specialising on your system's name that returns the readily made project instance.

```lisp
(defmethod staple:find-project ((system (eql (asdf:find-system :my-system))) &key)
  #|.. create project ..|#)
```

See the documentation for the different kinds of pages to see what you can do with them. One thing you should always respect is the `:output-directory` keyword argument, which should provide the root directory in which the documentation is stored. You can find a good default using the `output-directory` function on your system.

You should still read the following sections, as they will show examples on how to customise pages and what kinds of functions there are to influence behaviour so that you don't necessarily need to write everything from scratch unless you want to.

### Custom Inference
As mentioned in the How To section above, you can persist the different options you can pass to generate by changing the project inference. The following functions are called to determine the default values for the respective keyword arguments:

* `output-directory`
* `documents`
* `images`
* `page-type`
* `template`
* `packages`
* `subsystems`

In order to override these, just write a method specialising on your system:

```lisp
(defmethod staple:template ((system (eql (asdf:find-system :my-system))))
  (asdf:system-relative-pathname system #p"my-template.ctml"))
```

Some properties like the way documentation and docstrings are formatted require changing the way pages behave. For that, you can override the `page-type` similar to the above code snippet, and implement a custom page subclass as illustrated in the next section.

### Custom Pages
By subclassing `simple-page`, you can customise all sorts of behaviours.

```lisp
(defclass my-page (staple:simple-page) ())
```

Following are a few examples for things one might frequently want to change about the default behaviour of a page. If you are customising project inference, you can use `page-type` to use this page:

```lisp
(defmethod staple:page-type ((system (eql (asdf:find-system :my-system))))
  'my-page)
```

#### Changing Which Definitions are Shown
```lisp
(defmethod staple:definition-wanted-p ((_ definitions:method) (__ my-page)) T)
(defmethod staple:definition-wanted-p ((_ definitions:compiler-macro) (__ my-page)) T)
(defmethod staple:definition-wanted-p ((_ definitions:macro) (__ my-page)) NIL)
```

This will show methods and compiler-macros, but hide macros. By default all definitions for external symbols are shown except for methods, packages, compiler-macros, and declarations.

#### Including Additional Definitions
```lisp
(defmethod staple:definitions ((page my-page) package)
  (append (definitions:find-definitions 'cl:if)
          (call-next-method)))
```

This forces the definitions for `cl:if` to be included with the rest of the definitions for the packages of the page.

#### Changing Source Links
```lisp
(defmethod staple:resolve-source-link (source (page my-page))
  (format NIL "http://someplace/view-file/~a#line-~a" 
    (make-path-relative-somehow (getf source :file))
    (getf source :row)))
```

Note that by default, if you set the `:homepage` property in your ASDF system definition to a GitHub or GitLab project URL, it will try to automatically compute the URL to GitHub's or GitLab's file viewer.

#### Changing Docstring Formatting
```lisp
(defmethod staple:format-documentation ((docstring string) (page my-page))
  (let ((*package* (first (staple:packages page))))
    (staple:markup-code-snippets-ignoring-errors
     (staple:compile-source docstring :markdown))))
```

This will parse the docstring as Markdown and cross-reference all code snippets. Make sure to also load the `staple-markdown` system in your extension file.

#### Changing Document Formatting
```lisp
(defmethod staple:compile-source ((document pathname) (page my-page))
  (staple:compile-source document :text))
```

This will force the document to be parsed as raw text.

#### Changing the Filename
```lisp
(defmethod staple:filename ((page my-page))
  (make-pathname :name "foo" :type "html"))
```

This will force the file name of all pages to be `foo.html`.

#### Changing or Adding Template Data
```lisp
(defmethod staple:template-data append ((page my-page))
  (list :title "My Title"
        :generation-time (get-universal-time)))
```

Due to the `append` method-combination and the way `getf` works, this will override the `:title` value, and add the new `:generation-time` value which can now be referenced from the template.

#### Changing Generation Behaviour
```lisp
(defmethod staple:generate :after ((page my-page) &key)
  (format *debug-io* "~& Generated ~a.~%" page))
```

This adds a method that is called once the generation has completed, and simply prints a status message saying as much. You can use all the usual tricks of the standard method combination to customise things to your heart's content.

### Custom Templates
Writing a custom template is mostly a question of writing an HTML document that you want, and then filling in the necessary Clip attributes to add the data in the right spots. Figuring this out should be pretty trivial if you have a look at the existing [default template](https://github.com/Shinmera/staple/tree/master/default/default.ctml) and the [Clip documentation](https://shinmera.github.io/clip)

## An Example Customisation File
This is a simple example customisation file that changes the inferred project to use a custom markup syntax and package list.

```lisp
(asdf:load-system :staple-markdown)

(defclass my-page (staple:simple-page) ())

(defmethod staple:page-type ((system (eql (asdf:find-system :my-system))))
  'my-page)

(defmethod staple:packages ((system (eql (asdf:find-system :my-system))))
  (mapcar #'find-package '(:my-system :my-system-other)))

(defmethod staple:format-documentation ((docstring string) (page my-page))
  (let ((*package* (first (staple:packages page))))
    (staple:markup-code-snippets-ignoring-errors
     (staple:compile-source docstring :markdown))))
```

## Custom Global Definitions
Staple has support for documenting arbitrary definition types aside from the standard top level definition types that Common Lisp exposes. This is done through the [Definitions](https://shinmera.github.io/definitions) library. Please see its documentation on how to [add custom definitions](https://shinmera.github.io/definitions/#extending_definitions). You can write this extra glue code into your `staple.ext.lisp` file along with all the other Staple customisations. When a new definition type is defined, Staple will automatically try to find it and include it in your `simple-page`s. If you would like to be more selective, see `definition-wanted-p` above.

Also of interest are `definition-id`, `definition-order`, and `definition-importance`, which control the page anchors and order of appearance of definitions in an index.

## Github Actions
You can generate your documentation with Staple through a Github Action automatically, using a workflow file like this:

```yaml
name: documentation
on:
  push:
    branches:
      - main
      - master
permissions:
  pages: write
  id-token: write
jobs:
  publish:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v1
      - uses: shinmera/staple@v2.0.0
        with:
          gh-pages: true
          dist: http://dist.shirakumo.org/shirakumo.txt
```

The ``gh-pages`` argument means it'll try to push to your Github Pages automatically. If you disable that, you'll have to make sure to upload or otherwise distribute the documentation yourself. The ``dist`` argument lets you include an additional Quicklisp dist, if your project needs extra dependencies to be loaded. Other inputs you can pass to the action are:

- `project`  
  The project to compile documentation for. Defaults to the name of the repository.
- `output`  
  Where to put the documentation output. Defaults to `${{runner.temp}}/staple-output/`. This is also provided as an output variable.
- `template`  
  The Clip template to use for documentation pages.
