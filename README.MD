Scala from JavaScript
=====================

Javascript to Scala.js converter.

This tools makes using JS examples from Web in Scala.js easier, but perhaps it might eventually be able to convert
even larger JS files.

[Live version][3] - currently in alpha, use at your own risk.

Current version understands almost all common Javascript code and emits corresponding Scala counterparts.
Some Harmony (ES 2015) features are already supported:
- `let` scoped variables
- arrow functions
- classes

Unsupported constructs (like `break` and `continue`) are emitted as they are, with a comment.
Argument types for functions are read from JSDoc `@param` comments. The tools also tries to infer types of arguments and
 variables.

Besides of converting JS syntax to Scala the tools performs severals transformations on the AST:

- increment / decrement unary operator handling
- join variable initialization with declaration
- remove [Immediately-invoked function expression][4]
- class detection
- infer types for many symbols (variables, functions and arguments)
- remove trailing `return` statements
- detect `val` variables

The most complicated parts are probably class detection and type inference.

Class detection
---------------
The converter detects some common ways how JS objects are written using prototypes and converts them to
Scala classes. When not known otherwise, class types are derived from members used.

Type inference
--------------

Currently number, string and boolean types are detected and class types as well. 

Exports / imports
-----------------
Multiple files may be converted, referencing each other with `export` / `import` statements.

Usage examples
--------------

When an import is preceeded with a comment containing `@example`, it is considered as an usage example, and the
correspoding file is read, but not output. This can be used to help type inference by providing examples of how
functions are used.

Transformation rules
--------------------

Source file may contain a variable called `ScalaFromJS_settings`. This variable may configure the conversion. Currently
it is possible to define following transformation rules:


      var ScalaFromJS_settings = {
          members: [
              {
                  cls: ".*",
                  name: "is(.*)",
                  operation: "instanceof"
              },{
                  cls: ".*",
                  name: "name",
                  operation: "make-property"
              },{
                  cls: ".*",
                  name: "exotic.*",
                  operation: "delete"
              },
          ]
      };

Rule `instanceof`
-----------------
Calls matching the name pattern, where the first matching group matches the class name, are converted to "isInstanceOf[]".

Rule `delete`
-------------
Definitions of the member functions or values matching the rule are deleted (usages of the member are left intact)

Rule `make-property`
--------------------
Value member matching the rule are replaced with a property. Note: this transformation may change code semantics, as the property value
is evaluated when the property is accessed. If expression defining the property depends on any mutable values, the result may be different.  

Developer notes
===============

I have struggled a lot with including Uglify-js as a npm depencency in a way it can be used both from tests and from the browser page.

For the tests it was possible to use NPM (via [sbt-scalajs-bundler][1] plugin). I was however unable to make it
work this way in the browser. I gave up eventually and included the Uglify script obtained by using:

`uglifyjs --self -o ./tmp/uglifyjs.js`, see [UglifyJS 2 browser build download Stack Overflow question][2]
 
Note: you need to use Harmony branch of uglifyjs.js, I have produced
the .js file from git version, it might be also possible to use `npm install uglify-js-harmony`. 


 [1]: https://github.com/scalacenter/scalajs-bundler
 [2]: http://stackoverflow.com/questions/20027492/uglifyjs-2-browser-build-download
 [3]: https://ondrejspanel.github.io/ScalaFromJS/live/
 [4]: https://en.wikipedia.org/wiki/Immediately-invoked_function_expression