Scala from JavaScript
=====================

Javascript to Scala.js converter.

This tools makes using JS examples from Web in Scala.js easier, with some effort it can assist in porting complete JS projects.

[Live version][3] - currently in alpha, use at your own risk.


Current version understands almost all common Javascript code and emits corresponding Scala counterparts.
Many Harmony (ES 2015) features are already supported:
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
- many other transformation converting JS constructs into more natural Scala ones

Class detection
---------------
The converter detects some common ways how JS objects are written using prototypes and converts them to
Scala classes. When not known otherwise, class types are derived from members used.

Type declarations from d.ts
---------------------------

Types can be read from d.ts files. To do so, the file must be referenced in the settings file as:

      var ScalaFromJS_settings = {
          types: "input.d.ts"
      };

Similar to js files, d.ts files can import or export other d.ts files.

Type inference
--------------

Number, string and boolean types are detected, class types are inferred and guessed. 

Exports / imports
-----------------
Multiple files may be converted, referencing each other with `export` / `import` statements.

Usage examples
--------------

When an import is preceded with a comment containing `@example`, it is considered as an usage example, and the
corresponding file is read, but not output. This can be used to help type inference by providing examples of how
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

[Scala port of Esprima][5] is used as a JS parser . The port was assisted by this tool. UglifyJS was used previously, running on Node.js. If you are
interested in that version, see tag `uglify-scala.js`. 

 
Command line usage
==================

`java -jar ScalaFromJS.jar temp/esprima/esprima-convert.ts temp/esprima/scala/esprima-convert.scala`
 
Conversion overview
===================

The conversion works in several phases:

- js files are read, imports and exports resolved by concatenation, creating one large source file
- the resulting file is parsed to create AST
- AST is transformed by many transformation steps described in variable `transforms` in `com.github.opengrabeso.scalafromjs.Transform.apply`
  - import transformation steps:
    - preprocess regex substitution: `com.github.opengrabeso.scalafromjs.ConvertProject.RegexPreprocessRule`
    - prototype to class conversion: `com.github.opengrabeso.scalafromjs.transform.classes.transforms`
    - type imports: `com.github.opengrabeso.scalafromjs.transform.TypesRule`
    - type inference: `com.github.opengrabeso.scalafromjs.transform.InferTypes.multipass`
    - postprocess regex substitution `com.github.opengrabeso.scalafromjs.ConvertProject.RegexPostprocessRule`

AST transformation
==================

There are two operations usually used to implement AST transformations: `walk` and `transform`. Both of them exist in a
variant handling a `ScopeContext`, which is needed for proper variable scope resolution.

One usually uses `walk` to gather any necessary information and `transform` to perform any work needed. There are two
flavors of `transform`: `transformBefore` and `transformAfter`, which differ in a traversal order: `transformAfter`
traverses all children first, while `transformBefore` is given a `descend` function and can determine the traversal order
as required.

Types
=====

Type information is stored outside of the AST, in `NodeExtended.types`. It is first constructed from TypeScript type
information (when available) and then type inference fills the rest. 

Symbols scopes and IDs
======================

Each symbol is made unique by pairing with its scope ID (`symbols.SymId`). The scope ID is the source code range of the
corresponding block (see `symbols.ScopeContext#getNodeId`). The processing can be see in `enterScope`.

There are some important cases:

- scope ID of the class is the scope ID of its body
- scope ID of the function is its corresponding Node.FunctionExpression or other `AnyFunEx` matching node, not of the
  function body, so that parameters have the same scope as the body
- scope ID of the for loop is the for statement, not the loop body, so that loop variable has the same scope as the body
- scope ID of Node.MethodDefinition is ID of its value (most often `Node.FunctionExpression`)

Class representation
====================

Classed are represented using `ClassDeclaration`. Member functions are listed in the class body as `MethodDefinition`s.
There is a special method called "inline^" which corresponds to the Scala primary constructor.

Class member variables
----------------------

Class member variables are represented using `MethodDefinition` (see `newValue`) with `kind = "value"`. 
They are also listed using `VariableDeclaration` / `VariableDeclarator` in the  "inline^" method.
 
  [3]: https://ondrejspanel.github.io/ScalaFromJS/
  [4]: https://en.wikipedia.org/wiki/Immediately-invoked_function_expression
  [5]: https://github.com/OpenGrabeso/esprima-scala
  
