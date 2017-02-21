Scala from JavaScript
=====================

Experimental project to see if automated or assisted conversion from JS to Scala.js is possible.

Minimal functionality is to make using JS examples from Web in Scala easier.

[Live version][3] - currently in alpha, use at your own risk.

Current version understands almost all common Javascript code and emits corresponding Scala counterparts.
Some Harmony (ES 2015) features are already supported, namely `let`, other will be added later.
Unsupported constructs (like `break` and `continue`) are emitted as they are, with a comment.

TODO:

- read parameter types from JavaDoc comments or Scala.js facades
- type inference for variables and parameters
- convert prototype based classes to Scala classes (at least when Scala.js facade exists)
- support Harmony class and arrow functions

If everything works well, larger goals may be added later, like automated or assisted conversion of larger JS projects.

Developer notes
===============

I have struggled a lot with including Uglify-js as a npm depencency in a way it can be used both from tests and from the browser page.

For the tests it was possible to use NPM (via [sbt-scalajs-bundler][1] plugin). I was however unable to make it
work this way in the browser. I gave up eventually and included the Ugligy script obtained by using:

`uglifyjs --self -o ./tmp/uglifyjs.js`, see [UglifyJS 2 browser build download Stack Overflow question][2]
 
 Note: use Harmony branch of uglifyjs.js, otherwise variable initialization detection is unreliable. I have produced
  the .js file from git, it might be also possible to use `npm install uglify-js-harmony`. 


 [1]: https://github.com/scalacenter/scalajs-bundler
 [2]: http://stackoverflow.com/questions/20027492/uglifyjs-2-browser-build-download
 [3]: https://ondrejspanel.github.io/ScalaFromJS/live/