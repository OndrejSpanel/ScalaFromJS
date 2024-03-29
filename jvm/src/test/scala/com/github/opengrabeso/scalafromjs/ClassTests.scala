package com.github.opengrabeso.scalafromjs

import org.scalatest.funsuite.AnyFunSuite

class ClassTests extends AnyFunSuite with TestUtils {

  test("Simple JS 1.8 (ES 5) class") {
    exec check ConversionCheck(rsc("types/simpleClass.js"))
      .required(
        "class Person",
        """person = new Person("Bob", "M")"""
      )
      .forbidden(".prototype.")
  }

  test("Harmony (ES 6) class with inheritance") {
    exec check ConversionCheck(rsc("types/harmonyClass.js"))
      .required(
        "class Employe",
        "class Janitor",
        "extends Person",
        "extends Employee",
        "def nameGetter =",
        "def nameFunc() =",
        "val localVar = age",
        "def constructor(name: String, age: Double, salary: Double)",
        "class Person(var name: String, var age: Double)",
        "def printEmployeeDetails() ="
      )

  }

  test("Infer member types defined in constructor or functions") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      function C() {
          this.a = 0;
      }
      C.prototype.constructor = C;
      C.prototype.f = function () {
          this.b = "b"
      }
      """).required(
        "class C",
        "var a: Double = 0",
        "var b: String"
      )

  }

  test("Infer constructor parameter types backward from variable usage") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      function C(a) {
        var b;
        this.value = a
      }

      C.prototype.constructor = C;
      var c = new C;
      var d = "";
      d = c.value;
      """).required(
      "class C(a: String)",
      "def constructor(a: String)"
    )

  }


  test("Handle getters / setters") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      function C() {
      }

      C.prototype = {
        get v() {
          return this._v
      },
      set v(vv) {
        this._v= vv
       }
      };

      var c = new C();
      var cv;

      c.v = "Value";
      if (true) {
        cv = c.v
      }
      """).required(
      "def v = ",
      "def v_=(",
      "var cv: String"
    )

  }

  test("Support local classes") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      var v1, v2;
      if (true) v1 = (function () {
              function C() {
                  this.a = 0;
                  this.x = 0;
              }
              C.prototype.constructor = C;
              var c1 = new C();
              return c1.a;
      })();

      if (true) v2 = (function () {
              function C() {
                  this.x = "X";
                  this.x = ""
              }
              C.prototype.constructor = C;
              var c2 = new C();
              return c2.x;
      })();
      """).required(
        "class C",
        "var a: Double = 0",
        "var x: String = \"X\"",
        "var v1: Double",
        "var v2: String"
      )
  }

  test("Support local objects") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      function f() {
        var pars = {
            a: 0,
            b: 1,
            c: 2
        };
        return pars.a;
      }

      function g() {
        var pars = {
            x: "X",
            y: "Y",
            z: "Z"
        };
        return pars.x;
      }
      var v1, v2;
      if (true) v1 = f();
      if (true) v2 = g();
      """).required(
      "object pars",
      "var a = ",
      "var x = "
    )
  }

  test("Create a class when constructor is returning a value") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      function C() {

        var list = {};

        function priv( id ) {}

        function get( id ) {
          var buffer = list[ id ];
          priv(id);
          return buffer;
        }

        function update( geometry ) {}

        function proc( geometry ) {}

        return {
          get: get,
          update: update,
          process: proc,
          calc: function ( t ) {return t * t;}
        };
      }

      var w = new C;
      """).required(
        "class C",
        "var list",
        "def get(",
        "def process(",
        "def calc(",
        "t * t"
      )
  }

  test("Handle constructor parameters when constructor is returning a value") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      function C(gl) {
        function get() {return gl[""]}

        return {
          get: get
        };
      }

      var w = new C;
      """).required(
        "class C(var gl: Map["
      ).forbidden(
        "var gl ="
      )
  }

  test("Handle local classes") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      function C() {

        function L() {
        }

        L.prototype.constructor = L;
      }

      C.prototype.constructor = C;
      """).required("class C", "class L").forbidden("def C","def L")


  }


  test("Inherit constructor") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      class Person {
          constructor(name) {
              this.name = name;
          }
      }

      class Man extends Person {
      }

      let bob = new Man('Bob');
      """).required(
      "class Person(",
        "class Man(", "extends Person("
      ).forbidden(
        "class Man extends"
      )


  }

  test("Handle override for member functions") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      class Person {
          constructor(name) {
              this.name = name;
          }

          set(name) {
              this.name = name;
          }
          clone() {
              return new Person(name);
          }

          myClone() {
              return new Person(name);
          }

      }

      class Employee extends Person {
          constructor(name, salary ) {
              super(name);
              this.salary = salary;
          }

          set(name, salary) {
              this.name = name;
              this.salary = salary;
          }

          myClone() {
              return new Employee(name, salary);
          }
      }

      let bob = new Employee('Bob', 1000);
      """).required(
      "def set(",
      "override def clone()",
      "override def myClone()"
    ).forbidden(
      "override def set("
    )


  }



  test("Handle unknown classes and classes in a package") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      var undef;
      var defined;
      var unknown;
      var known;

      class Known {
      }
      function init() {
        undef = new PACKAGE.Undef();
        defined = new PACKAGE.Defined();
        unknown = new Unknown();
        known = new Known();
      }

      PACKAGE.Defined = function ( object ) {

        this.object = object;
      };

      new PACKAGE.Defined
      """).required(
        //"var undef: PACKAGE.Undef",
        //"var defined: PACKAGE.Defined",
        "var undef: Undef",
        "var defined: Defined",
        "var unknown: Unknown",
        "var known: Known"
      ).forbidden(

      )


  }

  test("Handle properties defined for classes") {
    exec check ConversionCheck(
      // language=JavaScript
      """
        class Foo {
          constructor() {
            Object.defineProperty( this, 'isFoo', { value: true } );
            this.value = "";
          }
        }
      """).required(
      "class Foo",
      "def isFoo()"
    ).forbidden(
      "defineProperty"
    )
  }

  test("Handle instanceof properties defined for classes") {
    exec check ConversionCheck(
      // language=JavaScript
      """
        class Foo {
          constructor() {
            Object.defineProperty( this, 'isFoo', { value: true } );
            this.value = "";
          }
        }
        function f() {
            var c = new Foo();
            if (c.isFoo) {}
        }
        var ScalaFromJS_settings = {
            members: [
                {
                    cls: ".*",
                    name: "is(.*)",
                    operation: "instanceof"
                }]
        };
      """).required(
      "class Foo",
      "case c_cast: Foo" // simple if replaced with a match / case
    ).forbidden(
      "defineProperty",
      ".isFoo"
    )
  }

  test("Handle instanceof values defined for classes") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      class Foo {
        constructor() {
          this.isFoo = true;
          this.value = "";
        }
      }
      function f() {
          var c = new Foo();
          if (c.isFoo) {}
      }
      var ScalaFromJS_settings = {
          members: [
              {
                  cls: ".*",
                  name: "is(.*)",
                  operation: "instanceof"
              }]
      };
    """).required(
      "class Foo",
      "case c_cast: Foo" // simple if replaced with a match / case
    ).forbidden(
      "var isFoo", "isFoo = true", // the variable and its initialization should be removed
      ".isFoo"
    )
  }

  test("Handle constructor call on a class") {
    exec check ConversionCheck(
      // language=JavaScript
      """
        class A {
        }

        var a = new A();
        var b;
        if (true) {
          b = new a.constructor();
        }
      """).required(
      "var b: A",
    )
  }


}
