package com.github.opengrabeso.scalafromjs

import org.scalatest.funsuite.AnyFunSuite

class MemberResolutionTests extends AnyFunSuite with TestUtils {
  test("Calling member function - return type") {
    exec check ConversionCheck(
      //language=JavaScript
      """
         class C {
          fun(par) {
            return par;
          }
         }

         var str;
         var cc;
         if (true) {
          var c = new C();
          cc = c;
          str = c.fun("");
        }
      """).required(
      "var str: String",
      "def fun(par: String)",
    )
  }


  test("Proper scopes of member / global functions") {
    exec check ConversionCheck(
      //language=JavaScript
      """
         class C {
          fun(par) {
            return par;
          }
          callingMember() {
            this.strMember = this.fun("") // should call the member one
            this.xStrMember = "";
          }
          callingGlobal() {
            this.numMember = fun(0); // should call the global one
            this.xNumMember = 0;
          }
         }

         function fun(par) {
          return par;
         }

         var num;
         var str;
         var cc;
         if (true) {
          var c = new C();
          cc = c;
          num = fun(0);
          str = c.fun("");
        }
      """).required(
      "var num: Double",
      "var str: String",
      "var xStrMember: String",
      "var xNumMember: Double",
      "def fun(par: String)",
      "def fun(par: Double)",
      "var strMember: String",
      "var numMember: Double",
    )
  }

  ignore("Proper scopes of local symbols") {
    // ignored - the test is complicated and its purpose unclear
    exec check ConversionCheck(
      //language=JavaScript
      """
          const x = 1;
          class Quaternion {
            get x(){return ""}
            set x(v){this._x = v;}
            constructor(x) {
              this._x = x;
            }
          }

          var callback = () => {
            var a = new Quaternion();
            a.x = x;
          };
      """).required(
      "def x = ",
      "def x_=(",
      "var _x: Double"
      )
  }

}
