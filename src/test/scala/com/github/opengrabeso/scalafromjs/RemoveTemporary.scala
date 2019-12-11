package com.github.opengrabeso.scalafromjs

import org.scalatest.FunSuite

class RemoveTemporary extends FunSuite with TestUtils {
  test("Temporary variable (local) removal from Three.js Vector2 clampScalar") {
    // see c69128cb670b144f09df9b7697bfbecfc5ef66f7 - Helper.AsFunction / onlyVariables
    exec check ConversionCheck(
      // language=JavaScript
      """
      function Vector2( x, y ) {

        this.x = x || 0;
        this.y = y || 0;

      }
      Object.assign( Vector2.prototype, {
        clampScalar: function () {

          var min = new Vector2();
          var max = new Vector2();

          return function clampScalar( minVal, maxVal ) {

            min.set( minVal, minVal );
            max.set( maxVal, maxVal );

            return this.clamp( min, max );

          };

        }()
      })
      """).required(
      "val min = new Vector2()",
      "val max = new Vector2()",
      "def clampScalar"
    ).forbidden("(minVal, maxVal) =>")
  }

  test("Temporary variable (global) removal from Three.js Box3 clampScalar") { // detectGlobalTemporaries
    exec check ConversionCheck(
      // language=JavaScript
      """
      var _vector = new Vector3();
      function Box3( min, max ) {

        this.min = ( min !== undefined ) ? min : new Vector3( + Infinity, + Infinity, + Infinity );
        this.max = ( max !== undefined ) ? max : new Vector3( - Infinity, - Infinity, - Infinity );

      }


      Object.assign( Box3.prototype, {
        setFromCenterAndSize: function ( center, size ) {

          var halfSize = _vector.copy( size ).multiplyScalar( 0.5 );

          this.min.copy( center ).sub( halfSize );
          this.max.copy( center ).add( halfSize );

          return this;

        }
      })
      """).required(
     "val vector = new Vector3()",
    ).forbidden("_vector")
  }

  test("Temporary variable (global) removal from multiple scopes") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      var _vector = new Vector3();
      function a( size ) {

        _vector.copy( size ).multiplyScalar( 0.5 );
      }
      function b( center, size ) {

        _vector.set( 0, 1, 2);

      }
      """).required(
      "val vector = new Vector3()",
      "vector.copy(",
      "vector.set(",
    ).forbidden("_vector")
  }

  test("Do not remove variable used from its own declaration") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      var _Math = {
        DEG2RAD: Math.PI / 180,
        degToRad: function ( degrees ) {
          return degrees * _Math.DEG2RAD;
        },
      }""").required("object _Math", "_Math.DEG2RAD").forbidden("object Math")
  }

  test("Make multiple global variables unique") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      var _vector = new Vector3();
      function a( size ) {
        _vector.copy( size ).multiplyScalar( 0.5 );
      }
      var _vector = new Vector2();
      function b( center, size ) {
        _vector.set( 0, 1);
      }
      var _vector = 0;
      function c() {
        _vector = _vector + 1;
      }
      """).required(
      "val vector = new Vector3()",
      "vector.copy(",
      "val vector$1 = new Vector2()",
      "vector$1.set(",
      "var vector$2 = 0",
      "vector$2 = vector$2 + 1",
    ).forbidden("_vector", "vector.set(")
  }

}
