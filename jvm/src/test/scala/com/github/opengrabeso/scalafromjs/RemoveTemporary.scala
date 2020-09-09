package com.github.opengrabeso.scalafromjs

import org.scalatest.funsuite.AnyFunSuite

class RemoveTemporary extends AnyFunSuite with TestUtils {
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

  test("Do not remove a global variable used from its own declaration") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      var _recursive = (_recursive);

      function use() {_recursive();}
      """).required("val _recursive").forbidden(" recursive")
  }
  test("Do not remove a global variable initialized with an object expression") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      var _Obj = {
        VALUE: 100,
        multiply: function ( x ) {
          return x * 10;
        },
      }
      """).required("object _Obj").forbidden(" Obj")
  }
  test("Do not remove a global variable initialized with scalar") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      var _id = 0;
      function newId() {
        return _id ++;
      }
      """).required("var _id = 0").forbidden(" id")

  }

  test("Remove multiple definition global variables as unique ones") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      var _vector = new Vector3();
      function a( size ) {
        _vector.copy( size ).multiplyScalar( 0.5 );
      }
      var _vector = new Vector2();
      function b( center, size ) {
        _vector.set(0, 1);
      }
      var _vector = 0;
      function c() {
        _vector = _vector + 1;
      }
      var _vector = new Vector4();
      function d( center, size ) {
        _vector.set(4, 3, 2, 1);
      }
      """).required(
      "val vector = new Vector3()",
      " vector.copy(",
      "val vector = new Vector2()",
      " vector.set(0",
      "var _vector = 0",
      "_vector = _vector + 1",
      "val vector = new Vector4()",
      " vector.set(4",
    ).forbidden("_vector.")
  }

  test("Avoid name clash when introducing a temporary") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      var _vector = new Vector3();
      function create() {
        var vector = new Vector3();
        _vector.copy(vector);
      }
      """).required(
      "val vector = new Vector3",
      "val _vector = new Vector3",
      " _vector.copy(vector)"
    )
  }

}
