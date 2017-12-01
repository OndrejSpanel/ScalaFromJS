package com.github.opengrabeso

import org.scalatest.FunSuite

class CollectionTests extends FunSuite with TestUtils {
  test("Detect Array access") {
    execute check ConversionCheck(
      //language=JavaScript
      """
      var ar = [0, 1, 2, 3, 4];
      var sum = 0;

      for ( v = 0, vl = ar.length; v < vl; v ++ ) {

        sum += ar[ v ];

      }
      for ( var i = 0; i < ar.length; i++ ) {

        ar[ i ] = 0;

      }
      """).required(
        "for (v <- ar)",
        "sum += v",
        "for (i <- ar.indices)",
        "ar(i)"
      ).forbidden(
        "i = 0"
      )
  }
  test("Detect Array access with substituted variable") {
    execute check ConversionCheck(
      //language=JavaScript
      """
      var ar = [0, 1, 2, 3, 4];
      var sum = 0;

      for ( var index = 0; index < ar.length; index++ ) {
        var a = ar[index];
        sum += a;

      }
      """).required(
      "for (a <- ar)"
    ).forbidden(
      "index"
    )
  }
  test("Detect Array fill") {
    execute check ConversionCheck(
      //language=JavaScript
      """
      var v, vl, vertices;

      vertices = new Array( vertices.length );

      for ( v = 0, vl = vertices.length; v < vl; v ++ ) {

        vertices[ v ] = new Vector3();

      }
      """).required(
      )
  }

  test("Detect Array append") {
    execute check ConversionCheck(
      //language=JavaScript
      """
      var x = 10;
      var vertices = new Array(x), colors = new Array(x);
      for ( var i = 0, il = vertices2.length; i < il; i ++ ) {

        var vertex = vertices2[ i ];

        var vertexCopy = vertex.clone();

        if ( matrix !== undefined ) vertexCopy.applyMatrix4( matrix );

        vertices.push( vertexCopy );

      }

      for ( var i = 0, il = colors2.length; i < il; i ++ ) {

        colors.push( colors2[ i ].clone() );

      }
      """
    )
  }

  test("Detect Array map") {
    execute check ConversionCheck(
      //language=JavaScript
      """
      var vertices = [], colors = new Array();
      for ( var i = 0, il = vertices2.length; i < il; i ++ ) {

        var vertex = vertices2[ i ];

        var vertexCopy = vertex.clone();

        if ( matrix !== undefined ) vertexCopy.applyMatrix4( matrix );

        vertices.push( vertexCopy );

      }

      for ( var i = 0, il = colors2.length; i < il; i ++ ) {

        colors.push( colors2[ i ].clone() );

      }
      """
    )
  }

}
