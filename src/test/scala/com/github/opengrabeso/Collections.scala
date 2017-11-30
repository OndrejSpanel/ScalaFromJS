package com.github.opengrabeso

import org.scalatest.FunSuite

class Collections extends FunSuite with TestUtils {
  test("Detect Array fill") {
    execute check ConversionCheck(
      //language=JavaScript
      """
      var v, vl, vertices;

      vertices = new Array( this.vertices.length );

      for ( v = 0, vl = this.vertices.length; v < vl; v ++ ) {

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
