package com.github.opengrabeso.scalafromjs

import org.scalatest.funsuite.AnyFunSuite

class ScriptExtractorTest extends AnyFunSuite {
  test("import map replacement does not remap a generated value") {
    val html =
      """<!DOCTYPE html>
        |<script type="importmap">
        |{
        |  "imports": {
        |    "three": "../build/three.webgpu.js",
        |    "three/tsl": "../build/three.webgpu.js"
        |  }
        |}
        |</script>
        |<script type="module">
        |import { pass } from 'three/tsl';
        |</script>
        |""".stripMargin

    val extracted = ScriptExtractor.fromHTML("overlapping-import-map.html", html).get

    assert(extracted.contains("import { pass } from '../build/three.webgpu.js';"))
    assert(!extracted.contains("../build/../build/three.webgpu.js.webgpu.js"))
  }
}
