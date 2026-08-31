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

  test("HTML extraction terminates a semicolonless static import") {
    val html =
      """<!DOCTYPE html>
        |<script type="importmap">
        |{
        |  "imports": {
        |    "three/addons/": "./jsm/"
        |  }
        |}
        |</script>
        |<script type="module">
        |import { OrbitControls } from 'three/addons/controls/OrbitControls.js'
        |let controls;
        |</script>
        |""".stripMargin

    val extracted = ScriptExtractor.fromHTML("semicolonless-import.html", html).get

    assert(extracted.contains("import { OrbitControls } from './jsm/controls/OrbitControls.js';"))
  }

  test("HTML extraction does not terminate the first line of a multiline import") {
    val html =
      """<!DOCTYPE html>
        |<script type="module">
        |import {
        |  First,
        |  Second
        |} from './module.js';
        |</script>
        |""".stripMargin

    val extracted = ScriptExtractor.fromHTML("multiline-import.html", html).get

    assert(extracted.contains("import {\n  First,"))
    assert(!extracted.contains("import {;"))
  }
}
