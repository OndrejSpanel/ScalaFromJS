package com.github.opengrabeso

import _root_.esprima.Node.Node

package object esprima extends NodeExt {
  // interface inspired by uglify-js
  def walk(ast: Node)(callback: Node => Boolean) = {
    walker.walkRecursive(ast)(callback)()
  }

  def transformBefore() = ???

  def transformAfter() = ???


}
