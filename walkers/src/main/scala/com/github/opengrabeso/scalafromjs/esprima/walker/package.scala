package com.github.opengrabeso.scalafromjs.esprima

import com.github.opengrabeso.esprima.Node
import Node.Node

import scala.collection.mutable
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

package object walker {
  type Walker = this.type
  val Walker: Walker = this

  type NodeWalker = (Node, Node => Unit) => Unit
  type NodeTransformer = (Node, Node => Node) => Unit


  def createWalkerForNode[B, T <: B]: (B, B => Unit) => Unit = macro walker_impl[B, T]
  def createTransformerForNode[B, T <: B]: (B, B => B) => Unit = macro transformer_impl[B, T]
  def createWalkers[B, O]: Map[Class[_], (B, B => Unit) => Unit] = macro createWalkers_impl[B, O]
  def createTransformers[B, O]: Map[Class[_], (B, B => B) => Unit] = macro createTransformers_impl[B, O]

  def walker_impl[B: c.WeakTypeTag, T: c.WeakTypeTag](c: blackbox.Context): c.Expr[(B, B => Unit) => Unit] = {
    import c.universe._

    val iterable = typeOf[Iterable[Any]]
    val iterableClass = iterable.typeSymbol

    val T = weakTypeOf[T]
    val B = weakTypeOf[B]

    def isSeqB(returnType: Type) = {
      returnType <:< iterable && returnType.baseType(iterableClass).typeArgs.headOption.exists(_ <:< B)
    }
    val dive = T.members.sorted.collect {
      case f if f.isMethod && f.asMethod.paramLists.isEmpty && f.asMethod.isGetter && f.asMethod.returnType <:< B =>
        q"if (t.$f != null) f(t.$f)"
      case f if f.isMethod && f.asMethod.paramLists.isEmpty && f.asMethod.isGetter && isSeqB(f.asMethod.returnType) =>
        q"if (t.$f != null) t.$f.foreach(f)"
    }
    //println(s"dive ${dive.size} for $T:$B")
    val r = q" (t: $B, f: $B => Unit) => {..$dive}"
    c.Expr(r)
  }

  def transformer_impl[B: c.WeakTypeTag, T: c.WeakTypeTag](c: blackbox.Context): c.Expr[(B, B => B) => Unit] = {
    import c.universe._
    val T = weakTypeOf[T]
    val B = weakTypeOf[B]
    val iterable = typeOf[Iterable[Any]]
    val iterableClass = iterable.typeSymbol
    def isSeqB(returnType: Type) = {
      returnType <:< iterable && returnType.baseType(iterableClass).typeArgs.headOption.exists(_ <:< B)
    }
    val dive = T.members.sorted.collect {
      case f if f.isMethod && f.asMethod.paramLists.isEmpty && f.asMethod.isGetter && f.asMethod.setter != NoSymbol && f.asMethod.returnType <:< B =>
        val s = f.asMethod.setter
        val MT = f.asMethod.returnType
        q"if (t.$f != null) t.$s(f(t.$f).asInstanceOf[$MT])"
      case f if f.isMethod && f.asMethod.paramLists.isEmpty && f.asMethod.isGetter && f.asMethod.setter != NoSymbol && isSeqB(f.asMethod.returnType) =>
        val s = f.asMethod.setter
        val MT = f.asMethod.returnType.baseType(iterableClass).typeArgs.head
        q"if (t.$f != null) t.$s(t.$f.map(m => f(m).asInstanceOf[$MT]))"
    }

    val r = q"(t: $B, f: $B => $B) => {..$dive}"
    c.Expr(r)
  }

  def createWalkers_impl[B: c.WeakTypeTag, O: c.WeakTypeTag](c: blackbox.Context): c.Expr[Map[Class[_], (B, B => Unit) => Unit]] = {
    import c.universe._
    val O = weakTypeOf[O]
    val B = weakTypeOf[B]
    val walkers = O.decls.collect {
      case m if m.isClass /*&& !m.isAbstract*/ && m.asClass.baseClasses.contains(B.typeSymbol) =>
        val C = m.asClass

        q"(classOf[$C], createWalkerForNode[$B,$C])"

    }
    val r = q"Map[Class[_], ($B, $B=>Unit) => Unit](..$walkers)"
    c.Expr(r)
  }

  def createTransformers_impl[B: c.WeakTypeTag, O: c.WeakTypeTag](c: blackbox.Context): c.Expr[Map[Class[_], (B, B => B) => Unit]] = {
    import c.universe._
    val O = weakTypeOf[O]
    val B = weakTypeOf[B]
    val walkers = O.decls.collect {
      case m if m.isClass && !m.isAbstract && m.asClass.baseClasses.contains(B.typeSymbol) =>
        val C = m.asClass

        q"(classOf[$C], createTransformerForNode[$B,$C])"

    }
    val r = q"Map[Class[_], ($B, $B=>$B) => Unit](..$walkers)"
    c.Expr(r)
  }

  def specializedWalkers(walkers: Map[Class[_], NodeWalker]): Map[Class[_], NodeWalker] = {
    // optimization: provide statical implementations of frequently used node types
    def walkStaticMemberExpression(node: Node.StaticMemberExpression, callback: Node => Unit) = {
      callback(node.`object`)
      // do not enter the property - it looks like an identifier, but it is respecting normal scoping rules
      // callback(node.property)
    }
    def walkExpressionStatement(node: Node.ExpressionStatement, callback: Node => Unit) = {
      callback(node.expression)
    }

    walkers ++ Seq(
      classOf[Node.StaticMemberExpression] -> (walkStaticMemberExpression _).asInstanceOf[NodeWalker],
      classOf[Node.ExpressionStatement] -> (walkExpressionStatement _).asInstanceOf[NodeWalker]
    )
  }
  def specializedTransformers(walkers: Map[Class[_], NodeTransformer]): Map[Class[_], NodeTransformer] = {
    // optimization: provide statical implementations of frequently used node types
    def walkStaticMemberExpression(node: Node.StaticMemberExpression, callback: Node => Node) = {
      node.`object` = callback(node.`object`).asInstanceOf[Node.Expression]
      // do not enter the property - it looks like an identifier, but it is respecting normal scoping rules
      // callback(node.property)
    }

    walkers ++ Seq(
      classOf[Node.StaticMemberExpression] -> (walkStaticMemberExpression _).asInstanceOf[NodeTransformer]
    )
  }

  var allWalkers = mutable.Map.empty[Class[_], NodeWalker]
  var allTransformers = mutable.Map.empty[Class[_], NodeTransformer]

  /*
  * Extend AST types with all classes inherited from Node in given object
  * Note: from should be a type of a singleton object Xxxxx, obtained as typeOf[Xxxx.type]
  * The singleton object may be nested in other singletons, but not in a class or trait
  * Note: this is not thread safe. Take care if you have multiple extensions.
   * */
  def addNodeTypes[T, O](w: Walker): Unit = macro addNodeTypes_impl[T,O]

  def addNodeTypes_impl[T: c.WeakTypeTag, O: c.WeakTypeTag](c: blackbox.Context)(w: c.Expr[Walker]): c.Expr[Unit] = {
    import c.universe._

    val walkers = createWalkers_impl[T, O](c)
    val transformers = createTransformers_impl[T, O](c)

    c.Expr(q"$w.allWalkers ++= $walkers; $w.allTransformers ++= $transformers")
  }

  /*
  call callback, if it returns false, descend recursively into children nodes
  */
  def walkRecursive(o: Node)(callback: Node => Boolean)(post: Node => Unit = _ => ()): Unit = {
    assert(allWalkers.nonEmpty) // verify someone has intialized the list
    // some fields may be null (e.g FunctionExpression id)
    if (o != null && !callback(o)) {
      walkInto(o)(walkRecursive(_)(callback)(post))
      post(o)
    }
  }

  def walkInto(o: Node)(callback: Node => Unit): Unit = {
    if (o != null) {
      val walker = allWalkers(o.getClass)
      walker(o, callback)
    }
  }

  def transformInto(o: Node)(callback: Node => Node): Unit = {
    if (o != null) {
      val walker = allTransformers(o.getClass)
      walker(o, callback)
    }
  }
}
