package com.github.opengrabeso.esprima

import _root_.esprima.Node
import Node.Node
import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror

package object walker {
  type TermCallback = (InstanceMirror, (Node) => Unit) => Unit
  type TermTransformerCallback = (InstanceMirror, (Node) => Node) => Unit

  type NodeWalker = (Node, Node => Unit) => Unit
  type NodeTransformer = (Node, Node => Node) => Unit

  def createWalkerForNode[T <: Node](tag: TypeTag[T]): NodeWalker = {
    createWalkerForType(typeOf[T](tag))
  }

  def createWalkerForType(t: Type): NodeWalker = {
    val members = t.members.filter(_.isTerm).map(_.asTerm).filter(_.isGetter)

    val walker: Iterable[TermCallback] = members.flatMap { term =>
      term.typeSignature match {
        case NullaryMethodType(resultType) if resultType <:< typeOf[Node] =>
          Some[TermCallback] {(oMirror, callback) =>
            callback(oMirror.reflectField(term).get.asInstanceOf[Node])
          }
        case NullaryMethodType(resultType) if resultType <:< typeOf[Seq[Node]] =>
          Some[TermCallback]{(oMirror, callback) =>
            oMirror.reflectField(term).get.asInstanceOf[Seq[Node]].foreach(callback)
          }
        case NullaryMethodType(resultType) if resultType <:< typeOf[Array[_]] =>
          resultType.typeArgs match {
            case at :: Nil if at <:< typeOf[Node] =>
              Some[TermCallback]{(oMirror, callback) =>
                oMirror.reflectField(term).get.asInstanceOf[Array[Node]].foreach(callback)
              }
            case _ =>
              None
          }
        case  _ =>
          None
      }
    }

    if (walker.isEmpty) {
      // special case optimization: no need to reflect on o when there are no members to dive into
      (_, _) => {}
    } else {
      (o: Node, callback: Node => Unit) => {
        val oMirror = currentMirror.reflect(o)
        walker.foreach { w =>
          // walkNode(w)
          w(oMirror, callback)
        }
      }
    }
  }

  // TODO: DRY createWalkerForType and createTransformerForType
  def createTransformerForType(t: Type): NodeTransformer = {
    val members = t.members.filter(_.isTerm).map(_.asTerm).filter(_.isGetter)

    val walker: Iterable[TermTransformerCallback] = members.flatMap { term =>
      term.typeSignature match {
        case NullaryMethodType(resultType) if resultType <:< typeOf[Node] =>
          Some[TermTransformerCallback] {(oMirror, callback) =>
            val termMirror = oMirror.reflectField(term)
            termMirror set callback(termMirror.get.asInstanceOf[Node])
          }
        case NullaryMethodType(resultType) if resultType <:< typeOf[Seq[Node]] =>
          Some[TermTransformerCallback]{(oMirror, callback) =>
            val termMirror = oMirror.reflectField(term)
            termMirror set termMirror.get.asInstanceOf[Seq[Node]].map(callback)
          }
        case NullaryMethodType(resultType) if resultType <:< typeOf[Array[_]] =>
          resultType.typeArgs match {
            case at :: Nil if at <:< typeOf[Node] =>
              Some[TermTransformerCallback]{(oMirror, callback) =>
                val termMirror = oMirror.reflectField(term)
                termMirror set termMirror.get.asInstanceOf[Array[Node]].map(callback)
              }
            case _ =>
              None
          }
        case  _ =>
          None
      }
    }

    if (walker.isEmpty) {
      // special case optimization: no need to reflect on o when there are no members to dive into
      (_, _) => {}
    } else {
      (o: Node, callback: Node => Node) => {
        val oMirror = currentMirror.reflect(o)
        walker.foreach { w =>
          // walkNode(w)
          w(oMirror, callback)
        }
      }
    }
  }


  def createWalkers(from: Type): Map[Class[_], NodeWalker] = {
    // https://stackoverflow.com/questions/27189258/list-all-classes-in-object-using-reflection
    import scala.reflect.runtime.universe._
    val mirror = runtimeMirror(this.getClass.getClassLoader)
    val nodes = from.decls.collect {
      case c: ClassSymbol if c.toType <:< typeOf[Node] =>
        val t = c.selfType
        mirror.runtimeClass(t) -> createWalkerForType(t)
    }

    nodes.toMap
  }

  def createTransformers(from: Type): Map[Class[_], NodeTransformer] = {
    // https://stackoverflow.com/questions/27189258/list-all-classes-in-object-using-reflection
    import scala.reflect.runtime.universe._
    val mirror = runtimeMirror(this.getClass.getClassLoader)
    val nodes = from.decls.collect {
      case c: ClassSymbol if c.toType <:< typeOf[Node] =>
        val t = c.selfType
        mirror.runtimeClass(t) -> createTransformerForType(t)
    }

    nodes.toMap
  }

  def createAllWalkers: Map[Class[_], NodeWalker] = createWalkers(typeOf[Node.type])
  def createAllTransformers: Map[Class[_], NodeTransformer] = createTransformers(typeOf[Node.type])

  def specializedWalkers(walkers: Map[Class[_], NodeWalker]): Map[Class[_], NodeWalker] = {
    // optimization: provide statical implementations of frequently used node types
    def walkStaticMemberExpression(node: Node.StaticMemberExpression, callback: Node => Unit) = {
      callback(node.`object`)
      callback(node.property)
    }
    def walkExpressionStatement(node: Node.ExpressionStatement, callback: Node => Unit) = {
      callback(node.expression)
    }

    walkers ++ Seq(
      classOf[Node.StaticMemberExpression] -> (walkStaticMemberExpression _).asInstanceOf[NodeWalker],
      classOf[Node.ExpressionStatement] -> (walkExpressionStatement _).asInstanceOf[NodeWalker]
    )
  }

  var allWalkers = specializedWalkers(createAllWalkers)
  var allTransformers = createAllTransformers

  /*
  * Extend AST types with all classes inherited from Node in given object
   * */
  def addNodeTypes(from: Type): Unit = {
    allWalkers ++= createWalkers(from)
    allTransformers ++= createTransformers(from)
  }

  /*
  call callback, if it returns false, descend recursively into children nodes
  */
  def walkRecursive(o: Node)(callback: Node => Boolean)(post: Node => Unit = _ => ()): Unit = {
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
