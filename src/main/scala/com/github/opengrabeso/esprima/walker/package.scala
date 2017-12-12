package com.github.opengrabeso.esprima

import _root_.esprima.Node
import Node.Node
import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror

package object walker {
  type TermCallback = (InstanceMirror, (Node) => Unit) => Unit
  type NodeWalker = (Node, Node => Unit) => Unit

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

  def createAllWalkers: Map[Class[_], NodeWalker] = {
    // https://stackoverflow.com/questions/27189258/list-all-classes-in-object-using-reflection
    import scala.reflect.runtime.universe._
    val mirror = runtimeMirror(this.getClass.getClassLoader)
    val nodes = typeOf[Node.type].decls.collect {
      case c: ClassSymbol if c.toType <:< typeOf[Node] =>
        val t = c.selfType
        mirror.runtimeClass(t) -> createWalkerForType(t)
    }

    nodes.toMap
  }

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

  def walkNode(o: Node, walker: NodeWalker, callback: Node => Unit): Unit = {
    walker(o, callback)
  }

  lazy val allWalkers = specializedWalkers(createAllWalkers)

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
      walkNode(o, walker, callback)
    }

  }

}
