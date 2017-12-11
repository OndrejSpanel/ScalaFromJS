package com.github.opengrabeso
package transform
package classes

import Transform._
import Classes._
import net.gamatron.esprima._
import esprima._

import Symbols._

import scala.collection.mutable

object Rules {
  def deleteMembers(n: NodeExtended, member: ConvertProject.MemberDesc) = {
    processAllClasses(n, Some(member.cls)) { c =>
      val ret = c.clone()
      // filter member functions and properties
      ret.properties = c.properties.filterNot(p => member.name.test(propertyName(p)))

      deleteVarMember(ret, member.name)
    }

  }

  def makeProperties(n: NodeExtended, member: ConvertProject.MemberDesc) = {
    processAllClasses(n, Some(member.cls)) { c =>

      // search constructor for a property definition
      val applied = for (constructor <- Classes.findConstructor(c)) yield {
        val cc = c.clone()

        deleteVarMember(cc, member.name)

        object CheckPropertyInit {
          def unapply(arg: Node.Node): Option[Node.Node] = arg match {
            case c: Node.Constant =>
              Some(c)
            case o: Node.Object =>
              // TODO: check if values are acceptable (no dependencies on anything else then parameters)
              Some(o)
            case _ =>
              None

          }
        }
        object MatchName {
          def unapply(arg: String): Option[String] = {
            Some(arg).filter(member.name.test)
          }
        }

        val newC = constructor.transformAfter { (node, transformer) =>
          node match {
            case Node.SimpleStatement(Node.Assign(Node.This() Node.StaticMemberExpression MatchName(prop), "=", CheckPropertyInit(init))) =>
              //println(s"Found property definition ${nodeClassName(init)}")
              val ss = Node.SimpleStatement(init) {
                Classes.transformClassParameters(c, init.clone())
              }
              cc.properties += newMethod(prop, Seq(), Seq(ss), init)
              Node.EmptyStatement(node)
            case _ =>
              node
          }
        }

        Classes.replaceProperty(cc, constructor, newC)
      }

      applied.getOrElse(c)
    }

  }

  def substMember(n: NodeExtended, member: ConvertProject.MemberDesc, template: String) = {

    import TextTemplates._
    def applyTemplate(cls: String, name: String) = {
      template.substitute("class", cls).substitute("name", name)
    }

    processAllClasses(n, Some(member.cls)) { c =>
      val cc = c.clone()
      val clsName = cc.name.fold("")(_.name)
      val mappedProps = cc.properties.flatMap { p =>
        val name = propertyName(p)
        if (member.name.test(name)) {
          val value = applyTemplate(clsName, name)
          val v = new Node.Sequence {
            fillTokens(this, p)
            expressions = js.Array(p, Node.String(p)(value))
          }
          // special named property which is passed to output as a string, used for insertion of Scala code
          Seq[Node.ObjectProperty](Node.ObjectKeyVal(p)(templatePrefix + name, v))
        } else {
          Seq(p)
        }
      }
      cc.properties = mappedProps
      cc
    }

  }

  def removeScope(n: NodeExtended, scope: List[String]) = {
    //println(s"Removing $toRemove")
    object MatchingScope {

      def matches(dot: Node.Node, seq: List[String]): Boolean = {
        val seqHead = seq.head
        dot match {
          case expr Node.StaticMemberExpression `seqHead` if seq.tail.nonEmpty && matches(expr, seq.tail) =>
            true
          case Node.Identifier(`seqHead`) =>
            true
          case _ =>
            false
        }
      }

      def unapply(arg: Node.StaticMemberExpression): Option[String] = {
        val seq = scope.reverse
        arg match {
          case expr Node.StaticMemberExpression name if matches(expr, seq) =>
            Some(name)
          case _ =>
            None
        }
      }
    }

    val r = n.top.transformAfter {(node, transformer) =>
      node match {
        case MatchingScope(name) =>
          Node.Identifier(node)(name)
        case _ =>
          node
      }
    }
    n.copy(top = r)
  }

  def replaceIsClass(n: NodeExtended, member: ConvertProject.MemberDesc): NodeExtended = {
    // first scan for all symbols matching the rule
    var isClassMembers = Map.empty[String, Node.DefClass]

    n.top.walk {
      case cls@Node.DefClass(Defined(Node.SymbolName(cName)), _, _) if member.cls test cName =>
        val matching = cls.properties
          .collect{
            // we expect getter, no parameters, containing a single true statement
            case Node.ObjectGetter(Node.SymbolName(name), Node.Function(Seq(), Seq(Node.SimpleStatement(_: Node.True)))) =>
              name
          }.filter { n =>
          val matched = member.name.exec(n)
          if (matched == null || matched.length < 2) false
          else {
            // only when class name matches the first match group
            matched.lift(1).exists(_.contains(cName))
          }
        }.toSet

        isClassMembers ++= matching.map(_ -> cls)
        true
      case _ =>
        false
    }

    //println(s"Detected isClass members $isClassMembers")

    object GetClass {
      def unapply(arg: String): Option[Node.DefClass] = isClassMembers.get(arg)
    }

    val ret = n.top.transformAfter { (node, _) =>
      node match {
        case callOn Node.StaticMemberExpression GetClass(Node.DefClass(Defined(Node.SymbolName(prop)), _, _)) =>
          //println(s"Detect call $prop")
          Node.BinaryExpression(node) (callOn, instanceof, Node.Identifier(node)(prop))
        case _ =>
          node
      }
    }

    n.copy(top = ret)
  }

  def replaceGetClass(n: NodeExtended, member: ConvertProject.MemberDesc): NodeExtended = {
    // first scan for all symbols matching the rule
    val symbols = VariableUtils.listSymbols(n.top).toSeq.map(s => s.name -> s).toMap

    object DetectClassCompare {
      def unapply(arg: Node.BinaryExpression)(implicit tokensFrom: Node.Node)= arg match {
        case Node.BinaryExpression(callOn Node.StaticMemberExpression prop, "=="|"===", expr) if member.name.test(prop) =>
          val className = expr match {
            case s: Node.String =>
              Some(s.value)
            case _ Node.StaticMemberExpression s =>
              Some(s)
            case Node.Identifier(s) =>
              Some(s)
            case _ =>
              None
          }
          // find any symbol matching the name
          // TODO: try even a similar symbol
          val sym = className.flatMap(symbols.get)

          //className.foreach(c => println(s"Found $c sym ${sym.map(_.name).getOrElse(expr)}"))
          // TODO: even when there is no class, consider passing the name if it looks reasonable
          sym.map {
            sym => (callOn, Node.Identifier.symDef(tokensFrom)(sym))
          }
        case _ =>
          None

      }
    }
    val ret = n.top.transformAfter { (node, _) =>
      implicit val tokensFrom = node
      node match {
        case DetectClassCompare(callOn, symRef) =>
          Node.BinaryExpression(node)(callOn, instanceof, symRef)
        case _ =>
          node
      }
    }

    n.copy(top = ret)
  }

}
