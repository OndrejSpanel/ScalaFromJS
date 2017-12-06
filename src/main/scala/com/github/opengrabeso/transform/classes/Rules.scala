package com.github.opengrabeso
package transform
package classes

import Transform._
import Classes._
import Uglify._
import UglifyExt._
import UglifyExt.Import._
import Symbols._

import scala.collection.mutable
import scala.scalajs.js

object Rules {
  def deleteMembers(n: AST_Extended, member: ConvertProject.MemberDesc) = {
    processAllClasses(n, Some(member.cls)) { c =>
      val ret = c.clone()
      // filter member functions and properties
      ret.properties = c.properties.filterNot(p => member.name.test(propertyName(p)))

      deleteVarMember(ret, member.name)
    }

  }

  def makeProperties(n: AST_Extended, member: ConvertProject.MemberDesc) = {
    processAllClasses(n, Some(member.cls)) { c =>

      // search constructor for a property definition
      val applied = for (constructor <- Classes.findConstructor(c)) yield {
        val cc = c.clone()

        deleteVarMember(cc, member.name)

        object CheckPropertyInit {
          def unapply(arg: AST_Node): Option[AST_Node] = arg match {
            case c: AST_Constant =>
              Some(c)
            case o: AST_Object =>
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
            case AST_SimpleStatement(AST_Assign(AST_This() AST_Dot MatchName(prop), "=", CheckPropertyInit(init))) =>
              //println(s"Found property definition ${nodeClassName(init)}")
              val ss = AST_SimpleStatement(init) {
                Classes.transformClassParameters(c, init.clone())
              }
              cc.properties += newMethod(prop, Seq(), Seq(ss), init)
              AST_EmptyStatement(node)
            case _ =>
              node
          }
        }

        Classes.replaceProperty(cc, constructor, newC)
      }

      applied.getOrElse(c)
    }

  }

  def substMember(n: AST_Extended, member: ConvertProject.MemberDesc, template: String) = {

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
          val v = new AST_Sequence {
            fillTokens(this, p)
            expressions = js.Array(p, AST_String(p)(value))
          }
          // special named property which is passed to output as a string, used for insertion of Scala code
          Seq[AST_ObjectProperty](AST_ObjectKeyVal(p)(templatePrefix + name, v))
        } else {
          Seq(p)
        }
      }
      cc.properties = mappedProps
      cc
    }

  }

  def removeScope(n: AST_Extended, scope: List[String]) = {
    //println(s"Removing $toRemove")
    object MatchingScope {

      def matches(dot: AST_Node, seq: List[String]): Boolean = {
        val seqHead = seq.head
        dot match {
          case expr AST_Dot `seqHead` if seq.tail.nonEmpty && matches(expr, seq.tail) =>
            true
          case AST_SymbolRefName(`seqHead`) =>
            true
          case _ =>
            false
        }
      }

      def unapply(arg: AST_Dot): Option[String] = {
        val seq = scope.reverse
        arg match {
          case expr AST_Dot name if matches(expr, seq) =>
            Some(name)
          case _ =>
            None
        }
      }
    }

    val r = n.top.transformAfter {(node, transformer) =>
      node match {
        case MatchingScope(name) =>
          AST_SymbolRef(node)(name)
        case _ =>
          node
      }
    }
    n.copy(top = r)
  }

  def replaceIsClass(n: AST_Extended, member: ConvertProject.MemberDesc): AST_Extended = {
    // first scan for all symbols matching the rule
    var isClassMembers = Map.empty[String, AST_DefClass]

    n.top.walk {
      case cls@AST_DefClass(Defined(AST_SymbolName(cName)), _, _) if member.cls test cName =>
        val matching = cls.properties
          .collect{
            // we expect getter, no parameters, containing a single true statement
            case AST_ObjectGetter(AST_SymbolName(name), AST_Function(Seq(), Seq(AST_SimpleStatement(_: AST_True)))) =>
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
      def unapply(arg: String): Option[AST_DefClass] = isClassMembers.get(arg)
    }

    val ret = n.top.transformAfter { (node, _) =>
      node match {
        case callOn AST_Dot GetClass(AST_DefClass(Defined(AST_SymbolName(prop)), _, _)) =>
          //println(s"Detect call $prop")
          AST_Binary(node) (callOn, instanceof, AST_SymbolRef(node)(prop))
        case _ =>
          node
      }
    }

    n.copy(top = ret)
  }

  def replaceGetClass(n: AST_Extended, member: ConvertProject.MemberDesc): AST_Extended = {
    // first scan for all symbols matching the rule
    val symbols = VariableUtils.listSymbols(n.top).toSeq.map(s => s.name -> s).toMap

    object DetectClassCompare {
      def unapply(arg: AST_Binary)(implicit tokensFrom: AST_Node)= arg match {
        case AST_Binary(callOn AST_Dot prop, "=="|"===", expr) if member.name.test(prop) =>
          val className = expr match {
            case s: AST_String =>
              Some(s.value)
            case _ AST_Dot s =>
              Some(s)
            case AST_SymbolRefName(s) =>
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
            sym => (callOn, AST_SymbolRef.symDef(tokensFrom)(sym))
          }
        case _ =>
          None

      }
    }
    val ret = n.top.transformAfter { (node, _) =>
      implicit val tokensFrom = node
      node match {
        case DetectClassCompare(callOn, symRef) =>
          AST_Binary(node)(callOn, instanceof, symRef)
        case _ =>
          node
      }
    }

    n.copy(top = ret)
  }

}
