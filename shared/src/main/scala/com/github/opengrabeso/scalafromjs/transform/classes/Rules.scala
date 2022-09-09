package com.github.opengrabeso.scalafromjs
package transform
package classes

import Classes._
import com.github.opengrabeso.scalafromjs.esprima._
import com.github.opengrabeso.esprima._
import Symbols._
import com.github.opengrabeso.scalafromjs.Expressions.SingleExpression
import com.github.opengrabeso.scalafromjs.esprima.symbols.ScopeContext
import scala.collection.Seq


object Rules {
  def deleteMembers(n: NodeExtended, member: ConvertProject.MemberDesc) = {
    processAllClasses(n, Some(member.cls)) { (c, ctx) =>
      val ret = c.cloneNode()
      // filter member functions and properties
      ret.body.body = c.body.body.filterNot(p => hasName(p) && member.name.findFirstIn(methodName(p)).isDefined)

      findInlineBody(ret).foreach { inlineBody =>
        val body = getMethodBody(inlineBody)
        body.foreach { b =>
          val filteredBody = b.body.filter {
            case VarDecl(name, _, _)  =>
              member.name.findFirstIn(name).isEmpty
            case _ =>
              true
          }
          inlineBody.value.asInstanceOf[Node.FunctionExpression].body.body = filteredBody
        }
      }

      deleteVarMember(ret, member.name)
    }

  }

  def makeProperties(n: NodeExtended, member: ConvertProject.MemberDesc) = {
    processAllClasses(n, Some(member.cls)) { (c, ctx) =>

      // search constructor for a property definition
      val applied = for (constructor <- Classes.findConstructor(c)) yield {
        val cc = c.cloneNode()

        deleteVarMember(cc, member.name)

        object CheckPropertyInit {
          def unapply(arg: Node.Node): Option[Node.Expression] = arg match {
            case c: Node.Literal =>
              Some(c)
            case o: OObject =>
              // TODO: check if values are acceptable (no dependencies on anything else then parameters)
              Some(o)
            case _ =>
              None

          }
        }
        object MatchName {
          def unapply(arg: String): Option[String] = {
            Some(arg).filter(member.name.findFirstIn(_).isDefined)
          }
        }

        val newC = constructor.transformAfter(ctx) { (node, transformer) =>
          implicit val ctx = transformer.context
          node match {
            case Node.ExpressionStatement(Assign(Node.ThisExpression() Dot MatchName(prop), "=", CheckPropertyInit(init))) =>
              //println(s"Found property definition ${nodeClassName(init)}")
              val ss = Classes.transformClassParameters(c, init.cloneNode())
              val m = newMethod(prop, Seq(), Block(ss), init)
              cc.body.body = cc.body.body :+ m
              Node.EmptyStatement()
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

    processAllClasses(n, Some(member.cls)) { (c, ctx) =>
      val cc = c.cloneNode()
      val clsName = cc.id.name
      val mappedProps = cc.body.body.filter(hasName).map { p =>
        val name = methodName(p)
        if (member.name.findFirstIn(name).isDefined) {
          val value = applyTemplate(clsName, name)
          val v = ScalaNode.MemberTemplate (name, p, value)
          // special named property which is passed to output as a string, used for insertion of Scala code
          v
        } else {
          p
        }
      }
      cc.body.body = mappedProps
      cc
    }

  }

  def removeScope(n: NodeExtended, scope: List[String]) = {
    //println(s"Removing $toRemove")
    object MatchingScope {

      def matches(dot: Node.Node, seq: List[String]): Boolean = {
        val seqHead = seq.head
        dot match {
          case expr Dot `seqHead` if seq.tail.nonEmpty && matches(expr, seq.tail) =>
            true
          case Node.Identifier(`seqHead`) =>
            true
          case _ =>
            false
        }
      }

      def unapply(arg: Dot): Option[String] = {
        val seq = scope.reverse
        arg match {
          case expr Dot name if matches(expr, seq) =>
            Some(name)
          case _ =>
            None
        }
      }
    }

    val r = n.top.transformAfter {(node, transformer) =>
      node match {
        case MatchingScope(name) =>
          Node.Identifier(name).withTokens(node)
        case _ =>
          node
      }
    }
    n.copy(top = r)
  }

  def replaceIsClass(n: NodeExtended, member: ConvertProject.MemberDesc): NodeExtended = {
    // first scan for all symbols matching the rule
    var isClassProperties = Map.empty[String, Node.ClassDeclaration]
    var isClassVars = Map.empty[String, Node.ClassDeclaration]

    object ReturnTrue {
      def unapply(arg: Node.Node) = arg match {
        case SingleExpression(BooleanLiteral(true)) =>
          true
        case Node.BlockStatement(Seq(Node.ReturnStatement(BooleanLiteral(true)))) =>
          true
        case Node.ReturnStatement(BooleanLiteral(true)) =>
          true
        case _ =>
          false
      }
    }
    n.top.walk {
      case cls@Node.ClassDeclaration(Node.Identifier(cName), _, _, _, _) if (member.cls findFirstIn cName).isDefined =>
        object MatchName {
          def unapply(n: String): Option[String] = {
            member.name.findFirstMatchIn(n).filter(_.group(1).contains(cName)).map(_.matched)
          }
        }
        val matching = cls.body.body
          .collect{
            // we expect getter, no parameters, containing a single true statement
            case Node.MethodDefinition(Node.Identifier(MatchName(name)), _, _, AnyFun(Seq(), Defined(ReturnTrue())), _, _) =>
              name
          }
        val matchingVar = for {
          in <- findInlineBody(cls).toSeq
          body <- getMethodBody(in).toSeq
          // initialization not inlined yet - will be done in InlineConstructors, which is  done later
          VarDecl(MatchName(name), _,  _) <- body.body
          if findConstructor(cls).exists { method =>
            getMethodBody(method).exists { block =>
              block.body.exists {
                case Node.ExpressionStatement(Node.AssignmentExpression("=", Node.ThisExpression() Dot `name`, Node.Literal(OrType(true), _))) =>
                  true
                case _ =>
                  false
              }
            }
          }
        } yield {
          name
        }
        isClassProperties ++= matching.map(_ -> cls)
        isClassVars ++= matchingVar.map(_ -> cls)
        true
      case _ =>
        false
    }

    val funNames = isClassVars.map(_.swap)

    // we might introduce properties instead, but using isClass directly in a Scala code is unlikely anyway, removing them should do no harm
    val removedVariables = n.top.transformBefore { (node, descend, transformer) =>
      node match {
        case cd: Node.ClassDeclaration =>
          funNames.get(cd) match {
            case Some(funName) =>
              val clsName = cd.id.name
              for {
                c <- findConstructor(cd)
                cBody <- getMethodBody(c)
              } {
                val newBody = cBody.body.flatMap {
                  case Node.ExpressionStatement(Node.AssignmentExpression("=", Node.ThisExpression() Dot `funName`, Node.Literal(OrType(true), _))) =>
                    None
                  case node =>
                    Some(node)
                }
                c.value.asInstanceOf[Node.FunctionExpression].body.body = newBody
              }
              for {
                c <- findInlineBody(cd)
                cBody <- getMethodBody(c)
              } {
                val newBody = cBody.body.flatMap {
                  case VarDecl(`funName`, None, _) =>
                    None
                  case node =>
                    Some(node)
                }
                c.value.asInstanceOf[Node.FunctionExpression].body.body = newBody
              }
              cd
            case None =>
              // there might be some inner classes, but we expect isClass properties to exist only for the top-level ones
              node
          }
        case _ =>
          descend(node, transformer)
      }
    }

    //println(s"Detected isClass members $isClassMembers")
    val isClassMembers = isClassProperties ++ isClassVars

    object GetClass {
      def unapply(arg: String): Option[Node.ClassDeclaration] = isClassMembers.get(arg)
    }

    val ret = removedVariables.transformBefore { (node, descend, transformer) =>
      node match {
        case a: Node.AssignmentExpression =>
          // do not replace inside of left side of assignment, only inside of right side
          a.cloneNode().copy(right = descend(a.right, transformer).asInstanceOf[Node.Expression])
        case callOn Dot GetClass(Node.ClassDeclaration(Defined(propId), _, _, _, _)) =>
          //println(s"Detect call $prop")
          Binary(callOn, instanceof, propId)
        case _ =>
          descend(node, transformer)
      }
    }

    n.copy(top = ret)
  }

  def replaceGetClass(n: NodeExtended, member: ConvertProject.MemberDesc): NodeExtended = {
    // first scan for all symbols matching the rule
    val symbols = VariableUtils.listSymbols(n.top)(new ScopeContext).toSeq.map(s => s.name -> s).toMap

    object DetectClassCompare {
      def unapply(arg: Node.BinaryExpression)(implicit tokensFrom: Node.Node)= arg match {
        case Binary(callOn Dot prop, "=="|"===", expr) if member.name.findFirstIn(prop).isDefined =>
          val className = expr match {
            case StringLiteral(s) =>
              Some(s)
            case Node.Identifier(parent) Dot s =>
              // regex may exclude some parent classes - try it
              if (parent.matches(member.cls.regex)) Some(s)
              else None
            case _ Dot s =>
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
            sym => (callOn, Node.Identifier(sym.name).withTokens(expr))
          }
        case _ =>
          None

      }
    }
    val ret = n.top.transformAfter { (node, _) =>
      implicit val tokensFrom = node
      node match {
        case DetectClassCompare(callOn, symRef) =>
          Binary(callOn, instanceof, symRef)
        case _ =>
          node
      }
    }

    n.copy(top = ret)
  }

}
