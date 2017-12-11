package com.github.opengrabeso
package transform

import net.gamatron.esprima._
import esprima._

object Collections {

  trait ValuePath {
    def append(member: String) = MemberValuePath(this, member)
    def parent: ValuePath

    def unapply(arg: Node.Node): Boolean
  }

  case object ThisValuePath extends ValuePath {
    def parent: ValuePath = throw new NoSuchFieldException(s"No more parents in $this")
    def unapply(arg: Node.Node) = arg match {
      case _: Node.This => // Node.Identifier does not match against this
        //println(s"ThisValuePath: match $arg")
        true
      case _ =>
        //println(s"ThisValuePath: no match $arg")
        false
    }

  }
  case class VariableValuePath(name: SymbolDef) extends ValuePath {
    def parent: ValuePath = throw new NoSuchFieldException(s"No more parents in $this")

    def unapply(arg: Node.Node) = arg match {
      case Node.Identifier(`name`) => // Node.Identifier does not match against this
        //println(s"VariableValuePath $this: match $arg against ${name.name}")
        true
      case _ =>
        //println(s"VariableValuePath $this: no match $arg against ${name.name}")
        false
    }
  }
  case class MemberValuePath(parent: ValuePath, member: String) extends ValuePath {

    def unapply(arg: Node.Node): Boolean = {
      arg match {
        case parent() Node.StaticMemberExpression `member` =>
          //println(s"MemberValuePath $this: match $arg against $parent.$member")
          true
        case _ =>
          //println(s"MemberValuePath $this: nomatch $arg against $parent.$member")
          false
      }
    }

  }

  object ValuePath {
    def unapply(arg: Node.Node): Option[ValuePath] = {
      //println(s"ValuePath Unapply $arg")
      arg match {
        case Node.Identifier(refDef) => // Node.Identifier does not match against this
          //println(s"ValuePath: Match $arg as ${refDef.name}")
          Some(VariableValuePath(refDef))
        case _: Node.This =>
          Some(ThisValuePath)
        case ValuePath(parent) Node.StaticMemberExpression name =>
          //println(s"ValuePath: Match $arg as $parent.$name")
          Some(parent append name)
        case _ =>
          //println(s"ValuePath no match $arg")
          None

      }
    }
  }

  def usedOnlyAsIndex(body: Node.Node, varName: SymbolDef, objName: ValuePath): Boolean = {
    object IndexUsage {
      def unapply(arg: Node.Sub) = arg match {
        case objName() Node.Sub Node.Identifier(`varName`) =>
          true
        case _ =>
          false
      }
    }
    var otherUse = false
    body.walk {
      case node@Node.Assign(IndexUsage(), _, _) =>
        // use on the left side of assignment - that is not allowed
        //println(s"L-value use $node")
        otherUse = true
        otherUse
      case node@IndexUsage() =>
        //println(s"Allowed use $node")
        true
      case node@Node.Identifier(`varName`) =>
        //println(s"Forbidden use $node")
        otherUse = true
        otherUse
      case _ =>
        otherUse
    }
    !otherUse
  }

  def transformIndexUse(body: Node.Node, varName: SymbolDef, objName: ValuePath): Node.Node = {
    body.transformAfter {(node, _) =>
      node match {
        case objName() Node.Sub (varRef@Node.Identifier(`varName`)) =>
          varRef
        case _ =>
          node

      }
    }
  }

  def substituteIndex(forStatement: Node.ForIn, varName: SymbolDef) = {
    // if there is a single variable inside of the body as const xx = varName and varName it not used otherwise, substitute it
    var otherUse = false
    var subst = Option.empty[SymbolDef]
    forStatement.body.walk {
      case Node.Const(Node.VarDef(Node.Identifier(name), Defined(Node.Identifier(`varName`)))) =>
        subst = Some(name)
        true
      case Node.Identifier(`varName`) =>
        otherUse = true
        otherUse
      case _ =>
        otherUse

    }
    for {
      substName <- subst if !otherUse
    } {
      //println(s"Detected substitution of ${varName.name} as ${substName.name}")
      Variables.renameVariable(forStatement.body, varName, substName.name, substName)
      // the body now contains "const substName = substName, remove it
      forStatement.body = forStatement.body.transformAfter {(node, _) =>
        node match {
          case Node.Const(Node.VarDef(Node.Identifier(`substName`), Defined(Node.Identifier(`substName`)))) =>
            Node.EmptyStatement(node)
          case _ =>
            node
        }
      }
      Variables.renameVariable(forStatement.init, varName, substName.name, substName)
      forStatement.name = Node.Identifier.symDef(forStatement.init)(substName)
    }
  }


  def transformFor(forStatement: Node.ForIn, varName: SymbolDef, objName: ValuePath): Node.ForIn = {
    transformIndexUse(forStatement.body, varName, objName)
    substituteIndex(forStatement, varName)
    forStatement
  }
  def apply(n: Node.Node): Node.Node = {

    n.transformAfter { (node, _) =>
      node match {

        case forStatement@ForRange(varName, "until", initVar@Node.Number(0), (obj@ValuePath(objName)) Node.StaticMemberExpression "length", Node.Number(1))
          if usedOnlyAsIndex(forStatement.body, varName, objName) =>
          // note: Node.ForOf would be more appropriate, however it is not present yet in the Uglify AST we use
          //println(s"Detect for ..in $forStatement")
          val newFor = new Node.ForIn {
            fillTokens(this, node)
            this.`object` = obj
            this.init = Node.Let(node)(Node.VarDef.uninitialized(node)(varName.name))
            this.name = Node.Identifier.symDef(node)(varName)
            this.body = forStatement.body
          }
          transformFor(newFor, varName, objName).asInstanceOf[Node.Statement]
        case _ =>
          node
      }
    }
  }
}
