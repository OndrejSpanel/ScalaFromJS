package com.github.opengrabeso
package transform

import com.github.opengrabeso.esprima._
import _root_.esprima._
import com.github.opengrabeso.esprima.symbols.{Id, ScopeContext, SymId}

object Collections {

  trait ValuePath {
    def append(member: String) = MemberValuePath(this, member)
    def parent: ValuePath

    def unapply(arg: Node.Node)(implicit context: ScopeContext): Boolean
  }

  case object ThisValuePath extends ValuePath {
    def parent: ValuePath = throw new NoSuchFieldException(s"No more parents in $this")
    def unapply(arg: Node.Node)(implicit context: ScopeContext) = arg match {
      case _: Node.ThisExpression => // Node.Identifier does not match against this
        //println(s"ThisValuePath: match $arg")
        true
      case _ =>
        //println(s"ThisValuePath: no match $arg")
        false
    }

  }
  case class VariableValuePath(name: SymId) extends ValuePath {
    def parent: ValuePath = throw new NoSuchFieldException(s"No more parents in $this")

    def unapply(arg: Node.Node)(implicit context: ScopeContext) = arg match {
      case Node.Identifier(Id(`name`)) => // Node.Identifier does not match against this
        //println(s"VariableValuePath $this: match $arg against ${name.name}")
        true
      case _ =>
        //println(s"VariableValuePath $this: no match $arg against ${name.name}")
        false
    }
  }
  case class MemberValuePath(parent: ValuePath, member: String) extends ValuePath {

    def unapply(arg: Node.Node)(implicit context: ScopeContext): Boolean = {
      arg match {
        case parent() Dot `member` =>
          //println(s"MemberValuePath $this: match $arg against $parent.$member")
          true
        case _ =>
          //println(s"MemberValuePath $this: nomatch $arg against $parent.$member")
          false
      }
    }

  }

  object ValuePath {
    def unapply(arg: Node.Node)(implicit context: ScopeContext): Option[ValuePath] = {
      //println(s"ValuePath Unapply $arg")
      arg match {
        case Node.Identifier(Id(refDef)) => // Node.Identifier does not match against this
          //println(s"ValuePath: Match $arg as ${refDef.name}")
          Some(VariableValuePath(refDef))
        case _: Node.ThisExpression =>
          Some(ThisValuePath)
        case ValuePath(parent) Dot name =>
          //println(s"ValuePath: Match $arg as $parent.$name")
          Some(parent append name)
        case _ =>
          //println(s"ValuePath no match $arg")
          None

      }
    }
  }

  def usedOnlyAsIndex(body: Node.Node, varName: SymId, objName: ValuePath)(implicit context: ScopeContext): Boolean = {
    object IndexUsage {
      def unapply(arg: Sub) = arg match {
        case objName() Sub Node.Identifier(Id(`varName`)) =>
          true
        case _ =>
          false
      }
    }
    var otherUse = false
    body.walk {
      case node@Assign(IndexUsage(), _, _) =>
        // use on the left side of assignment - that is not allowed
        //println(s"L-value use $node")
        otherUse = true
        otherUse
      case node@IndexUsage() =>
        //println(s"Allowed use $node")
        true
      case node@Node.Identifier(Id(`varName`)) =>
        //println(s"Forbidden use $node")
        otherUse = true
        otherUse
      case _ =>
        otherUse
    }
    !otherUse
  }

  def transformIndexUse(body: Node.Node, varName: SymId, objName: ValuePath)(implicit context: ScopeContext): Node.Node = {
    body.transformAfter(context) {(node, _) =>
      node match {
        case objName() Sub (varRef@Node.Identifier(Id(`varName`))) =>
          varRef
        case _ =>
          node

      }
    }
  }

  def substituteIndex(forStatement: Node.ForInStatement, varName: SymId) = {
    // if there is a single variable inside of the body as const xx = varName and varName it not used otherwise, substitute it
    var otherUse = false
    var subst = Option.empty[SymId]
    forStatement.body.walkWithScope { (node, scope) =>
      implicit val ctx = scope
      node match {
        case Node.VariableDeclaration(Seq(Node.VariableDeclarator(Node.Identifier(Id(name)), Defined(Node.Identifier(Id(`varName`))))), "const") =>
          subst = Some(name)
          true
        case Node.Identifier(Id(`varName`)) =>
          otherUse = true
          otherUse
        case _ =>
          otherUse
      }
    }
    /*
    for {
      substName <- subst if !otherUse
    } {
      //println(s"Detected substitution of ${varName.name} as ${substName.name}")
      Variables.renameVariable(forStatement.body, varName, substName.name, substName)
      // the body now contains "const substName = substName, remove it
      forStatement.body = forStatement.body.transformAfter {(node, _) =>
        node match {
          case Node.Const(Node.VariableDeclarator(Node.Identifier(`substName`), Defined(Node.Identifier(`substName`)))) =>
            Node.EmptyStatement(node)
          case _ =>
            node
        }
      }
      Variables.renameVariable(forStatement.init, varName, substName.name, substName)
      forStatement.name = Node.Identifier.symDef(forStatement.init)(substName)
    }
    */
  }


  def transformFor(forStatement: Node.ForInStatement, varName: SymId, objName: ValuePath)(implicit ctx: ScopeContext): Node.ForInStatement = {
    transformIndexUse(forStatement.body, varName, objName)
    substituteIndex(forStatement, varName)
    forStatement
  }
  def apply(n: Node.Node): Node.Node = {
    n.transformAfter { (node, transformer) =>
      import transformer.context
      node match {

        case forStatement@ForRange(varName, "until", initVar@NumberLiteral(0), (obj@ValuePath(objName)) Dot "length", NumberLiteral(1))
          if usedOnlyAsIndex(forStatement.body, varName, objName) =>
          // note: Node.ForOf would be more appropriate, however it is not present yet in the Uglify AST we use
          //println(s"Detect for ..in $forStatement")
          val newFor = new Node.ForInStatement (
            left = Node.Identifier(varName.name),
            right = obj,
            body = forStatement.body
          )
          transformFor(newFor, varName, objName).asInstanceOf[Node.Statement]
        case _ =>
          node
      }
    }
  }
}
