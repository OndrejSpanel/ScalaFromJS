package com.github.opengrabeso.scalafromjs
package transform

import JsUtils._
import com.github.opengrabeso.scalafromjs.esprima._
import com.github.opengrabeso.esprima._
import Classes._
import SymbolTypes._
import Expressions._
import Transform._
import com.github.opengrabeso.scalafromjs.esprima.symbols.{Id, ScopeContext, SymId}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

object Variables {
  import VariableUtils._
  import Symbols._

  // detect variables which can be declared as val instead of var
  def detectVals(n: Node.Node): Node.Node = Time("detectVals") {
    // walk the tree, check for possible val replacements and perform them
    val refs = buildReferenceStacks(n)

    n.transformBefore {(node, descend, transformer) =>
      implicit val ctx = transformer.context
      node match {
        case cm: Node.MethodDefinition =>
          if (propertyKeyName(cm.key) != inlineBodyName) {
            // no var detection inside of inline class body (its variables are actually class members)
            val n = cm.clone()
            descend(n, transformer)
            n
          } else cm.clone()

        case VarDecl(Id(varName), Some(value), kind) if kind != "const" => // var with init - search for a modification
          // check if any reference is assignment target

          val assignedInto = refs.isModified(varName)
          val n = if (!assignedInto) {
            VarDecl(varName.name, Some(value), "const").withTokens(node)
          } else {
            node.clone()
          }
          descend(n, transformer) // may contain other scopes with initializations
          n
        case _ =>
          val n = node.clone()
          descend(n, transformer)
          n
      }
    }
  }


  // detect variable defined twice in a scope, remove the 2nd definition
  def detectDoubleVars(n: Node.Node): Node.Node = Time("detectDoubleVars") {
    // walk the tree, check for possible val replacements and perform them
    def usedAsForVar(varName: String, transformer: TreeTransformer) = {
      def varIn(node: Node.Node) = {
        var present = false
        node walk {
          case Node.Identifier(`varName`) =>
            present = true
            present
          case IsScope() =>
            true
          case _ =>
            present
        }
        present
      }
      transformer.parent().exists {
        case f: Node.ForStatement if Option(f.init).exists(varIn) || Option(f.update).exists(varIn) || Option(f.test).exists(varIn) =>
          true
        case _ =>
          false

      }
    }

    var declared = Set.empty[SymId]

    n.transformAfter {(node, transformer) =>
      implicit val ctx = transformer.context
      node match {
        // match only Node.Var, assume Node.Let and Node.Const are already well scoped
        case VarDecl(Id(varName), Some(value), _) if !usedAsForVar(varName.name, transformer) => // var with init - search for a modification
          //println(s"Node.VariableDeclarator ${varName.name}")
          //println(s"${varName.thedef.get.name} $varDef ${varName.thedef.get.orig} ${varName == varName.thedef.get.orig.head}")

          if (!(declared contains varName)) {
            declared += varName
            node
          } else {
            Node.ExpressionStatement(Node.AssignmentExpression(
              left = Node.Identifier(varName.name),
              operator = "=",
              right = value.cloneNode()
            ))
          }
        case _ =>
          node
      }
    }
  }

  // detect function key values which can be declared as concise methods instead
  def detectMethods(n: Node.Node): Node.Node = {
    val refs = buildReferenceStacks(n)

    n.transformBefore {(node, descend, transformer) =>
      implicit val ctx = transformer.context
      node match {
        case obj@OObject(props) =>
        //case Node.VariableDeclaration(Node.VariableDeclarator(Node.Identifier(df), Defined(obj@OObject(props)))) =>

          // check if the object is part of variable / const initialization, like: var df = {}
          transformer.parent(1) match {
            case Some(VarDecl(Id(df), Some(o: OObject), _)) =>
              //println(s"Scan object ${df.name} for methods ${o.properties}")
              assert(o == obj)

              object IsDfMember extends Extractor[String] {
                def unapply(arg: Node.Node) = arg match {
                  case Node.Identifier(Id(`df`)) Dot key => Some(key)
                  case _ => None
                }
              }
              object IsDfModified extends IsModified(IsDfMember)

              // check which members are ever written to - we can convert all others to getters and methods
              var modifiedMembers = Set.empty[String]
              refs.walkReferences(df, IsDfModified) { key =>
                modifiedMembers += key
                false
              }
              //println(s"Detected modified members $modifiedMembers")

              /*
              val newProps: Seq[Node.MethodDefinition] = props.map {
                case kv@Node.MethodDefinition(KeyName(k), _, AnyFun(args, body), _, _) =>
                  if (modifiedMembers contains k) {
                    //println(s"Modified member $k")
                    kv
                  } else newMethod(k, args, Block(body), kv)

                case p => p
              }
              val newObj = obj.cloneNode()
              newObj.properties = newProps
              newObj
              */
              obj

            case None =>
              node
            case Some(p) =>
              //println(s"Parent ${nodeClassName(p)}")
              node
          }
        case _ =>
          val n = node.clone()
          descend(n, transformer)
          n
      }
    }
  }

  // transform a function defined as var x = function() {} into function x(){}
  def convertConstToFunction(n: Node.Node): Node.Node = {
    n.transformAfter { (node, context) =>
      node match {
        case VarDecl(sym, Some(AnyFunctionExpression(args, body)), "const") =>
          Node.FunctionDeclaration(
            id = Node.Identifier(sym),
            params = args,
            body = Block(body),
            generator = false
          ).withTokensDeep(node)
        case _ =>
          node
      }
    }
  }

  /*
  * many transformations assume each Node.VariableDeclaration contains at most one Node.VariableDeclarator
  * Split multiple definitions (comma separated definitiob lists)
  * */
  def splitMultipleDefinitions(n: Node.Node): Node.Node = {
    n.transformAfter { (node, transformer) =>
      node match {
        case block: Node.BlockStatement =>
          block.body = block.body.flatMap {
            case defs: Node.VariableDeclaration =>
              defs.declarations.map { d =>
                val clone = defs.cloneNode() // keeps Node.VariableDeclaration type (Node.Var or whatever it is)
                clone.declarations = Seq(d)
                clone
              }
            case other =>
              Seq(other)
          }
          block
        case _ =>
          node
      }

    }

  }
  // merge variable declaration and first assignment if possible
  def varInitialization(n: Node.Node): Node.Node = {

    object MatchInitWithAssign {
      def unapply(arg: Node.Node)(implicit context: ScopeContext) = arg match {
        // sr = xxx
        case Node.ExpressionStatement(Assign(Node.Identifier(Id(name)), "=", right)) =>
          Some(name, right)
        // if (m1 == undefined) m1 = xxx
        case Node.IfStatement(
          Binary(sr@Node.Identifier(Id(thedef)), "==" | "===", Node.Identifier("undefined")),
          SingleExpression(Assign(Node.Identifier(Id(thedef2)), "=", right)),
          IsNull()
        ) if thedef == thedef2 =>
          Some(thedef, right)
        case Node.IfStatement(
          Binary(Node.Identifier(Id(thedef)), "==" | "===", Node.Identifier("undefined")),
          SingleExpression(Assign(Node.Identifier(Id(thedef2)), "=", right)),
          IsNull()
        )  =>
          None
        case _ =>
          None
      }
    }



    // walk the tree, check for first reference of each var
    var defined = Set.empty[SymId] // symbol definition -> first reference
    var used = Set.empty[SymId] // any use makes assignment not first
    val init = mutable.Map.empty[SymId, Int] // one or multiple assignments?
    n.walkWithScope { (node, scopeContext) =>
      implicit val ctx = scopeContext
      node match {
        case VarDecl(Id(name), None, _) =>
          defined += name
          true

        case MatchInitWithAssign(name, _) if scopeContext.scopeId == name.sourcePos && !(used contains name) && (defined contains name) =>
          if (!(init contains name)) {
            init += name -> 0
          } else {
            init(name) += 1 // if already used, mark as var, not const
          }
          false

        case Assign(Node.Identifier(Id(name)), _, _) if init contains name =>
          init(name) += 1 // if already used, mark as var, not const
          false

        case Node.Identifier(Id(name)) if defined contains name =>
          used += name
          false

        case _ =>
          false
      }
    }

    val refs = init.keySet
    var replaced = Set.empty[SymId]

    //println(s"transform, vars ${pairs.keys.map(SymbolTypes.id).mkString(",")}")

    // walk the tree, check for possible val replacements and perform them
    val changeAssignToVar = n.transformAfter {(node, transformer) =>
      implicit val ctx = transformer.context
      // descend informs us how to descend into our children - cannot be used to descend into anything else
      //println(s"node ${nodeClassName(node)} ${ScalaOut.outputNode(node, "")}")
      // we need to descend into assignment definitions, as they may contain other assignments
      node match {
        case MatchInitWithAssign(vName, right) if ctx.scopeId == vName.sourcePos && (refs contains vName) =>
          //println(s"ss match assign ${nodeClassName(left)} ${ScalaOut.outputNode(left, "")}")
          // checking if inside of a block statement, to prevent replacing inside of a compound statement
          val stackTail = ctx.parent(0)
          stackTail match {
            case Some(_: Node.BlockStatement) =>
              if (!(replaced contains vName)) {
                replaced += vName
                val r = if (init(vName) == 0) "const" else "let"
                // use td.orig if possible to keep original initialization tokens
                //println(s"  Replaced $sr Node.Identifier with $r, init ${nodeClassName(right)}")
                VarDecl(vName.name, Some(right), r).withTokens(node)
              } else {
                node
              }
            case _ =>
              node
          }
        case _ =>
          //println(s"ss no match ${nodeClassName(node)}")
          node
      }
    }

    //println(s"transform done, replaced ${replaced.map(SymbolTypes.id).mkString(",")}")

    // walk the tree, remove the variable declarations
    changeAssignToVar.transformAfter{ (node, transformer) =>
      implicit val ctx = transformer.context
      // descend informs us how to descend into our children - cannot be used to descend into anything else
      node match {
        case VarDecl(Id(name), None, _) if replaced contains name =>
          Node.EmptyStatement()
        case c =>
          c
      }
    }
  }

  def nodeContainsRef(init: Node.Node, sym: SymId)(implicit ctx: ScopeContext) = {
    var found = false
    init.walkWithScope(ctx) { (node, context) =>
      implicit val ctx = context
      node match {
        case Node.Identifier(Id(`sym`)) =>
          found = true
          found
        case _ =>
          found
      }
    }
    found
  }

  object ExtractVariables {
    def unapply(n: Node.Node)(implicit ctx: ScopeContext): Option[Seq[(SymId, Node.Expression)]] = {
      val b = mutable.ArrayBuilder.make[(SymId, Node.Expression)]
      var assignsOnly = true
      n.walkWithScope(ctx) {(node, context) =>
        implicit val ctx = context
        node match {
          case _: Node.SequenceExpression =>
            false
          case Assign(Node.Identifier(Id(sym)), "=", init) if !nodeContainsRef(init, sym) =>
            b += sym -> init
            true
          case nn =>
            assignsOnly = false
            true
        }
      }
      val r = b.result()
      if (assignsOnly && r.nonEmpty) Some(r) else None
    }
  }

  def renameVariable[T <: Node.Node](n: T, oldName: SymId, newName: String): T = {
    n.transformAfter { (node, transformer) =>
      implicit val ctx = transformer.context
      node match {
        case sym@Node.Identifier(Id(`oldName`)) =>
          //println(s"  renamed ${oldName.name} to $newName")
          sym.name = newName
          sym
        case _ =>
          node
      }
    }
  }

  def replaceVariable[T <: Node.Node](n: T, oldName: SymId, newExpr: Node.Node): T = {
    n.transformAfter { (node, transformer) =>
      implicit val ctx = transformer.context
      node match {
        case Node.Identifier(Id(`oldName`)) =>
          newExpr.cloneNode().withTokens(node)
        case _ =>
          node
      }
    }
  }

  def replaceVariableInit[T <: Node.Node](n: T, oldName: SymId)(transform: (Node.Identifier, Node.Node) => Node.Node): T = {
    n.transformAfter { (node, transformer) =>
      implicit val ctx = transformer.context
      node match {
        case VarDecl(Id(`oldName`), init, _) =>
          init.map { init =>
            transform(Node.Identifier(oldName.name), init).withTokensDeep(node)
          }.getOrElse {
            Node.EmptyStatement()
          }
        case _ =>
          node
      }
    }
  }

  /**
    * when possible, introduce a var into the for loop
    * i.e. transform for (i = 0; ..) {} into for (var i = 0; ..)
    */
  def detectForVars(n: Node.Node): Node.Node = {
    val log = false
    // TODO: transformAfter changing AST, ForStatement equality test not working
    n.transformAfter { (node, transformer) =>
      implicit val ctx = transformer.context
      node match {
        case f: Node.ForStatement =>
          val forOK = Option(f.init).toSeq.flatMap {
            case _: Node.VariableDeclaration => // if init already is a definition, no need to process anything
              Seq()
            case ExtractVariables(vars) =>
              // we expect a sequence of variable initializations

              //println(s"Detect for with ${vars.map(_._1.name).mkString(",")}")
              // for each variable we need to verify the first use after the for loop is assignment
              // (or the variable is not used after the loop at all)
              // note: the assignment will often be in the init of another for loop
              val vScopes = vars.map(_._1)

              val forScopesOK = vScopes.forall { vId =>
                if (log) println(s"Checking $vId in ${f.simpleName}, scopes ${ctx.scopes.map(_._1.simpleName).mkString(":")}")
                // walk the scope, ignore references before the for, check first after the for
                var seenFor = false
                var seenAfterFor = false
                var seenAfterForInAssignment = false
                for {
                  (scope, ctx) <- ctx.findScopeById(vId.sourcePos)
                } {
                  scope.walkWithScope(ctx) {(node, context) =>
                    implicit val ctx = context
                    node match {
                      case `f` =>
                        if (log) println(s"Seen $vId in for")
                        seenFor = true
                        true // no need to dive into the for
                      case Assign(Node.Identifier(Id(`vId`)), "=", init) if seenFor && !seenAfterFor && !nodeContainsRef(init, vId) =>
                        if (log) println(s"Seen $vId after the for - in assignment")
                        seenAfterForInAssignment = true
                        seenAfterFor = true
                        true
                      case Node.Identifier(Id(`vId`)) if seenFor =>
                        if (log) println(s"Seen $vId after the for - in use")
                        seenAfterFor = true
                        true
                      case _ =>
                        //if (log) println(s"  Seen $node")
                        seenAfterFor
                    }
                  }
                }
                seenAfterForInAssignment || !seenAfterFor
              }

              if (forScopesOK) vars else Seq.empty
            case _ =>
              // something else than assignments into variables - leave it
              Seq()
          }
          if (forOK.nonEmpty) {
            if (log) println(s"Transform for $forOK")

            val vars = forOK.map { case (vId, initV) =>
              Node.VariableDeclarator(Node.Identifier(vId.name), initV).withTokens(initV)
            }

            f.init = Node.VariableDeclaration(vars, "let").withTokens(f)
            f
          } else {
            f
          }
        case _ =>
          node
      }
    }
  }

  def instanceofImpliedCast(n: NodeExtended): NodeExtended = {

    import Casting._

    object SingleCast {
      def unapply(arg: Node.IfStatement)(implicit context: ScopeContext): Option[(SymId, Seq[Node.Identifier], Node.Statement, Option[Node.Statement])] = arg match {
        // if (symDef instanceof cs)
        // if (symDef instanceof cs || symDef instanceof ds)
        case Node.IfStatement(InstanceOfCondition(symDef, cs), ifStatement, MayBeNull(elseStatement)) =>
          Some(symDef, cs, ifStatement, elseStatement)

        case _ =>
          None
      }
    }

    object SequenceOfCasts {
      def unapply(arg: Node.IfStatement)(implicit context: ScopeContext): Option[(SymId, Seq[(Seq[Node.Identifier], Node.Statement)], Option[Node.Statement])] = arg match {
        case SingleCast(symDef, cs, ifStatement, Some(SequenceOfCasts(symDef2, casts, elseStatement))) if symDef == symDef2 =>
          //println(s"Match ex ${symDef.name}")
          Some(symDef, (cs, ifStatement) +: casts, elseStatement)
        case SingleCast(symDef, cs, ifStatement, elseStatement) =>
          //println(s"Match ${symDef.name} elseStatement $elseStatement")
          Some(symDef, Seq((cs, ifStatement)), elseStatement)
        case _ =>
          None
      }
    }

    object ExpressionWithCasts {
      def unapplySeq(arg: Node.Node)(implicit context: ScopeContext): Option[Seq[(SymId, SymId)]] = {
        val buffer = ArrayBuffer.empty[(SymId, SymId)]
        arg.walk {
          case Binary(Node.Identifier(Id(symDef)), `instanceof`, Node.Identifier(Id(cs))) =>
            buffer.append((symDef, cs))
            false
          case _ =>
           false
        }
        if (buffer.isEmpty) None
        else Some(buffer)
      }
    }

    def condition(sym: Node.Identifier, cs: Seq[String]): Node.BinaryExpression = {
      cs match {
        case Seq(head) =>
          Binary (sym, asinstanceof, Node.Identifier(head))
        case head +: tail =>
          Node.BinaryExpression (
            "||",
            Binary(sym, asinstanceof, Node.Identifier(head)),
            condition(sym, tail)
          )
      }
    }

    def createCaseVariable(from: Node.Node, name: String, castTo: Seq[String]) = {
      //println(s"createCaseVariable $name $from ${from.start.get.pos}..${from.start.get.endpos}")
      val symRef = Node.Identifier(name)
      VarDecl(name + castSuffix, Some(condition(symRef, castTo)), "let").withTokens(from)
    }

    lazy val classInfo = Transform.listClassMembers(n)

    implicit object classOps extends ClassOps {
      def mostDerived(c1: ClassType, c2: ClassType) = {
        //println("mostDerived")
        classInfo.mostDerived(c1.name, c2.name).fold[TypeDesc](any)(ClassType)
      }

      def commonBase(c1: ClassType, c2: ClassType) = {
        //println("commonBase")
        classInfo.commonBase(c1.name, c2.name).fold[TypeDesc](any)(ClassType)
      }
    }

    def consolidateCasts(casts: Seq[(SymId, SymId)]): Seq[(SymId, String)] = {
      /*
      val varOrder = casts.map(_._1.name).zipWithIndex.toMap
      val castGroups = casts.groupBy(_._1)
      val castSets = castGroups.map { case (id, cg) =>
        id -> cg.map(_._2).map { clsSym =>
          val clsId = transform.classes.ClassId(clsSym)
          ClassType(clsId)
        }
      }
      castSets.map { case (id, cs) =>
        id -> cs.reduceLeft(typeUnion).toOut
      }.toSeq.sortBy(v => varOrder(v._1.name)) // sort by order in casts
      */
      casts.map {case (sym1, sym2) => sym1 -> sym2.name}
    }

    val ret = n.top.transformBefore { (node, descend, transformer) =>
      implicit val ctx = transformer.context
      node match {
        // note: handles one or multiple casts
        case s@SequenceOfCasts(symDef, casts, elseStatement) /*if casts.lengthCompare(1) > 0*/ =>
          val castVar = Node.Identifier(symDef.name)
          Node.SwitchStatement(
            castVar,
            casts.map { cast =>
              Node.SwitchCase (
                // we handle this in the ScalaOut as a special case, see CASE_CAST
                Node.CallExpression (
                  Node.Identifier("cast_^"),
                  {
                    val castExpr = condition(castVar, cast._1.map(_.name))
                    //args = js.Array(Node.Identifier.symDef(s)(symDef), castExpr)
                    Seq(castExpr)
                  }
                ),
                Seq(
                  Node.BlockStatement {
                    // without renaming I was unable to convince Uglify scoper (figure_out_scope) this is a new variable
                    val transformedBlock = Block.statements(cast._2).map(renameVariable(_, symDef, symDef.name + castSuffix))
                    createCaseVariable(s, symDef.name, cast._1.map(_.name)) +: transformedBlock
                  }.withTokens(s), Node.BreakStatement(null)
                )

              ).withTokens(s)
            } :+ new Node.SwitchCase (
              null,
              elseStatement.map { e =>
                Block.statements(e.transform(transformer))
              }.getOrElse(Seq())
            ).withTokens(s)
          ).withTokens(s)
        case ifs@Node.IfStatement(ex@ExpressionWithCasts(extractedCasts@_*), ifStatement, MayBeNull(elseStatement)) =>
          // check which cast variables are used in the ifStatement
          val used = listSymbols(ifStatement)
          val usedCasts = extractedCasts.filter { case (sym, _) =>
            used contains sym
          }

          val casts = consolidateCasts(usedCasts)
          //println(s"Detected casts ${casts.map(p => SymbolTypes.id(p._1) + " " + SymbolTypes.id(p._2))}")
          val n = Node.IfStatement (
            ex,
            Node.BlockStatement(
              casts.map { c =>
                createCaseVariable(ex, c._1.name, Seq(c._2))
              } ++ Block.statements(ifStatement).map { s =>
                casts.foldLeft(s) { (s, c) =>
                  renameVariable(s, c._1, c._1.name + castSuffix)
                }
              }
            ).withTokens(ifs),
            elseStatement.orNull
          )
          descend(n, transformer)
          n
        case _ =>
          //println(s"No match $node")
          val n = node.clone()
          descend(n, transformer)
          n

      }
    }

    val inConditionCast = ret.transformAfter { (node, transformer) =>
      implicit val ctx = transformer.context
      node match {
        // TODO: allow other forms of callOn, not only Node.Identifier
        case Binary(right@Binary(callExpr@Node.Identifier(Id(callOn)), "instanceof", classExpr), "&&", expr) =>
          //println(s"Detected cast && on $callExpr")
          val instancedExpr = Binary(callExpr, asinstanceof, classExpr)
          Variables.replaceVariable(expr, callOn, instancedExpr)
          Binary(right, "&&", expr)
        case _ =>
          node
      }
    }

    n.copy(top = inConditionCast)
  }

}
