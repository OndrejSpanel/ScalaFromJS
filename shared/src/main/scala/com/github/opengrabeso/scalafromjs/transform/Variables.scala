package com.github.opengrabeso.scalafromjs
package transform

import com.github.opengrabeso.scalafromjs.esprima._
import com.github.opengrabeso.esprima._
import Classes._
import SymbolTypes._
import Expressions._
import com.github.opengrabeso.scalafromjs
import com.github.opengrabeso.scalafromjs.esprima.symbols.{Id, ScopeContext, SymId, SymbolDeclaration}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions
import scala.collection.Seq

object Variables {
  import VariableUtils._
  import Symbols._

  // detect variables which can be declared as val instead of var
  def detectVals(n: Node.Node): Node.Node = Time("detectVals") {
    // walk the tree, check for possible val replacements and perform them
    val refs = buildReferenceStacks(n)(new ScopeContext)

    n.transformBefore {(node, descend, transformer) =>
      implicit val ctx = transformer.context
      node match {
        case cm: Node.MethodDefinition =>
          if (cm.key != null && propertyKeyName(cm.key) != inlineBodyName) {
            // no var detection inside of inline class body (its variables are actually class members)
            descend(cm, transformer)
            cm
          } else cm

        case VarDeclTyped(Id(varName), Some(value), kind, tpe) if kind != "const" => // var with init - search for a modification
          // check if any reference is assignment target

          // isModified call is quite slow this is quite slow (15 sec. of 18 in detectVals for three.js - total time 160 s)
          val assignedInto = refs.isModified(varName)
          val n = if (!assignedInto) {
            VarDecl(varName.name, Some(value), "const", tpe)(node)
          } else {
            node
          }
          descend(n, transformer) // may contain other scopes with initializations
          n
        case _ =>
          descend(node, transformer)
          node
      }
    }
  }


  // detect variable defined twice in a scope, remove the 2nd definition
  // for global do not remove the following defintions, rename them instead
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

    val declaredGlobal = mutable.Map.empty[SymId, Int] // add unique postfix
    val declaredLocal = mutable.Set.empty[SymId]

    def addIndex(s: String, i: Int) = if (i > 0) s + "$" + i.toString else s

    def isMethodIdentifier(name: Node.Identifier)(implicit ctx: ScopeContext) = {
      ctx.parent() match {
        case Some(Node.MethodDefinition(nameNode, _, _, _, _, _)) =>
          nameNode eq name
        case _ =>
          false
      }
    }
    n.transformBefore {(node, descend, transformer) =>
      implicit val ctx = transformer.context
      node match {
        case VarDeclTyped(Id(varName), init, kind, tpe) if ctx.scopes.last._1 == n && !usedAsForVar(varName.name, transformer) =>
          // declared in the top-level scope - special handling - if already exists, create a new variable
          assert(ctx.scopes.size == 1)
          val count = declaredGlobal.getOrElse(varName, -1)
          declaredGlobal += varName -> (count + 1)
          val ret = VarDecl(addIndex(varName.name, count + 1), init, kind, tpe)(node)
          // global variable body may contain references to other global variables
          descend(ret, transformer)

        // we could assume Node.Let and Node.Const are already well scoped
        case VarDecl(Id(varName), Some(value), _) if !usedAsForVar(varName.name, transformer) => // var with init - search for a modification
          //println(s"Node.VariableDeclarator ${varName.name}")
          //println(s"${varName.thedef.get.name} $varDef ${varName.thedef.get.orig} ${varName == varName.thedef.get.orig.head}")
          if (!(declaredLocal contains varName)) {
            declaredLocal += varName
            descend(node, transformer)
          } else {
            descend(Node.ExpressionStatement(Node.AssignmentExpression(
              left = Node.Identifier(varName.name).withTokens(node),
              operator = "=",
              right = value.cloneNode()
            )).withTokens(node), transformer)
          }
        case name@Node.Identifier(Id(id))
          // do not rename symbols used as class member identifiers
          if (declaredGlobal contains id) && !isMethodIdentifier(name) =>
          // $1, $2 ... used in three.js build as well
          Node.Identifier(addIndex(id.name, declaredGlobal(id))).withTokens(node)
        case _ =>
          descend(node, transformer)
      }
    }
  }

  // detect function key values which can be declared as concise methods instead
  def detectMethods(n: Node.Node): Node.Node = {
    val refs = buildReferenceStacks(n)(new ScopeContext)

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

              val newProps: Seq[Node.ObjectExpressionProperty] = props.map {
                case kv@Node.Property(kind, KeyName(k), _, AnyFun(args, body), _, _) =>
                  if (modifiedMembers contains k) {
                    //println(s"Modified member $k")
                    kv.kind = "value"
                    kv
                  } else {
                    newProperty("init", k, args, Block(body), kv)
                  }

                case p =>
                  p
              }
              val newObj = obj.cloneNode()
              newObj.properties = newProps
              newObj

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
            generator = false,
            null
          ).withTokensDeep(node)
        case _ =>
          node
      }
    }
  }

  /*
  * many transformations assume each Node.VariableDeclaration contains at most one Node.VariableDeclarator
  * Split multiple definitions (comma separated definition lists)
  * */
  def splitMultipleDefinitions(n: Node.Node): Node.Node = {

    def splitInBlock(body: Seq[Node.StatementListItem]) = body.flatMap {
      case defs: Node.VariableDeclaration =>
        defs.declarations.map { d =>
          val clone = defs.cloneNode() // keeps Node.VariableDeclaration type (Node.Var or whatever it is)
          clone.declarations = Seq(d)
          clone
        }
      case other =>
        Seq(other)
    }

    n.transformAfter { (node, transformer) =>
      node match {
        case block: Node.Program =>
          block.body = splitInBlock(block.body)
          block

        case block: Node.BlockStatement =>
          block.body = splitInBlock(block.body)
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
          null
        ) if thedef == thedef2 =>
          Some(thedef, right)
        case Node.IfStatement(
          Binary(Node.Identifier(Id(thedef)), "==" | "===", Node.Identifier("undefined")),
          SingleExpression(Assign(Node.Identifier(Id(thedef2)), "=", right)),
          null
        )  =>
          None
        case _ =>
          None
      }
    }



    // walk the tree, check for first reference of each var
    var defined = Map.empty[SymId, (String, Option[Node.TypeAnnotation])] // symbol definition -> first reference
    var used = Set.empty[SymId] // any use makes assignment not first
    val init = mutable.Map.empty[SymId, Int] // one or multiple assignments?
    val initAt = mutable.Map.empty[SymId, Node.Node] // one or multiple assignments?
    n.walkWithScope { (node, scopeContext) =>
      implicit val ctx = scopeContext
      node match {
        case VarDeclTyped(Id(name), None, kind, tpe) =>
          defined += name -> (kind, tpe)
          true
        //case fn: Node.MethodDefinition if methodName(fn) == inlineBodyName =>
        //  true

        case MatchInitWithAssign(name, _) if scopeContext.scopeId == name.sourcePos && !(used contains name) && (defined contains name) =>
          if (!(init contains name)) {
            init += name -> 0
            initAt += name -> node
          } else {
            init(name) += 1 // if already used, mark as var, not const
          }
          false

        case Assign(Node.Identifier(Id(name)), _, _) if init contains name =>
          // ignore use inside of the init statement
          if (!ctx.parents.contains(initAt(name))) {
            init(name) += 1 // if already used, mark as var, not const
          }
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
          val stackTail = ctx.parent()
          stackTail match {
            case Some(IsDeclScope()) =>
              if (!(replaced contains vName)) {
                replaced += vName
                //val r = if (init(vName) == 0) "const" else "let"
                val (kind, tpe) = defined(vName) // val detection will be done separately, no need to do it now
                // use td.orig if possible to keep original initialization tokens
                //println(s"  Replaced $sr Node.Identifier with $r, init ${nodeClassName(right)}")
                VarDecl(vName.name, Some(right), kind, tpe)(right)
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
          Node.EmptyStatement().withTokens(node)
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

  def renameVariable[T <: Node.Node](n: T, oldName: SymId, newName: String)(implicit ctx: ScopeContext): T = {
    n.transformAfter(ctx) { (node, transformer) =>
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

  def replaceVariable[T <: Node.Node](n: T, oldName: SymId, newExpr: => Node.Node)(implicit ctx: ScopeContext): T = {
    n.transformBefore(ctx) { (node, descend, transformer) =>
      implicit val ctx = transformer.context
      node match {
        case VarDecl(Id(`oldName`), _, _) => // do not replace in the declaration
          node
        case Node.Identifier(Id(`oldName`)) =>
          newExpr.cloneNode().copyLoc(node)
        case p: Node.Property =>
          // do not replace the variable in a key name
          descend(p.value, transformer)
          p
        case vd: Node.VariableDeclarator =>
          descend(vd.init, transformer)
          vd
        case AnyFun(_, body) =>
          // do not replace the variable in a parameter list
          ctx.withScope(body)(descend(body, transformer))
          node
        case _ =>
          descend(node, transformer)
          node
      }
    }
  }

  def replaceVariableInit[T <: Node.Node](n: T, oldName: SymId)(transform: (Node.Identifier, Node.Expression) => Node.Node)(implicit ctx: ScopeContext): T = {
    n.transformAfter(ctx) { (node, transformer) =>
      implicit val ctx = transformer.context
      node match {
        case VarDecl(Id(`oldName`), init, _) =>
          init.map { init =>
            transform(Node.Identifier(oldName.name), init).withTokensDeep(node)
          }.getOrElse {
            Node.EmptyStatement().withTokens(node)
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
              Node.VariableDeclarator(Node.Identifier(vId.name).withTokens(initV), initV, null).withTokens(initV)
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
        case Node.IfStatement(InstanceOfCondition(symDef, cs), ifStatement, elseStatement) =>
          Some(symDef, cs, ifStatement, Option(elseStatement))

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
          Binary (sym, asinstanceof, Node.Identifier(head).withTokens(sym))
        case head +: tail =>
          Node.BinaryExpression (
            "||",
            Binary(sym, asinstanceof, Node.Identifier(head).withTokens(sym)),
            condition(sym, tail)
          )
      }
    }

    def createCaseVariable(from: Node.Node, name: String, castTo: Seq[String]) = {
      //println(s"createCaseVariable $name $from ${from.start.get.pos}..${from.start.get.endpos}")
      val symRef = Node.Identifier(name).withTokens(from)
      VarDecl(name + castSuffix, Some(condition(symRef, castTo)), "let")(from)
    }

    lazy val classInfo = Transform.listClassMembers(n)

    implicit object classOps extends ClassOps {
      def mostDerived(c1: ClassType, c2: ClassType) = {
        //println("mostDerived")
        classInfo.mostDerived(c1.name, c2.name).fold[TypeDesc](any)(ClassType.apply)
      }

      def commonBase(c1: ClassType, c2: ClassType) = {
        //println("commonBase")
        classInfo.commonBase(c1.name, c2.name).fold[TypeDesc](any)(ClassType.apply)
      }
    }

    def consolidateCasts(casts: Seq[(SymId, SymId)]): Seq[(SymId, String)] = {
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
    }

    val ret = n.top.transformBefore { (node, descend, transformer) =>
      implicit val ctx = transformer.context
      node match {
        // note: handles one or multiple casts
        case s@SequenceOfCasts(symDef, casts, elseStatement) /*if casts.lengthCompare(1) > 0*/ =>
          val castVar = Node.Identifier(symDef.name).withTokens(s)
          Node.SwitchStatement(
            castVar,
            casts.map { cast =>
              Node.SwitchCase (
                // we handle this in the ScalaOut as a special case, see CASE_CAST
                Node.CallExpression (
                  Node.Identifier("cast_^").withTokens(s),
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
            } :+ Node.SwitchCase (
              null,
              elseStatement.map { e =>
                Block.statements(e.transform(transformer))
              }.getOrElse(Seq())
            ).withTokens(s)
          ).withTokens(s)
        case ifs@Node.IfStatement(ex@ExpressionWithCasts(extractedCasts@_*), ifStatement, elseStatement) =>
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
            elseStatement
          ).withTokens(ifs)
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
          val exprReplaced = Variables.replaceVariable(expr, callOn, instancedExpr)(ctx)
          Binary(right, "&&", exprReplaced)
        case _ =>
          node
      }
    }

    n.copy(top = inConditionCast)
  }

  def detectGlobalTemporaries(n: Node.Node): Node.Node = Time("detectGlobalTemporaries") {
    // scan the global scope for temporary variables to remove
    // for each variable: first option is erase state (last initialization), second is optional initialization
    var globals = mutable.Map.empty[SymId, Option[Option[Node.Expression]]]

    def matchName(s: String) = s.length > 1 && s(0) == '_'
    def nameToUse(s: String, localSymbols: collection.Set[String]) = {
      val wanted = s.drop(1)
      // in case of a name clash use the original name - that can never clash, as the variable could not be accessed otherwise
      if (!localSymbols.contains(wanted)) wanted
      else s
    }

    // the purpose of the construct it optimization. If the value is scalar, it makes no sense. Therefore if we see
    // a scalar (number or string) global, it is most likely a proper global
    def nonScalarInitializer(init: Node.Node): Boolean = init match {
      case Node.Literal(null, _) =>
        true
      case _: Node.TemplateLiteral =>
        false
      case Node.Literal(literal, _) =>
        !literal.is[Double] && !literal.is[String]
      case _: Node.ObjectExpression =>
        false
      case _ =>
        true
    }

    def recursiveInitializer(sym: SymId, init: Node.Node)(implicit context: ScopeContext): Boolean = {
      var recursive = false // when a variable is used in its own initialization, we cannot insert the declaration here
      context.withScope(init) {
        init.walkWithScope(context) ({ (node, context) =>
          implicit val ctx = context
          node match {
            case Node.Identifier(Id(`sym`)) =>
              recursive = true
              true // abort (note: we cannot return immediately, withScope needs to clean up)
            case _ =>
              false
          }
        })
      }
      recursive

    }

    def isGlobalTemporary(sym: SymId, init: Node.Node)(implicit context: ScopeContext) = nonScalarInitializer(init) && !recursiveInitializer(sym, init)

    object GlobalTemporary {
      def unapply(node: Node.Node)(implicit context: ScopeContext): Option[(SymId, Option[Node.Expression])] = node match {
        case VarDecl(Id(sym), init, _) if matchName(sym.name) && init.forall(isGlobalTemporary(sym, _)) =>
          Some(sym, init)
        case _ =>
          None
      }
    }
    n.walkWithScope {(node, context) =>
      implicit val ctx = context
      node match {
        case `n` => // enter into the top node ("module")
          false
        case GlobalTemporary(sym, init) => // global variable with or without (non-scalar) initialization
          globals += sym -> Some(init)
          true
        case _ =>
          true
      }
    }

    object GlobalVar {
      def unapply(name: String)(implicit context: ScopeContext): Option[SymId] = {
        val sym = Id(name)
        if (globals contains sym) Some(sym)
        else None
      }
    }

    val scopes = mutable.LinkedHashSet.empty[(SymId, Node.Node)] // caution: transform will invalidate Node references

    // identify scopes where the variables should be introduced
    n.walkWithScope { (node, context) =>
      implicit val ctx = context
      node match {
        case VarDecl(GlobalVar(_), _, _) =>
          true
        case Node.Identifier(GlobalVar(sym)) =>
          for (scope <- context.findFuncScope) {
            scopes += sym -> scope._1
          }
          true
        case _ =>
          false
      }
    }

    val globalUsedInSomeScope = scopes.map(_._1)

    // transform only variables which are used from some scope
    globals = globals.filter(g => globalUsedInSomeScope.contains(g._1))

    // TODO: check if each access is done from within the scope

    val scopeNodes = scopes.toSeq.groupBy(_._2).map {case (g, seq) =>
      g -> seq.map(_._1)
    }

    // find and handle all uses of the global variable
    val handle = n.transformBefore {(node, descend, transformer) =>
      implicit val ctx = transformer.context
      node match {
        case VarDecl(GlobalVar(sym), init, _) => // remove the original declaration
          init match {
            case Some(i) if isGlobalTemporary(sym, i) =>
              // track the last initialization so that replacement uses it, keep the short name
              globals += sym -> Some(init)
              Node.EmptyStatement().withTokens(node) // erase the declaration
            case None =>
              globals += sym -> Some(None)
              Node.EmptyStatement().withTokens(node) // erase the declaration
            case Some(i) => // non-temporary (scalar or object) initialization
              globals += sym -> None
              node // keep the declaration intact
          }
        case _ if scopeNodes contains node  =>
          // transform the node before we add the variables, so that the names are not present as the local symbols yet
          val syms = scopeNodes(node)
          val renamed = descend(node, transformer)
          // scope is some function
          def addDeclarations(block: Node.BlockStatement): Node.BlockStatement = {
            // check if the desired name is free in the block
            val symbols = SymbolDeclaration.declaredSymbols(block).to(mutable.LinkedHashSet)
            Node.BlockStatement(
              syms.flatMap { id =>
                // first level of the globals is whether the variable is erased now (last initialization may be scalar)
                globals(id).map(init => VarDecl(nameToUse(id.name, symbols), init, "var")(block))
              } ++ block.body
            ).withTokens(block)
          }
          renamed match {
            case Node.FunctionExpression(id, params, body, generator, tpe) =>
              Node.FunctionExpression(id, params, addDeclarations(body), generator, tpe).withTokens(node)
            case Node.FunctionDeclaration(id, params, body, generator, tpe) =>
              Node.FunctionDeclaration(id, params, addDeclarations(body), generator, tpe).withTokens(node)
            case _ => // ArrowFunctionExpression, Async
              renamed // we do not know how to introduce variables here
          }
        case Node.Identifier(Id(id)) =>
          globals.get(id).flatMap(_.map(_ /*init*/ => Node.Identifier(nameToUse(id.name, ctx.localSymbols)).withTokens(node))).getOrElse(node)
        case _ =>
          descend(node, transformer)
      }
    }

    handle
  }
}
