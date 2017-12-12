package com.github.opengrabeso
package transform

import JsUtils._
import net.gamatron.esprima._
import esprima._

import Classes._
import SymbolTypes._
import Expressions._
import Transform._

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
      node match {
        case cm: Node.ConciseMethod =>
          if (cm.key.name != inlineBodyName) {
            // no var detection inside of inline class body (its variables are actually class members)
            val n = cm.clone()
            descend(n, transformer)
            n
          } else cm.clone()

        case Node.Definitions(varDef@Node.VarDef(varName, value)) if value.nonEmpty => // var with init - search for a modification
          //println(s"Node.VarDef ${varName.name}")
          varName.thedef.fold(node) { df =>
            assert(df.name == varName.name)
            // check if any reference is assignment target

            val assignedInto = refs.isModified(df)
            val n = if (!assignedInto) {
              val c = varDef.clone()
              c.name = new Node.SymbolConst {
                fillTokens(this, varName)
                init = varName.init
                name = varName.name
                scope = varName.scope
                thedef = varName.thedef
              }
              new Node.Const {
                fillTokens(this, node)
                definitions = js.Array(c)
              }
            } else {
              node.clone()
            }
            descend(n, transformer) // may contain other scopes with initializations
            n
          }
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
    def usedAsForVar(varName: Node.SymbolVarOrConst, transformer: TreeTransformer) = {
      def varIn(node: Node.Node) = {
        var present = false
        node walk {
          case `varName` =>
            present = true
            present
          case _ =>
            present
        }
        present
      }
      transformer.parent().exists {
        case f: Node.For if f.init.exists(varIn) || f.step.exists(varIn) || f.condition.exists(varIn) =>
          true
        case _ =>
          false

      }
    }

    n.transformAfter {(node, transformer) =>
      node match {
        // match only Node.Var, assume Node.Let and Node.Const are already well scoped
        case Node.Var(Node.VarDef(varName, Defined(value))) if !usedAsForVar(varName, transformer) => // var with init - search for a modification
          // if the orig is different from this, it is suspicions
          //println(s"Node.VarDef ${varName.name}")
          //println(s"${varName.thedef.get.name} $varDef ${varName.thedef.get.orig} ${varName == varName.thedef.get.orig.head}")

          def countDefsInScope(varDef: SymbolDef, scope: Node.Scope) = {
            var count = 0
            var list = mutable.ArrayBuffer.empty[Node.Token]

            varDef.scope.walk {
              case n@Node.Var(Node.VarDef(Node.Identifier(`varDef`), _)) =>
                count += 1
                list ++= n.start
                false
              case scope: Node.Block if scope != varDef.scope =>
                // check only in the same block
                // this is not exactly what JS does, but better transforms to Scala
                // e.g. if () {var a = 1} else {var a = 2}
                true
              case _ =>
                false
            }
            if (count <= 1) {
              //println(s"Not multiple definitions for $varName in $scope, $count")
            } else {
              //println(s"Multiple definitions for $varName in $scope, $count - ${list.map(_.line).mkString(",")}")

            }
            count
          }
          val replaced = for {
            varDef <- varName.thedef
            //scope <- varName.scope
            scope = varDef.scope
            // if the symbol origin is different from this declaration, it is a candidate
            if varDef.orig.headOption.exists(_ != varName)
            // verify the suspicion
            count = countDefsInScope(varDef, scope)
            if count > 1
          } yield {
            //println(s"Multiple definitions for $varName ($count) in $scope - decl $node")
            new Node.Assign {
              fillTokens(this, node)
              left = Node.Identifier.sym(node)(varName)
              operator = "="
              right = value.clone()
            }
          }
          replaced.getOrElse(node)
        case _ =>
          node
      }
    }
  }

  // detect function key values which can be declared as concise methods instead
  def detectMethods(n: Node.Node): Node.Node = {
    val refs = buildReferenceStacks(n)

    n.transformBefore {(node, descend, transformer) =>
      node match {
        case obj@Node.Object(props) =>
        //case Node.Definitions(Node.VarDef(Node.Identifier(df), Defined(obj@Node.Object(props)))) =>

          // check if the object is part of variable / const initialization, like: var df = {}
          transformer.parent(1) match {
            case Some(Node.Definitions(Node.VarDef(Node.Identifier(df), Defined(o: Node.Object)))) =>
              //println(s"Scan object ${df.name} for methods ${o.properties}")
              assert(o == obj)

              object IsDfMember extends Extractor[String] {
                def unapply(arg: Node.Node) = arg match {
                  case Node.Identifier(`df`) Dot key => Some(key)
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

              val newProps = props.map {
                case kv@Node.ObjectKeyVal(k, f@Node.Function(args, body)) =>
                  if (modifiedMembers contains k) {
                    //println(s"Modified member $k")
                    kv
                  } else newMethod(k, args, body, f)

                case p => p
              }
              val newObj = obj.clone()
              newObj.properties = newProps.toJSArray
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
    n.transformAfter { (node, _) =>
      node match {
        case Node.Const(Node.VarDef(sym, Defined(Node.Function(args, body)))) =>
          new Node.Defun {
            defun =>
            name = new Node.SymbolDefun {
              name = sym.name
              thedef = sym.thedef
              scope = sym.scope
              init = js.Array[Node.Node](defun)
              /*_*/
              fillTokens(this, node)
              /*_*/
            }
            fillTokens(this, node)
            argnames = args.toJSArray
            this.body = body.toJSArray
          }
        case _ => node
      }
    }
  }

  /*
  * many transformations assume each Node.Definitions contains at most one Node.VarDef
  * Split multiple definitions (comma separated definitiob lists)
  * */
  def splitMultipleDefinitions(n: Node.Node): Node.Node = {
    n.transformAfter { (node, transformer) =>
      node match {
        case block: Node.Block =>
          block.body = block.body.flatMap {
            case defs: Node.Definitions =>
              defs.definitions.map { d =>
                val clone = defs.clone() // keeps Node.Definitions type (Node.Var or whatever it is)
                clone.definitions = js.Array(d)
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

    // walk the tree, check for first reference of each var
    var pairs = Map.empty[SymbolDef, (Node.Identifier, Boolean)] // symbol definition -> first reference
    n.walk { node =>
      node match {
        case defs@Node.Definitions(Node.VarDef(name, value)) if value.isEmpty =>
          //println(s"varInitialization Node.VarDef $name")
          for (df <- name.thedef) {
            assert(df.name == name.name)
            //println(s"  refs ${df.references}")
            if (df.references.nonEmpty) {
              // find the first reference
              val firstRef = df.references.minBy { ref =>
                assert(ref.thedef contains df)
                ref.start.map(_.pos).getOrElse(Int.MaxValue)
              }
              // if the first ref is in the current scope, we might merge it with the declaration
              if (firstRef.scope == name.scope) {
                pairs += df -> (firstRef, defs.isInstanceOf[Node.Const])
              }
            }
          }
        case _ =>
      }
      false
    }

    val refs = pairs.values.map(_._1).toSet
    var replaced = Set.empty[SymbolDef]

    //println(s"transform, vars ${pairs.keys.map(SymbolTypes.id).mkString(",")}")

    object MatchInitWithAssign {
      def unapply(arg: Node.Node) = arg match {
        // sr = xxx
        case Node.SimpleStatement(Node.Assign(sr@Node.Identifier(name, scope, thedef), "=", right)) if refs contains sr =>
          Some(sr, name, scope, thedef, right)
        // if (m1 == undefined) m1 = xxx
        case Node.IfStatement(
        Node.BinaryExpression(sr@Node.Identifier(name, scope, thedef), "==" | "===", Node.Identifier("undefined")),
        SingleStatement(Node.Assign(sr2@Node.Identifier(_, _, thedef2), "=", right)),
        None
        ) if thedef == thedef2 && (refs contains sr) =>
          Some(sr, name, scope, thedef, right)
        case _ => None
      }
    }

    // walk the tree, check for possible val replacements and perform them
    val changeAssignToVar = n.transformAfter {(node, transformer) =>
      // descend informs us how to descend into our children - cannot be used to descend into anything else
      //println(s"node ${nodeClassName(node)} ${ScalaOut.outputNode(node, "")}")
      // we need to descend into assignment definitions, as they may contain other assignments
      node match {
        case MatchInitWithAssign(sr, vName, vScope, Defined(td), right) =>
          //println(s"ss match assign ${nodeClassName(left)} ${ScalaOut.outputNode(left, "")}")
          // checking if inside of a block statement, to prevent replacing inside of a compound statement
          val stackTail = transformer.stack.takeRight(2).dropRight(1).toSeq
          stackTail match {
            case Seq(_: Node.Block) =>
              replaced += td
              val isVal = pairs(td)._2
              val r = if (isVal) new Node.Const else new Node.Var
              // use td.orig if possible to keep original initialization tokens
              val origNode = td.orig.headOption.getOrElse(node)
              fillTokens(r, origNode)
              r.definitions = js.Array(Node.VarDef.initializedSym(origNode)(td, right))
              //println(s"  Replaced $sr Node.Identifier with $r, init ${nodeClassName(right)}")
              r
            case _ =>
              node
          }
        case _ =>
          //println(s"ss no match ${nodeClassName(node)}")
          node
      }
    }

    //println(s"transform done, replaced ${replaced.map(SymbolTypes.id).mkString(",")}")

    pairs = pairs.filterKeys(replaced.contains)

    // walk the tree, check for possible val replacements and perform them
    changeAssignToVar.transformAfter{ (node, _) =>
      // descend informs us how to descend into our children - cannot be used to descend into anything else
      node match {
        case v: Node.Definitions =>
          // remove only the original declaration, not the one introduced by us
          // original declaration has no init value
          val af = v.definitions.filterNot { d =>
            d.value.isEmpty && d.name.thedef.exists(pairs.contains)
          }
          //if (af.size != v.definitions.size) println(s"  removed decl $v -> $af")
          v.definitions = af
          v
        case c =>
          c
      }
    }
  }

  def nodeContainsRef(init: Node.Node, sym: SymbolDef) = {
    var found = false
    init.walk {
      case Node.Identifier(`sym`) =>
        found = true
        found
      case _ =>
        found
    }
    found
  }

  object ExtractVariables {
    def unapply(n: Node.Node): Option[Seq[(SymbolDef, Node.Node)]] = {
      val b = mutable.ArrayBuilder.make[(SymbolDef, Node.Node)]
      var assignsOnly = true
      n.walk {
        case _ : Node.Sequence =>
          false
        case Node.Assign(Node.Identifier(sym), "=", init) if !nodeContainsRef(init, sym) =>
          b += sym -> init
          true
        case nn =>
          assignsOnly = false
          true
      }
      val r = b.result()
      if (assignsOnly && r.nonEmpty) Some(r) else None
    }
  }

  def renameVariable[T <: Node.Node](n: T, oldName: SymbolDef, newName: String, newSymbol: Option[SymbolDef] = js.undefined): T = {
    val ret = n.transformAfter { (node, _) =>
      node match {
        case sym@Node.Identifier(`oldName`) =>
          //println(s"  renamed ${oldName.name} to $newName")
          sym.name = newName
          sym.thedef = newSymbol // scope and definition needs to be filled by the parser
          sym.scope = js.undefined
          sym
        case _ =>
          node
      }
    }
    ret.asInstanceOf[T]
  }

  def replaceVariable[T <: Node.Node](n: T, oldName: SymbolDef, newExpr: Node.Node): T = {
    val ret = n.transformAfter { (node, _) =>
      node match {
        case Node.Identifier(`oldName`) =>
          val r = newExpr.clone()
          fillTokensRecursively(r, node)
          //println(s"replaceVariable ${oldName.name} $newExpr")
          r
        case _ =>
          node
      }
    }
    ret.asInstanceOf[T]
  }

  def replaceVariableInit[T <: Node.Node](n: T, oldName: SymbolDef)(transform: (Node.Identifier, Node.Node) => Node.Node): T = {
    val ret = n.transformAfter { (node, _) =>
      node match {
        case Node.Definitions(varDef@Node.VarDef(Node.Identifier(`oldName`), init)) =>
          init.map { init =>
            val r = transform(varDef.name, init)
            fillTokensRecursively(r, node)
            r
          }.getOrElse {
            Node.EmptyStatement(node)
          }
        case _ =>
          node
      }
    }
    ret.asInstanceOf[T]

  }

  /**
    * when possible, introduce a var into the for loop
    * i.e. transform for (i = 0; ..) {} into for (var i = 0; ..)
    */
  def detectForVars(n: Node.Node): Node.Node = {
    n.transformAfter { (node, _) =>
      node match {
        case f: Node.For =>
          val forOK: Seq[(SymbolDef, Node.Node, Node.Scope)] = f.init.toSeq.flatMap {
            case _: Node.Definitions => // if init already is a definition, no need to process anything
              Seq()
            case ExtractVariables(vars) =>
              // we expect a sequence of variable initializations

              //println(s"Detect for with ${vars.map(_._1.name).mkString(",")}")
              // for each variable we need to verify the first use after the for loop is assignment
              // (or the variable is not used after the loop at all)
              // note: the assignment will often be in the init of another for loop
              val vScopes = for {
                (v, init) <- vars
                Node.Identifier(_, Defined(scope), _ ) <- v.orig.headOption
              } yield {
                (v, init, scope)
              }

              val forScopesOK = vScopes.forall { case (v, _, scope) =>
                // walk the scope, ignore references before the for, check first after the for
                var seenFor = false
                var seenAfterFor = false
                var seenAfterForInAssignment = false
                scope.walk {
                  case `f` =>
                    seenFor = true
                    true // no need to dive into the for
                  case Node.Assign(Node.Identifier(`v`), "=", init) if seenFor && !seenAfterFor && !nodeContainsRef(init, v) =>
                    //println(s"Seen ${v.name} after the for - in assignment")
                    seenAfterForInAssignment = true
                    seenAfterFor = true
                    true
                  case Node.Identifier(`v`) if seenFor =>
                    //println(s"Seen ${v.name} after the for - in use")
                    seenAfterFor = true
                    true
                  case _ =>
                    seenAfterFor

                }
                seenAfterForInAssignment || !seenAfterFor
              }

              if (forScopesOK) vScopes else Seq.empty
            case _ =>
              // something else than assignments into variables - leave it
              Seq()
          }
          if (forOK.nonEmpty) {
            //println("Transform for")

            val vars = forOK.map { case (v, initV, _) =>
              Node.VarDef.initialized(initV)(v.name, initV)
            }

            f.init = Node.Let(f)(vars:_*)
            f
          } else {
            f
          }
        case _ =>
          node
      }
    }
    n

  }

  def instanceofImpliedCast(n: NodeExtended): NodeExtended = {

    import Casting._

    object SingleCast {
      def unapply(arg: Node.IfStatement): Option[(SymbolDef, Seq[Node.Identifier], Node.Statement, Option[Node.Statement])] = arg match {
        // if (symDef instanceof cs)
        // if (symDef instanceof cs || symDef instanceof ds)
        case Node.IfStatement(InstanceOfCondition(symDef, cs), ifStatement, elseStatement) =>
          Some(symDef, cs, ifStatement, elseStatement)

        case _ =>
          None
      }
    }

    object SequenceOfCasts {
      def unapply(arg: Node.IfStatement): Option[(SymbolDef, Seq[(Seq[Node.Identifier], Node.Statement)], Option[Node.Statement])] = arg match {
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
      def unapplySeq(arg: Node.Node): Option[Seq[(SymbolDef, SymbolDef)]] = {
        val buffer = ArrayBuffer.empty[(SymbolDef, SymbolDef)]
        arg.walk {
          case Node.BinaryExpression(Node.Identifier(symDef), `instanceof`, Node.Identifier(cs)) =>
            buffer.append((symDef, cs))
            false
          case _ =>
           false
        }
        if (buffer.isEmpty) None
        else Some(buffer)
      }
    }

    def condition(sym: Node.Identifier, cs: Seq[String])(from: Node.Node): Node.BinaryExpression = {
      cs match {
        case Seq(head) =>
          Node.BinaryExpression(from) (sym, asinstanceof, Node.Identifier(from)(head))
        case head +: tail =>
          Node.BinaryExpression(from) (
            Node.BinaryExpression(from) (sym, asinstanceof, Node.Identifier(from)(head)),
            "||",
            condition(sym, tail)(from)
          )
      }
    }

    def makeBlock(s: Node.Statement): js.Array[Node.Statement] = {
      s match {
        case b: Node.BlockStatement =>
          b.body
        case _ =>
          js.Array(s)
      }
    }

    def createCaseVariable(from: Node.Node, name: String, castTo: Seq[String]) = {
      //println(s"createCaseVariable $name $from ${from.start.get.pos}..${from.start.get.endpos}")
      val symRef = Node.Identifier(from)(name)
      Node.Let(from)(Node.VarDef.initialized(from)(name + castSuffix, condition(symRef, castTo)(from)))
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

    def consolidateCasts(casts: Seq[(SymbolDef, SymbolDef)]): Seq[(SymbolDef, String)] = {
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
      node match {
        // note: handles one or multiple casts
        case s@SequenceOfCasts(symDef, casts, elseStatement) /*if casts.lengthCompare(1) > 0*/ =>
          val castVar = Node.Identifier.symDef(s)(symDef)
          new Node.Switch {
            expression = castVar
            this.body = casts.map { cast =>
              new Node.Case {
                // we handle this in the ScalaOut as a special case, see CASE_CAST
                expression = new Node.Call() {
                  fillTokens(this, s)
                  expression = Node.Identifier(s)("cast_^")
                  val castExpr = condition(castVar, cast._1.map(_.name))(s)
                  //args = js.Array(Node.Identifier.symDef(s)(symDef), castExpr)
                  args = js.Array(castExpr)
                }
                this.body = js.Array(new Node.BlockStatement {
                  fillTokens(this, s)
                  // without renaming I was unable to convince Uglify scoper (figure_out_scope) this is a new variable
                  val transformedBlock = makeBlock(cast._2).map(renameVariable(_, symDef, symDef.name + castSuffix))
                  this.body = createCaseVariable(s, symDef.name, cast._1.map(_.name)) +: transformedBlock
                }, new Node.Break().withTokens(s))

              }.withTokens(s):Node.Statement
            }.toJSArray :+ new Node.Default {
              this.body = elseStatement.map { e =>
                makeBlock(e.transform_js(transformer).asInstanceOf[Node.Statement])
              }.getOrElse(js.Array())
            }.withTokens(node)
          }.withTokens(node)
        case ifs@Node.IfStatement(ex@ExpressionWithCasts(extractedCasts@_*), ifStatement, elseStatement) =>
          // check which cast variables are used in the ifStatement
          val used = listSymbols(ifStatement)
          val usedCasts = extractedCasts.filter { case (sym, _) =>
            used contains sym
          }

          val casts = consolidateCasts(usedCasts)
          //println(s"Detected casts ${casts.map(p => SymbolTypes.id(p._1) + " " + SymbolTypes.id(p._2))}")
          val n = new Node.IfStatement {
            fillTokens(this, ifs)
            condition = ex
            body = new Node.BlockStatement {
              fillTokens(this, ifStatement)
              this.body = (casts.map { c =>
                createCaseVariable(ex, c._1.name, Seq(c._2))
              } ++ makeBlock(ifStatement).map { s =>
                casts.foldLeft(s) { (s, c) =>
                  renameVariable(s, c._1, c._1.name + castSuffix)
                }
              }).toJSArray
            }
            alternative = elseStatement.orUndefined
          }
          descend(n, transformer)
          n
        case _ =>
          //println(s"No match $node")
          val n = node.clone()
          descend(n, transformer)
          n

      }
    }

    val inConditionCast = ret.transformAfter { (node, _) =>
      implicit val tokensFrom = node
      node match {
        // TODO: allow other forms of callOn, not only Node.Identifier
        case Node.BinaryExpression(right@Node.BinaryExpression(callExpr@Node.Identifier(callOn), "instanceof", classExpr), "&&", expr) =>
          //println(s"Detected cast && on $callExpr")
          val instancedExpr = Node.BinaryExpression(node)(callExpr, asinstanceof, classExpr)
          Variables.replaceVariable(expr, callOn, instancedExpr)
          Node.BinaryExpression(node)(right, "&&", expr)
        case _ =>
          node
      }
    }

    n.copy(top = inConditionCast)
  }

}
