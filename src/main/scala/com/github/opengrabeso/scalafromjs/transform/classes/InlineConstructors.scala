package com.github.opengrabeso.scalafromjs
package transform
package classes

import com.github.opengrabeso.scalafromjs.esprima._
import com.github.opengrabeso.esprima._
import Classes._
import Expressions._
import Variables._
import Symbols._
import SymbolTypes._
import VariableUtils._
import com.github.opengrabeso.scalafromjs
import com.github.opengrabeso.scalafromjs.esprima.symbols.{Id, ScopeContext, SymId}

object InlineConstructors {
  private case class PrivateMember(sym: SymId, isVal: Boolean, tokens: Node.Node)

  private object AssignToMember {
    def unapply(arg: Node.Node): Option[(String, Node.Node)] = arg match {
      case SingleExpression(Assign(Node.ThisExpression() Dot funName, "=", value)) =>
        Some(funName, value)
      case _ =>
        None
    }
  }
  private object DefinePrivateFunction {
    def unapply(arg: Node.Statement): Option[(String, Node.FunctionExpression)] = arg match {
      case AssignToMember(funName, lambda: Node.FunctionExpression) =>
        Some(funName, lambda)
      case _ =>
        None
    }
  }

  private def detectPrivateFunctions(n: Node.Node) = {
    // detect exported functions
    var localLambdas = Set.empty[Node.Node]
    n.walk {
      case fun@AnyFun(_, _) if fun != n =>
        localLambdas += fun
        true
      case _ =>
        false
    }

    //println(s"localLambdas $localLambdas")
    // detect functions which are not exported - i.e. local named function not assigned to anything

    def localLambdaByName(name: String): Option[Node.Node] = {
      localLambdas.collectFirst {
        case f: Node.FunctionDeclaration if f.id.name == name =>
          f
      }
    }

    var isExported = Set.empty[Node.Node]
    n.walk {
      case DefinePrivateFunction(_, lambda) =>
        assert(localLambdas contains lambda)
        isExported += lambda
        true
      case AssignToMember(_, Node.Identifier(funName)) =>
        val isLocal = localLambdaByName(funName)
        isExported ++= isLocal
        isLocal.nonEmpty
      case _ =>
        false
    }


    val definedFunctions = localLambdas.filter(_.isInstanceOf[DefFun])
    //println(s"definedFunctions $definedFunctions")

    var definedAndExported = Set.empty[Node.Node]
    def scanIds(n: Node.Node): Boolean =  n match {
      case Node.CallExpression(Node.Identifier(_), _) =>
        true // a plain call, do not dive into
      case f: Node.FunctionDeclaration =>
        // skip function declaration node id
        f.body.walk(scanIds)
        true
      case Node.Identifier(funName) =>
        definedAndExported ++= localLambdaByName(funName)
        true
      case _ =>
        false
    }

    n.walk(scanIds)

    //println(s"definedAndExported $definedAndExported")

    // select all lambdas with exception of plain defined functions which are not exported
    val perhapsExported = localLambdas -- (definedFunctions -- definedAndExported)

    //println(s"isExported $isExported")
    //println(s"perhapsExported $perhapsExported")

    val exportedDirectly = isExported ++ perhapsExported

    //println(s"exportedDirectly $exportedDirectly")

    // detect calls from exported functions

    def transitiveClosure(functions: Set[Node.Node]): Set[Node.Node] = {
      var called = functions
      var someChange = false
      def callback(node: Node.Node, walker: ScopeContext): Boolean = {
        implicit val ctx = walker
        node match {
          // any use other than a definition should be enough to export the function as well
          case f: Node.FunctionExpression =>
            f.body.walkWithScope(ctx)(callback)
            false
          case Node.Identifier(sym) =>
            for {
              fun <- localLambdaByName(sym)
              inFunction <- walker.stack.reverse.collectFirst {
                case c@AnyFun(_, _) =>
                  //println(s"Found ${c.name.map(_.name)}")
                  c
              }
            } {
              if ((called contains inFunction) && !(called contains fun)) {
                called += fun
                someChange = true
              }
            }

            false
          case _ =>
            false
        }
      }

      n.walkWithScope(callback _)

      if (someChange) transitiveClosure(called) else called
    }


    transitiveClosure(exportedDirectly)
  }

  private def listLocalVariables(n: Node.Node)(implicit ctx: ScopeContext): Map[SymId, Node.Node] = {
    // list all variables
    var variables = Map.empty[SymId, Node.Node]
    n.walkWithScope(ctx) {(node, context) =>
      implicit val ctx = context
      node match {
        case VarDecl(Id(name), _, _) =>
          variables += name -> node
          false
        // do not enter any functions, we extract only variables directly in the constructor
        case AnyFun(_, _) =>
          true
        case _ =>
          false
      }
    }
    variables
  }

  private def detectPrivateMembers(n: Node.Node, variables: Map[SymId, Node.Node])(implicit ctx: ScopeContext): Seq[PrivateMember] = {
    // any variable defined in the main body scope and references from any function is considered a private member
    //n.variables = Dictionary.empty[SymbolDef]

    val refs = buildReferenceStacks(n)

    val functions = detectPrivateFunctions(n)

    // check if they are references from any private function
    var privates = Set.empty[SymId]

    n.walkWithScope(ctx) { (node, context) =>
      implicit val ctx = context
      node match {
        case Node.Identifier(Id(name)) =>
          if (context.scopes.exists { scope =>
            functions contains scope._1
          }) {
            privates += name
          }
          false
        case _ =>
          false

      }
    }

    privates.toSeq.flatMap { priv =>

      //println(s"  priv ${priv.name} ${refs.refs.get(priv)} ")
      // check which members are ever written to - we can convert all others to getters and methods
      val modified = refs.isModified(priv)

      variables.get(priv).map {
        PrivateMember(priv, !modified, _)
      }

    }
  }


  def privateVariables(n: Node.Node): Node.Node = {
    n.transformAfter { (node, transformer) =>
      implicit val ctx = transformer.context
      node match {
        case cls: Node.ClassDeclaration =>
          for {
            constructorProperty@Node.MethodDefinition(_, _, _, AnyFun(params, body), _, _) <- findConstructor(cls)
          } ctx.withScope(cls.body, constructorProperty, constructorProperty.value) {

            // constructor parameters need a different handling
            val parameterLocals = params.map(p => Id(parameterNameString(p)) -> p).toMap

            val constructorLocals = listLocalVariables(body)

            val allPrivates = detectPrivateMembers(body, constructorLocals ++ parameterLocals)

            // convert private variables to members (TODO: mark them as private somehow)

            //println(s"Locals ${allPrivates.map(l => l.sym.name -> l.isVal)}")


            //val (privateParameters, privateLocals) = allPrivates.partition(l => parameterLocals.contains(l.sym))

            //println(s"Locals ${allPrivates.map(l => l.sym.name -> l.isVal)}")
            //println(s"Parameters ${localsForParameters.map(l => l.sym.name -> l.isVal)}")

            def newThisDotMember(member: String) = Dot(Node.ThisExpression(), Node.Identifier(member))

            val bodyWithRenamedPrivates = allPrivates.foldLeft(body) { (constructor, privateVar) =>
              // first replace variable use - without init it is no longer declared
              val replaced = replaceVariable(constructor, privateVar.sym, newThisDotMember(privateVar.sym.name).withTokens(privateVar.tokens))
              //println(s"Replaced private var ${privateVar.sym.name}")
             if (parameterLocals.contains(privateVar.sym)) {
                // rename constructor parameter, assign it into a this.xxx
                // transform constructor(x) {} into constructor(x){this.x = x}
                val block = Block(constructor).withTokens(constructor)
                def newPrivateVarIdentifier = Node.Identifier(privateVar.sym.name).withTokens(privateVar.tokens)
                val addPrefix: Node.StatementListItem = Node.ExpressionStatement(
                  Assign(Dot(Node.ThisExpression(), newPrivateVarIdentifier).withTokens(privateVar.tokens), "=", newPrivateVarIdentifier)
                )
                block.body = addPrefix +: block.body
                block
              } else {
                replaceVariableInit(replaced, privateVar.sym) { (sym, init) =>
                  Node.ExpressionStatement (
                    Assign(newThisDotMember(sym.name), "=", init.cloneNode())
                  )
                }
              }
            }

            constructorProperty.value match {
              case f: Node.FunctionExpression =>
                f.body.body = Block.statements(bodyWithRenamedPrivates)
            }

            // DRY: FillVarMembers
            val clsTokenDef = classTokenSource(cls)

            val vars = allPrivates.map { local =>
              val varDecl = VarDecl(local.sym.name, None, if (local.isVal) "const" else "var", local.tokens)
              //println(s"privateVariables ${local.sym.name} $varDecl ${cls.start.get.pos}")
              varDecl
            }

            if (vars.nonEmpty) {
              val accessor = getMethodBody(classInlineBody(cls, clsTokenDef)).get

              accessor.body ++= vars
              //println(s"privateVariables newMembers $vars")

              // remove overwritten members
              //cls.properties = cls.properties.filterNot(p => newMembers.contains(propertyName(p)))
            }

          }
          cls
        case _ =>
          node
      }
    }
  }

  def privateFunctions(n: Node.Node): Node.Node = {
    val log = false

    n.transformAfter { (node, t) =>
      implicit val ctx = t.context
      node match {
        case cls: Node.ClassDeclaration =>
          for {
            constructorProperty@Node.MethodDefinition(_, _, _, AnyFun(params, body), _, _) <- findConstructor(cls)
          } {
            val functionsToConvert = detectPrivateFunctions(body)
            if (log) println(s"functionsToConvert $functionsToConvert")

            val bodyStatements = Block.statements(body)

            val functions = bodyStatements.collect {
              case s@DefinePrivateFunction(funName, f) if functionsToConvert contains f =>
                (s, funName, f)
              case s@DefFun(Defined(Node.Identifier(funName)), _, _, _, _) if functionsToConvert contains s =>
                (s, funName, s)
            }

            val restRaw = bodyStatements diff functions.map(_._1)

            val rest = restRaw.filter {
              case Node.ExpressionStatement(Assign(Node.ThisExpression() Dot tgtName, "=", Node.Identifier(funName)))
                // TODO: relax tgtName == funName requirement
                if tgtName == funName && functionsToConvert.collectFirst {
                  case f: Node.FunctionExpression if Option(f.id).exists(_.name == funName) => f
                }.nonEmpty =>
                //if (log) println(s"Drop assign $funName")
                false
              case _ =>
                true

            }

            constructorProperty.value match {
              case f: Node.FunctionExpression =>
                f.body.body = rest
            }

            val parameterMembers = params.map(p => Id(parameterNameString(p))).toSet

            def transformParametersInBody(node: Node.BlockStatement)(ctx: ScopeContext) = {
              node.transformAfter(ctx) {(node, transformer) =>
                implicit val ctx = transformer.context
                node match {
                  case Node.Identifier(Id(id)) =>
                    if (parameterMembers contains id) {
                      Dot(Node.ThisExpression(), Node.Identifier(id.name).withTokens(node)).withTokens(node)
                    } else {
                      node
                    }
                  case _ =>
                    node
                }
              }
            }

            // add functions as methods
            cls.body.body = cls.body.body ++ functions.map {
              case (statement, funName, fun@AnyFun(funParams, funBody)) =>
                ctx.withScope(cls.body, fun, funBody) {
                  Node.MethodDefinition(
                    Node.Identifier(funName).withTokens(statement),
                    null,
                    false,
                    Node.FunctionExpression(null, funParams, transformParametersInBody(Block(funBody).withTokens(body))(ctx), false, null).withTokens(statement),
                    "init",
                    false
                  )
                }

            }
          }
          cls
        case _ =>
          node
      }
    }
  }

  def apply(n: NodeExtended): NodeExtended = {
    var types = n.types
    val r = n.top.transformAfter { (node, transformer) =>
      implicit val ctx = transformer.context
      node match {
        case cls: Node.ClassDeclaration =>
          val classSymIds = SymbolIds(cls)(ctx)

          ctx.withScope(cls.body) {

            val clsTokenDef = classTokenSource(cls)
            for {
              md <- findConstructor(cls)
              constructorProperty@AnyFun(params, b) <- Some(md.value)
            } {
              // anything before a first variable declaration can be inlined, variables need to stay private
              val body = Block.statements(b)
              val (inlined, rest_?) = body.span {
                case _: Node.VariableDeclaration => false
                case _ => true
              }

              def isClassConstant(varName: String): Boolean = {
                val body = findInlineBody(cls).flatMap(getMethodBody)
                body.exists {
                  _.body.exists {
                    case VarDecl(`varName`, _, "const") =>
                      true
                    case _ =>
                      false
                  }
                }
              }


              object IsConstantInitializerInThis extends RecursiveExpressionCondition {
                def allow(c: Node.Node): Boolean = c match {
                  case IsConstant() =>
                    true
                  case Node.ThisExpression() Dot varName if isClassConstant(varName) =>
                    true
                  case _ =>
                    false
                }

                def forbid(c: Node.Node): Boolean = false
              }

              val (inlineVars, rest) = rest_?.partition {
                case SingleExpression(Assign((_: Node.ThisExpression) Dot member, "=", IsConstantInitializerInThis(expr))) =>
                  //println(s"Assign const $expr")
                  true
                case _ =>
                  false
              }

              val accessor = classInlineBody(cls, clsTokenDef)
              val accessorValue = accessor.value.asInstanceOf[Node.FunctionExpression]

              val inlineBodyScope = ScopeContext.getNodeId(accessorValue)
              val constructorScope = ScopeContext.getNodeId(md.value)

              //println(s"inlining $cls")
              //println(s"  inlined $inlined")
              //println(s"  rest $rest")
              // transform parameter names while inlining (we need to use parSuffix names)
              val parNames = params.map(parameterNameString)

              val parNamesSet = parNames.toSet

              ctx.withScope(accessorValue) {
                val parNamesAdjusted = (inlined ++ inlineVars).map { s =>
                  s.transformAfter(ctx) { (node, transformer) =>
                    node match {
                      case sym: Node.Identifier =>
                        val id = classSymIds(sym)
                        assert(id.name == sym.name)
                        if ((parNamesSet contains sym.name) && (id.sourcePos == inlineBodyScope || id.sourcePos == constructorScope)) {
                          classSymIds.rename(sym, sym.name, sym.name + parSuffix)
                          // remove the old hint if needed?
                          types = types addHint Some(SymId(sym.name  + parSuffix, inlineBodyScope)) -> IsConstructorParameter
                          Node.Identifier(sym.name + parSuffix).withTokens(sym) // prevent modifying original node
                        } else sym
                      // do not inline call, we need this.call form for the inference
                      // on the other hand form without this is better for variable initialization
                      case (_: Node.ThisExpression) Dot member if !transformer.parent().exists(_.isInstanceOf[Node.CallExpression]) =>
                        Node.Identifier(member).withTokens(node)
                      case _ =>
                        node
                    }
                  }
                }
                // add adjusted constructor argument names so that parser correctly resolves them inside of the function

                accessorValue.params = params.map(Transform.funArg).map { p =>
                  val a = p.cloneNode()
                  a.name = p.name + parSuffix
                  val aId = SymId(a.name, inlineBodyScope)
                  types = types addHint Some(aId) -> IsConstructorParameter
                  // marking source as cls so that they use the class scope, same as member variables
                  a
                }
                //println(s"inlineConstructors classInlineBody clone ${accessor.argnames}")


                // add the constructor call itself, so that type inference binds its parameters and arguments
                val constructorCall = if (rest.nonEmpty) Some(Node.ExpressionStatement {
                  Node.CallExpression(
                    Dot(
                      Node.ThisExpression().withTokens(constructorProperty),
                      Node.Identifier("constructor").withTokens(rest.head)
                    ).withTokens(constructorProperty),

                    params.map { p =>
                      Node.Identifier(parameterNameString(p) + parSuffix).withTokens(rest.head)
                    }
                  )
                }) else None

                accessorValue.body.body = accessorValue.body.body ++ parNamesAdjusted ++ constructorCall
              }

              if (rest.nonEmpty) {
                constructorProperty.asInstanceOf[Node.FunctionExpression].body.body = rest
              } else {
                cls.body.body = cls.body.body diff Seq(md)
              }
            }

            cls
          }
        case _ =>
          node
      }
    }

    n.copy(top = r, types = types)
  }

}
