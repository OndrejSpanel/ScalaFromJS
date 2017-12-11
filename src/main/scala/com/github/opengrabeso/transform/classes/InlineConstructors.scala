package com.github.opengrabeso
package transform
package classes

import net.gamatron.esprima._
import esprima._

import Classes._
import Expressions._
import Variables._
import Symbols._
import SymbolTypes._
import VariableUtils._
import JsUtils._
import Transform._

object InlineConstructors {
  private case class PrivateMember(sym: SymbolDef, isVal: Boolean)

  private object AssignToMember {
    def unapply(arg: Node.Node): Option[(String, Node.Node)] = arg match {
      case SingleStatement(Node.Assign(Node.This() Node.StaticMemberExpression funName, "=", value)) =>
        Some(funName, value)
      case _ =>
        None
    }
  }
  private object DefinePrivateFunction {
    def unapply(arg: Node.Statement): Option[(String, Node.Lambda)] = arg match {
      case AssignToMember(funName, lambda: Node.Lambda) =>
        Some(funName, lambda)
      case _ =>
        None
    }
  }

  private def detectPrivateFunctions(n: Node.Lambda) = {
    // detect exported functions
    var localLambdas = Set.empty[Node.Lambda]
    n.walk {
      case fun: Node.Lambda if fun != n =>
        localLambdas += fun
        true
      case _ =>
        false
    }

    //println(s"localLambdas $localLambdas")
    // detect functions which are not exported - i.e. local named function not assigned to anything

    def localLambdaByName(name: String): Option[Node.Lambda] = {
      localLambdas.find(_.name.nonNull.exists(_.name == name))
    }

    var isExported = Set.empty[Node.Lambda]
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


    val definedFunctions = localLambdas.filter(_.isInstanceOf[Node.Defun])
    //println(s"definedFunctions $definedFunctions")

    var definedAndExported = Set.empty[Node.Lambda]
    n.walk {
      case Node.Call(Node.Identifier(_), _*) =>
        true // a plain call, do not dive into
      case Node.Identifier(funName) if localLambdas.exists(_.name.nonNull.exists(_.name == funName)) =>
        definedAndExported ++= localLambdas.filter(_.name.nonNull.exists(_.name == funName))
        true
      case _ =>
        false
    }

    //println(s"definedAndExported $definedAndExported")

    // select all lambdas with exception of plain defined functions which are not exported
    val perhapsExported = localLambdas -- (definedFunctions -- definedAndExported)

    //println(s"isExported $isExported")
    //println(s"perhapsExported $perhapsExported")

    val exportedDirectly = isExported ++ perhapsExported

    //println(s"exportedDirectly $exportedDirectly")

    // detect calls from exported functions

    def transitiveClosure(functions: Set[Node.Lambda]): Set[Node.Lambda] = {
      var called = functions
      var someChange = false
      n.walkWithDescend { (node, _, walker) =>
        node match {
          // any use other than a definition should be enough to export the function as well
          case _: Node.Lambda =>
            false
          case Node.Identifier(sym) =>
            for {
              fun <- localLambdaByName(sym)
              inFunction <- walker.stack.reverse.collectFirst {
                case c: Node.Lambda =>
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
      if (someChange) transitiveClosure(called) else called
    }

    transitiveClosure(exportedDirectly)
  }

  private def detectPrivateMembers(n: Node.Lambda): Seq[PrivateMember] = {
    // any variable defined in the main body scope and references from any function is considered a private member
    //n.variables = Dictionary.empty[SymbolDef]
    val functions = detectPrivateFunctions(n)

    val refs = buildReferenceStacks(n)

    //println(s"n.variables ${n.variables.map(_._1)}")
    //println(s"n.functions ${n.functions.map(_._1)}")
    val variables = n.variables.filter(v => !n.functions.contains(v._1))
    val privates = for {
      (_, sym) <- variables
      // empty 'references' means automatic symbol, like "arguments"
      rs <- refs.refs.get(sym)
      if (rs -- Set(n)).intersect(functions.asInstanceOf[Set[Node.Scope]]).nonEmpty
    } yield {
      sym
    }

    privates.toSeq.map { priv =>

      //println(s"  priv ${priv.name} ${refs.refs.get(priv)} ")
      // check which members are ever written to - we can convert all others to getters and methods
      val modified = refs.isModified(priv)

      PrivateMember(priv, !modified)

    }
  }


  def privateVariables(n: Node.Node): Node.Node = {
    n.transformAfter { (node, _) =>
      node match {
        case cls: Node.DefClass =>
          for {
            constructorProperty@Node.ConciseMethod(_, rawConstructor: Node.Lambda) <- findConstructor(cls)
          } {
            val allLocals = detectPrivateMembers(rawConstructor)
            // convert private variables to members (TODO: mark them as private somehow)

            // some of them may be a constructor parameters, they need a different handling
            val constructorParameters = allLocals.filter(_.sym.orig exists rawConstructor.argnames.contains)

            //println(s"Locals ${allLocals.map(l => l.sym.name -> l.isVal)}")
            //println(s"Parameters ${constructorParameters.map(l => l.sym.name -> l.isVal)}")

            val locals = allLocals diff constructorParameters

            def newThisDotMember(member: String) = new Node.StaticMemberExpression {
              expression = Node.This()
              property = member
            }

            def privateMember(v: SymbolDef): Node.Node = newThisDotMember(v.name)

            val constructor = locals.foldLeft(rawConstructor) { (constructor, privateVar) =>
              val replacedInit = replaceVariableInit(constructor, privateVar.sym) { (sym, init) =>
                new Node.SimpleStatement {
                  body = new Node.Assign {
                    left = newThisDotMember(sym.name)
                    operator = "="
                    right = init.clone()
                  }
                  //println(s"Replaced init ${sym.name} $init")
                }
              }

              //println(s"Replaced private var ${privateVar.sym.name}")
              replaceVariable(replacedInit, privateVar.sym, privateMember(privateVar.sym))
            }
            constructorProperty.value = constructor

            // DRY: FillVarMembers
            val clsTokenDef = classTokenSource(cls)

            val vars = locals.map { local =>
              val varDecl = if (local.isVal) new Node.Const else new Node.Var
              fillTokens(varDecl, clsTokenDef)
              varDecl.definitions = js.Array(Node.VarDef.uninitializedSym(clsTokenDef)(local.sym))
              //println(s"privateVariables ${local.sym.name} $varDecl ${cls.start.get.pos}")
              varDecl
            }

            if (vars.nonEmpty) {
              val accessor = classInlineBody(cls, clsTokenDef)

              accessor.body ++= (vars: Iterable[Node.Statement]).toJSArray
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

    n.transformAfter { (node, _) =>
      node match {
        case cls: Node.DefClass =>
          for {
            constructorProperty@Node.ConciseMethod(_, rawConstructor: Node.Lambda) <- findConstructor(cls)
          } {
            val functionsToConvert = detectPrivateFunctions(rawConstructor)
            if (log) println(s"functionsToConvert ${functionsToConvert.toSeq.map(_.name)}")

            val functions = rawConstructor.body.collect {
              case s@DefinePrivateFunction(funName, f) if functionsToConvert contains f =>
                (s, funName, f)
              case s@Node.Defun(Defined(Node.SymbolName(funName)), _, _) if functionsToConvert contains s =>
                (s, funName, s)
            }

            val restRaw = rawConstructor.body diff functions.map(_._1)

            val rest = restRaw.filter {
              case Node.SimpleStatement(Node.Assign(Node.This() Node.StaticMemberExpression tgtName, "=", Node.Identifier(funName)))
                // TODO: relax tgtName == funName requirement
                if tgtName == funName && (functionsToConvert exists (_.name.nonNull.exists(_.name==funName))) =>
                //if (log) println(s"Drop assign $funName")
                false
              case _ =>
                true

            }

            constructorProperty.value._body = rest
            // add functions as methods
            cls.properties = cls.properties ++ functions.map {
              case (statement, funName, lambda) =>
                new Node.ConciseMethod {
                  fillTokens(this, node)
                  key = new Node.SymbolMethod {
                    /*_*/
                    fillTokens(this, node)
                    /*_*/
                    name = funName
                    // scope, thedef will be filled

                  }
                  value = new Node.Accessor {
                    fillTokens(this, node)
                    name = lambda.name
                    argnames = lambda.argnames
                    uses_arguments = lambda.uses_arguments
                    this.body = lambda.body
                  }
                  `static` = false
                  is_generator = false
                  quote = "\""
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
    val r = n.top.transformAfter { (node, _) =>
      node match {
        case cls: Node.DefClass =>
          val clsTokenDef = classTokenSource(cls)
          for {
            constructorProperty@Node.ConciseMethod(_, constructor: Node.Lambda) <- findConstructor(cls)
          } {
            // anything before a first variable declaration can be inlined, variables need to stay private
            val (inlined, rest_?) = constructor.body.span {
              case _: Node.Definitions => false
              case _ => true
            }

            def isClassConstant(varName: String): Boolean = {
              val body = findInlineBody(cls)
              body.exists {
                _.value.body.exists {
                  case Node.Const(Node.VarDef(Node.SymbolName(`varName`), _)) =>
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
                case Node.This() Node.StaticMemberExpression varName if isClassConstant(varName) =>
                  true
                case _ =>
                  false
              }
            }

            val (inlineVars, rest) = rest_?.partition {
              case SingleStatement(Node.Assign((_: Node.This) Node.StaticMemberExpression member, "=", IsConstantInitializerInThis(expr))) =>
                //println(s"Assign const $expr")
                true
              case _ =>
                false
            }
            //println(s"inlining $cls")
            //println(s"  inlined $inlined")
            //println(s"  rest $rest")
            // transform parameter names while inlining (we need to use parSuffix names)
            val parNames = constructor.argnames.map(Transform.funArg).map(_.name)
            val parNamesSet = parNames.toSet
            object IsParameter {
              def unapply(arg: String): Boolean = parNamesSet contains arg
            }
            val parNamesAdjusted = (inlined ++ inlineVars).map { s =>
              s.transformAfter { (node, transformer) =>
                node match {
                  case sym@Node.SymbolName(IsParameter()) =>
                    sym.name = sym.name + parSuffix
                    types = types addHint sym.thedef.nonNull.flatMap(id).map(_.copy(name = sym.name)) -> IsConstructorParameter
                    sym
                  // do not inline call, we need this.call form for the inference
                  // on the other hand form without this is better for variable initialization
                  case (_: Node.This) Node.StaticMemberExpression member if !transformer.parent().isInstanceOf[Node.Call] =>
                    Node.Identifier(clsTokenDef)(member)
                  case _ =>
                    node
                }
              }
            }
            // add adjusted constructor argument names so that parser correctly resolves them inside of the function
            val accessor = classInlineBody(cls, clsTokenDef)
            accessor.argnames = constructor.argnames.map(Transform.funArg).map { p =>
              val a = p.clone()
              a.name = p.name + parSuffix
              val classSymbolId = cls.name.nonNull.flatMap(_.thedef.nonNull).flatMap(id)
              types = types addHint classSymbolId.map(_.copy(name = a.name)) -> IsConstructorParameter
              // marking source as cls so that they use the class scope, same as member variables
              fillTokens(a, clsTokenDef)
              a
            }
            //println(s"inlineConstructors classInlineBody clone ${accessor.argnames}")


            // add the constructor call itself, so that type inference binds its parameters and arguments
            val constructorCall = if (rest.nonEmpty) Some(Node.SimpleStatement(constructorProperty) {
              new Node.Call {
                fillTokens(this, constructorProperty)
                expression = new Node.StaticMemberExpression {
                  fillTokens(this, constructorProperty)
                  expression = Node.This().withTokens(constructorProperty)
                  property = "constructor"
                }

                args = constructor.argnames.map { p =>
                  Node.Identifier(p)(p.name + parSuffix)
                }
              }
            }) else None

            accessor.body = accessor.body ++ parNamesAdjusted ++ constructorCall

            if (rest.nonEmpty) {
              constructor.body = rest
            } else {
              cls.properties = cls.properties -- Seq(constructorProperty)
            }
          }

          cls
        case _ =>
          node
      }
    }

    n.copy(top = r, types = types)
  }

}
