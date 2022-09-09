package com.github.opengrabeso.scalafromjs
package transform

import com.github.opengrabeso.scalafromjs.esprima._
import com.github.opengrabeso.esprima._
import Classes._
import Transform._
import SymbolTypes._
import com.github.opengrabeso.scalafromjs.ScalaOut.SymbolDef
import com.github.opengrabeso.scalafromjs.esprima.symbols.{Id, ScopeContext, SymId}

import scala.language.implicitConversions
import scala.collection.Seq

object InferTypes {

  def scanFunctionReturns(funNode: Node.BlockStatement)(implicit ctx: ExpressionTypeContext, context: ScopeContext): Option[TypeInfo] = {
    import ctx.classOps

    var allReturns = Option.empty[TypeInfo]
    if (funNode != null) funNode.walkWithScope(context) { (node, context) =>
      implicit val scopeCtx: ScopeContext = context
      node match {
        // include any sub-scopes, but not local functions
        case innerFunc: Node.FunctionDeclaration if innerFunc.body != funNode =>
          true
        case innerFunc: Node.FunctionExpression if innerFunc.body != funNode =>
          true
        case objProperty: Node.Property =>
          // do not walk into any object properties
          true
        case Node.ReturnStatement(Defined(value)) =>
          //println(s"  return expression ${nodeClassName(value)}")
          val tpe = expressionType(value)
          //println(s"  Return type $tpe: expr ${ScalaOut.outputNode(value)}")
          if (tpe.isDefined) { // unknown types introduce Any return value, which we never get rid of
            allReturns = typeUnionOption(allReturns, tpe)
          }
          false
        case _ =>
          false
      }
    }

    allReturns.orElse {
      //println("Infer no return function")
      Option(funNode).flatMap {
        _.body match {
          case Seq(Node.ExpressionStatement(ex)) => expressionType(ex)
          case _ => None
        }
      }
    }
  }

  def inferTypes(n: NodeExtended): NodeExtended = {

    // TODO: cleanup inferred, was used for members, not for variables, now it is not used at all
    val allTypes = Ref(n.types) // keep immutable reference to a mutating var

    val classes = ClassListHarmony.fromAST(n.top)
    //println("Classes:\n" + classes.keys)


    val classInfo = listClassMembers(n)
    //println("ClassInfo:\n" + classInfo)

    implicit val ctx = ExpressionTypeContext(allTypes, classInfo, classes) // note: ctx.allTypes is mutable
    import ctx.classPos
    import ctx.classOps

    def typeFromOperation(op: String, n: Node.Node)(implicit context: ScopeContext) = {
      op match {
        case IsComparison() =>
          // comparison - most like the type we are comparing to
          expressionType(n)
        case IsArithmetic() =>
          // arithmetics - must be a number,
          // hint: most likely the same type as we are operating with
          Some(TypeInfo.both(number))
        case _ =>
          None
      }
    }

    type TypeInferenceKind = (Option[TypeInfo], Option[TypeInfo]) => Option[TypeInfo]

    def target(t1: Option[TypeInfo], t2: Option[TypeInfo]): Option[TypeInfo] = {
      typeUnionOption(t1, t2)
    }
    def source(t1: Option[TypeInfo], t2: Option[TypeInfo]): Option[TypeInfo] = {
      typeIntersectOption(t1, t2)
    }

    def addInferredType(tid: Option[SymbolMapId], tpeSrc: Option[TypeInfo], kind: TypeInferenceKind = target)(debug: () =>String*) = {
      val tpe = tpeSrc.map(_.copy(certain = false)) // inferred type is never considered certain
      if (tpe.exists(_.known)) {
        def noType = Seq("undefined", "null", "this", "super") // never infer anything about those identifiers

        if (tid.exists(t => !(noType contains t.name))) {
          val oldType = allTypes.get(tid)
          val symType = kind(oldType, tpe)
          //println(s"  Combined $symType = ${allTypes.get(tid)} * $tpe")
          for (tp <- symType) {

            if (tp.nonEmpty && tp.acceptable && !oldType.exists(tp.equivalent)) {
              //println(s"  Add type $tid: $tp")

              if (
                !allTypes.t.get(tid).exists(_.certain) && // when a type is certain, do not overrwrite it
                (!allTypes.t.locked || allTypes.t.get(tid).forall(tp.isSafeReplacementOf)) // when locked, allow only unknown type replacement
              ) {
                if (tid.exists(watchedSym)) {
                  println(s"Watched ${tid.get} result $tp, type ${tpe.get}, was $oldType")
                  debug.foreach(s => println("  " + s()))
                }
                allTypes.t += tid -> tp
              } else {
                if (tid.exists(watchedSym)) {
                  println(s"Watched (supressed) ${tid.get} result $tp, type ${tpe.get}, was $oldType")
                  debug.foreach(s => println("  " + s()))
                }
              }
            } else if (true) {
              if (tid.exists(watchedSym)) {
                println(s"Watched not acceptable ${tid.get} type $oldType === $tp from ${tpe.get}")
                debug.foreach(s => println("  " + s()))
              }
            }
            //println(s"All types ${allTypes.t.types}")
            //println(s"inferred ${inferred.types}")
          }
        }
      }
    }

    def addInferredMemberType(idAccess: Option[MemberId], tpeSrc: Option[TypeInfo], kind: TypeInferenceKind = target)(debug: (()=>String)*) = {
      val tpe = tpeSrc.map(_.copy(certain = false)) // inferred type is never considered certain
      if (tpe.exists(_.known)) {

        // search parents, we may be accessing an inherited member

        val id = idAccess.flatMap { i =>
          classInfo.classContains(i.cls, i.name).map { containedIn =>
            i.copy(cls = containedIn)
          }
        }.orElse(idAccess)

        //println(s"Member type was $id: ${inferred.getMember(id)}")

        val oldType = allTypes.getMember(id)
        val symType = kind(oldType, tpe)
        //println(s"  Adding member type $id: $tpe -> $symType")

        /*
        if (idAccess.contains(MemberId("_Math", "generateUUID"))) {
          println(s"member $idAccess type $tpe")
        }
        if (idAccess.exists(_.name == "uuid")) {
          println(s"member $idAccess type $tpe")
        }
        if (idAccess.contains(MemberId("Vector3", "x")) && tpe.exists(_.declType != number)) {
          println("Suspicious member Vector3.x type $tpe")
        } else if (idAccess.exists(_.name == "x") && tpe.exists(_.declType != number)) {
          println(s"Suspicious member $idAccess type $tpe")
        } else if (idAccess.exists(_.name == "_x") && tpe.exists(_.declType != number)) {
          println(s"Suspicious member $idAccess type $tpe")
        }
        */

        for (tp <- symType) {
          //println(s"  Add member type $idAccess - $id: $tp")
          //println("  " + classInfo)
          // TODO: DRY with addInferredType
          if (tp.nonEmpty && tp.acceptable && !oldType.exists(tp.equivalent)) {

            if (
              !allTypes.t.getMember(id).exists(_.certain) && // when a type is certain, do not overrwrite it
              (!allTypes.t.locked || allTypes.t.getMember(id).forall(tp.isSafeReplacementOf)) // when locked, allow only unknown type replacement
            ) {
              if (id.exists(_.isWatched)) {
                println(s"Watched member ${id.get} ${allTypes.t.symbolFromMember(id.get)} result $tp, type ${tpe.get}, was $oldType")
                debug.foreach(s => println("  " + s()))
              }
              allTypes.t = allTypes addMember id -> tp
            } else {
              if (id.exists(_.isWatched)) {
                println(s"Watched (supressed) member ${id.get} result $tp, type ${tpe.get}, was $oldType")
                debug.foreach(s => println("  " + s()))
              }
            }
          } else if (true) {
            if (!oldType.exists(tp.equivalent)) {
              if (id.exists(_.isWatched)) {
                println(s"Watched member not-acceptable ${id.get} type $oldType === $tp from ${tpe.get}")
                debug.foreach(s => println("  " + s()))
              }
            }
          }
        }
      }
    }

    // list all functions so that we can look-up them from call sites
    var functions = Map.empty[SymId, DefFun]
    n.top.walkWithScope {(node, scope) =>
      implicit val scopeCtx = scope
      node match {
        case defun@DefFun(Defined(Id(name)), _, _, _, _) =>
          functions += name -> defun
          false
        case _ =>
          false
      }
    }

    //println(functions.map(f => f._1.name))

    def inferArgsFromPars(pars: Seq[(Option[SymbolMapId], Option[TypeInfo])], args: Seq[Node.Node])(debug: (()=>String)*)(implicit context: ScopeContext) = {
      for (((parId, Some(par)), arg) <- pars zip args) {
        arg match {
          case SymbolInfo(a) =>
            //println(s"Infer arg $a as $par")
            a.addSymbolInferredType(Some(par), source)((()=>s"inferArgsFromPars $parId: $par $arg") +: debug:_*)
          case _ =>
        }
      }
    }

    def inferParsOrArgs(fun: Node.Node, pars: Seq[Node.FunctionParameter], args: Seq[Node.Node])(debug: (()=>String)*)(implicit context: ScopeContext) = {

      //println(s"inferParsOrArgs $pars")
      val scopeId = ScopeContext.getNodeId(fun) // pars need to be evaluated in the scope of the function
      val parIds = pars.flatMap(nameFromPar).map(SymId(_, scopeId))
      //println(s"  parIds $parIds")

      for ((par, arg) <- parIds zip args) {
        val tp = expressionType(arg)
        if (tp.exists(_.nonEmpty)) {
          //println(s"Infer par $par as $tp")
          addInferredType(Some(par), tp)((()=>s"inferParsOrArgs $par $arg") +: debug:_*)
        }
      }

      inferArgsFromPars(parIds.map(p => Some(p) -> allTypes.get(p)), args)(debug:_*)
    }

    def inferArgs(funType: FunctionType, args: Seq[Node.Node])(debug: (()=>String)*)(implicit context: ScopeContext) = {
      val argTypes = funType.args.map(par => None -> Some(TypeInfo.target(par)))
      //println(s"Infer args for $funType, $args, $argTypes")
      inferArgsFromPars(argTypes, args)(debug:_*)
    }


    def inferFunction(pars: Seq[Node.Node], log: Boolean)(implicit context: ScopeContext) = {
      val parTypes = pars.map(expressionType(_, log))
      //println(s"  $pars $parTypes")
      FunctionType(AnyType, parTypes.map(_.fold[TypeDesc](NoType)(_.target)).toIndexedSeq)
    }

    trait SymbolAccessInfo {
      // workaround for https://issues.scala-lang.org/browse/SI-5252
      def addSymbolInferredType(tpe: Option[TypeInfo], kind: TypeInferenceKind)(debug: (()=>String)*): Unit
      def tpe(types: SymbolTypes): Option[TypeInfo]
      def unknownType(types: SymbolTypes): Boolean = tpe(types).isEmpty
    }

    class SymbolAccessSymbol(symbol: SymbolDef) extends SymbolAccessInfo {
      override def toString = s"Symbol($symbol)"

      def addSymbolInferredType(tpe: Option[TypeInfo], kind: TypeInferenceKind)(debug: (()=>String)*): Unit = {
        //println(s"SymbolAccessSymbol: addSymbolInferredType $this $tpe")
        addInferredType(symbol, tpe, kind)(debug:_*)
      }

      def tpe(types: SymbolTypes) = {
        //println(s"Check $s => ${n.types.get(s)}")
        n.types.get(symbol)
      }

    }


    class SymbolAccessDot(symbol: MemberId) extends SymbolAccessInfo {
      override def toString = s"Member(${symbol.cls}.${symbol.name})"

      def addSymbolInferredType(tpe: Option[TypeInfo], kind: TypeInferenceKind)(debug: (()=>String)*): Unit = {
        //println(s"SymbolAccessDot: addSymbolInferredType $this $tpe")
        addInferredMemberType(Some(symbol), tpe, kind)((()=>s"Member $symbol") +: debug:_*)
      }

      def tpe(types: SymbolTypes) =  {
        val p = findInParents(symbol.cls, symbol.name)(ctx)
        //println(s"Check $d => $p = ${n.types.getMember(dot)}")
        types.getMember(p.map(pp => symbol.copy(cls = pp)))
      }
    }

    class SymbolAccessArray(symbol: SymbolDef) extends SymbolAccessInfo {
      override def toString = s"Array($symbol)"

      def addSymbolInferredType(tpe: Option[TypeInfo], kind: TypeInferenceKind)(debug: (()=>String)*): Unit = {
        //println(s"SymbolAccessArray: addSymbolInferredType $this $tpe")

        val mappedTpe = tpe.map(_.map(ArrayType))
        //println(s"${symbol.name}: Array type $tpe $mappedTpe")
        addInferredType(symbol, mappedTpe, kind)((()=>"Array") +: debug:_*)
      }

      def tpe(types: SymbolTypes) =  {
        val t = types.get(symbol)
        t.map(_.map(ArrayType))
      }
    }

    class SymbolAccessMap(symbol: SymbolDef) extends SymbolAccessInfo {
      override def toString = s"Map($symbol)"
      def addSymbolInferredType(tpe: Option[TypeInfo], kind: TypeInferenceKind)(debug: (()=>String)*): Unit = {
        //println(s"SymbolAccessMap: addSymbolInferredType $this $tpe")

        val mappedTpe = tpe.map(_.map(MapType))
        //println(s"${symbol.name}: Map type $tpe $mappedTpe")
        addInferredType(symbol, mappedTpe, kind)((()=>"Map") +: debug:_*)
      }

      def tpe(types: SymbolTypes) =  {
        val t = types.get(symbol)
        t.map(_.map(MapType))
      }
    }

    class SymbolAccessDotMap(symbol: MemberId) extends SymbolAccessInfo {
      override def toString = s"DotMap($symbol)"

      def addSymbolInferredType(tpe: Option[TypeInfo], kind: TypeInferenceKind)(debug: (()=>String)*): Unit = {
        val mappedTpe = tpe.map(_.map(MapType))
        addInferredMemberType(Some(symbol), mappedTpe, kind)((()=>s"Dot $symbol") +: debug:_*)
      }

      def tpe(types: SymbolTypes) =  { // TODO: verify
        val p = findInParents(symbol.cls, symbol.name)(ctx)
        //println(s"Check $d => $p = ${n.types.getMember(dot)}")
        types.getMember(p.map(pp => symbol.copy(cls = pp)))
      }
    }

    class SymbolAccessDotArray(symbol: MemberId) extends SymbolAccessInfo {
      override def toString = s"DotArray($symbol)"
      def addSymbolInferredType(tpe: Option[TypeInfo], kind: TypeInferenceKind)(debug: (()=>String)*): Unit = {
        //println(s"SymbolAccessDotArray: addSymbolInferredType $symbol $tpe")
        val mappedTpe = tpe.map(_.map(ArrayType))
        addInferredMemberType(Some(symbol), mappedTpe, kind)(debug:_*)
      }

      def tpe(types: SymbolTypes) =  { // TODO: verify
        val p = findInParents(symbol.cls, symbol.name)(ctx)
        //println(s"Check $d => $p = ${n.types.getMember(dot)}")
        types.getMember(p.map(pp => symbol.copy(cls = pp)))
      }
    }

    object SymbolInfo {
      def unapply(arg: Node.Node)(implicit context: ScopeContext): Option[SymbolAccessInfo] = arg match {
        case Node.Identifier(Id(symDef)) =>
          Some(new SymbolAccessSymbol(symDef))

        case Node.Identifier(Id(symDef)) Sub property =>
          //println(s"Node.Sub - ${symDef.name}")
          expressionType(property).flatMap {
            _.declType match {
              case `number` =>
                Some(new SymbolAccessArray(symDef)) // TODO: derive property is most likely Int, not Double
              case `string` =>
                //println(s"${symDef.name} - map")
                Some(new SymbolAccessMap(symDef))
              case _ =>
                None
            }
          }

        case expr Dot name Sub property =>
          //println(s"Dot Node.Sub - $name $property")
          // consider DRY with the case above
          expressionType(property).flatMap {
            _.declType match {
              case `number` =>
                val clsId = memberId(classFromType(expressionType(expr)), name)
                //println(s"$clsId - dot - array")
                clsId.map(new SymbolAccessDotArray(_))
              case `string` =>
                val clsId = memberId(classFromType(expressionType(expr)), name)
                //println(s"$clsId - dot - map")
                clsId.map(new SymbolAccessDotMap(_))
              case _ =>
                None
            }
          }



        case expr Dot name =>
          // dot access - most likely an object
          val exprType = expressionType(expr)
          val clsId = memberId(classFromType(exprType), name)
          clsId.map(new SymbolAccessDot(_))

        case _ =>
          None
      }
    }

    def inferConstructorCall(args: Seq[Node.Node], className: SymbolMapId)(implicit context: ScopeContext) = {
      //println(s"Infer arg types for class $className")
      for (c <- classes.get(className)) {
        for (value <- getMethodMethod(classInlineBody(c, transform.classes.classTokenSource(c)))) {
          //println(s"  Constructor inline pars ${value.argnames.map(_.name)} args ${args.map(ScalaOut.outputNode(_))}")
          inferParsOrArgs(value, value.params, args)(()=>s"Constructor call body $className")
        }

        for {
          md <- findConstructor(c)
          value <- getMethodMethod(md)
        } {
          //println(s"  Constructor pars ${value.argnames.map(_.name)} args ${args.map(ScalaOut.outputNode(_))}")
          inferParsOrArgs(value, value.params, args)(()=>s"Constructor call $className")
        }
      }
    }

    n.top.walkWithScopeAfter { (node, walker) =>
      //println(s"${nodeClassName(node)} ${node.toLocaleString()}")
      //descend(node, walker)
      implicit val scopeCtx = walker

      def handleAssignment(left: Node.ExpressionOrPattern, right: Node.Expression) = {
        val log = left match {
          case Node.Identifier(Id(symDef)) if watched(symDef.name) => true
          case _ => false
        }
        val leftT = expressionType(left, log)
        val rightT = expressionType(right, log)
        //if (log) println(s"Infer assign $leftT - $rightT ${ScalaOut.outputNode(node)}")
        if (leftT != rightT) { // equal: nothing to infer (may be both None, or both same type)
          for (SymbolInfo(symInfo) <- Some(left)) {
            if (log) println(s"Infer assign: $symInfo = $rightT")
            def rightInfo = {
              right match {
                case Node.Identifier(Id(symDef)) =>
                  s"sym $symDef"
                case _ =>
                  right.toString
              }
            }
            symInfo.addSymbolInferredType(rightT, target)(()=>s"  Infer assign: $symInfo = $rightT right $rightInfo")
          }
          for (SymbolInfo(symInfo) <- Some(right)) {
            if (log) println(s"Infer reverse assign: $leftT = $symInfo")
            symInfo.addSymbolInferredType(leftT, source)(()=>s"  Infer reverse assign: $leftT = $symInfo left $left")
          }
        }
      }

      node match {
        case Node.VariableDeclarator(Node.Identifier(Id(symDef)), Defined(right), _) =>
          val log = watched(symDef.name)
          val symId = id(symDef)
          val leftT = n.types.get(symId)
          val rightT = expressionType(right, log)


          if (log) {
            println(s"Infer var $symId $leftT - $rightT `$node` $right")
          }
          if (leftT != rightT) { // equal: nothing to infer (may be both None, or both same type)
            for (symInfo <- Some(new SymbolAccessSymbol(symDef))) {
              if (log) println(s"  Infer var: $symInfo = $rightT")
              symInfo.addSymbolInferredType(rightT, target)(()=>s"Infer var $symInfo = $rightT")
              if (log) println(s"  as: ${allTypes.get(symDef)}")
            }
            for (SymbolInfo(symInfo) <- Some(right)) {
              if (log) println(s"  Infer reverse var: $leftT = $symInfo")
              symInfo.addSymbolInferredType(leftT, source)(()=>s"  Infer reverse var: $leftT = $symInfo")
            }
          }

        /*
        case Node.FunctionParameter(Defined(symDef), _, Defined(JsArray(init))) =>
          if (n.types.get(symDef).isEmpty) {
            val tpe = expressionType(init)(ctx)
            addInferredType(symDef, tpe)(s"Node.FunctionParameter ${symDef.name}")
          }
        */

        case Node.TypeAliasDeclaration(Node.Identifier(Id(symDef)), tpe) =>
          addInferredType(symDef, TypesRule.typeInfoFromAST(tpe)(scopeCtx))()

        // a few special forms of assignment should infer no type - cyclic dependencies
        case Assign(SymbolInfo(symLeft), _, SymbolInfo(symRight)) if symLeft == symRight =>

        case Assign(left, _, right) =>
          handleAssignment(left, right)

        case Binary(SymbolInfo(symLeft), IsArithmetic(), SymbolInfo(symRight))
          if symLeft.unknownType(n.types) && symRight.unknownType(n.types) =>
          //println(s"Infer arithmetic: both unknown $symLeft $symRight")
          val numType = Some(TypeInfo.both(number))
          symLeft.addSymbolInferredType(numType, target)(()=>s"Infer arithmetic: both unknown $symLeft $symRight")
          symRight.addSymbolInferredType(numType, target)(()=>s"Infer arithmetic: both unknown $symLeft $symRight")

        case Binary(_, "as", _) => // does not tell anthing about any side

        case Binary(SymbolInfo(symInfo), op, expr) if symInfo.unknownType(n.types) =>
          val tpe = typeFromOperation(op, expr)
          //println(s"Infer binary: left unknown $symInfo $tpe")
          symInfo.addSymbolInferredType(tpe, target)(()=>s"Infer binary: left unknown $symInfo $tpe")

        case Binary(expr, op, SymbolInfo(symInfo)) if symInfo.unknownType(n.types) =>
          val tpe = typeFromOperation(op, expr)
          //println(s"Infer binary: right unknown $symInfo $tpe")
          symInfo.addSymbolInferredType(tpe, target)(()=>s"Infer binary: right unknown $symInfo $tpe")

        case Node.SwitchStatement(SymbolInfo(symInfo), body) =>
          var allCases = Option.empty[TypeInfo]
          body.foreach {
            case cc: Node.SwitchCase =>
              val cType = expressionType(cc.test)
              allCases = typeUnionOption(allCases, cType)
            case _ =>
          }
          symInfo.addSymbolInferredType(allCases, target)(()=>s"Infer switch")

        case fun@DefFun(Defined(Id(symDef)), _, _, _, _) =>
          val allReturns = scanFunctionReturns(fun.body)
          //println(s"${symDef.name} returns $allReturns")
          for {
            retType <- allReturns
            sd <- symDef
          } {
            // parameters do not matter here, they are infered as separate symbols
            val funType = FunctionType(retType.declType, IndexedSeq())
            addInferredType(sd, Some(TypeInfo.target(funType)))(()=>s"Infer fun ${symDef.name}")
          }

        case Node.MethodDefinition(Node.Identifier(sym), _, _, fun@AnyFun(Seq(), body), "get", _) =>
          scopeCtx.withScope(fun, body) {
            // check body final value
            //noinspection MatchToPartialFunction
            walkLastNode(body) { last =>
              last match {
                case Node.ReturnStatement(right@(Node.ThisExpression() Dot member)) =>
                  //println(s"Getter $sym from $member")
                  // pretend we are assigning the returned value to this.sym
                  val left = Dot(Node.ThisExpression(), Node.Identifier(sym))
                  handleAssignment(left, right)

                case _ =>

              }
              false
            }
          }

        case Node.MethodDefinition(Node.Identifier(sym), _, _, fun@AnyFun(Seq(Node.Identifier(param)), body), "set", _) =>
          // search for any member in form this.xx = param
          scopeCtx.withScope(fun, body) {
            val paramId = Id(param)
            body.walkWithScope(scopeCtx) { (node, context) =>
              implicit val scopeCtx = context
              node match {
                case Assign(left@(Node.ThisExpression() Dot member), "=", Node.Identifier(Id(`paramId`))) =>
                  //println(s"Setter detected: $member = $param")
                  val right = Dot(Node.ThisExpression(), Node.Identifier(sym))
                  handleAssignment(left, right)
                  true
                case _ =>
                  false

              }

            }
          }

        case Node.MethodDefinition(Node.Identifier(sym), _, _, fun: Node.FunctionExpression, kind, _) =>
          val allReturns = scopeCtx.withScope(fun, fun.body) {
            scanFunctionReturns(fun.body)
          }

          // method of which class is this?
          val scope = findThisClass(scopeCtx)
          for {
            retType <- allReturns
            Node.ClassDeclaration(Defined(Node.Identifier(cls)), _, _, _, _) <- scope
            clsId = Id(cls)
          } {
            //println(s"Infer return type for method ${cls.name}.$sym as $retType")
            val classId = MemberId(clsId, sym)
            val funType = FunctionType(retType.declType, IndexedSeq())
            addInferredMemberType(Some(classId), Some(TypeInfo.target(funType)))(()=>s"Infer return type for method $cls.$sym as $retType")
          }

        case Node.MethodDefinition(Node.Identifier(sym), _, _, expr: Node.Expression, kind, _) =>
          val scope = findThisClass(scopeCtx)
          for {
            retType <- expressionType(expr)
            Node.ClassDeclaration(Defined(Node.Identifier(cls)), _, _, _, _) <- scope
            clsId = Id(cls)
          } {
            val classId = MemberId(clsId, sym)
            if (kind == "value") { // not a real function, on a value member
              //println(s"Infer return type for method ${cls.name}.$sym as $retType")
              addInferredMemberType(Some(classId), Some(retType))(()=>s"Infer return type for member value $cls.$sym as $retType")
            } else {
              val funType = FunctionType(retType.declType, IndexedSeq())
              addInferredMemberType(Some(classId), Some(TypeInfo.target(funType)))(()=>s"Infer return type for method $cls.$sym as $retType")
            }
          }


        case Node.Property("get", Node.Identifier(sym), _, fun: Node.FunctionExpression, _, _) =>
          val allReturns = scanFunctionReturns(fun.body)
          // method of which class is this?
          val scope = findThisClass(scopeCtx)
          //println(s"Infer getter $sym as $allReturns")
          for {
            retType <- allReturns
            Node.ClassDeclaration(Defined(Node.Identifier(cls)), _, _, _, _) <- scope
            clsId = Id(cls)
          } {
            //println(s"Infer return type for getter ${cls.name}.$sym as $retType")
            val classId = MemberId(clsId, sym)
            addInferredMemberType(Some(classId), Some(retType))(()=>s"Infer return type for getter $cls.$sym as $retType")
          }
        case Node.Property("set", Node.Identifier(sym), _, fun: Node.FunctionExpression, _, _) =>
          // method of which class is this?
          val scope = findThisClass(scopeCtx)
          //println(s"Infer setter $sym")

          for {
            arg <- fun.params.headOption
            retType <- allTypes.get(symbolFromPar(arg).flatMap(id))
            Node.ClassDeclaration(Defined(Node.Identifier(cls)), _, _, _, _) <- scope
            clsId <- Id(cls)
          } {
            //println(s"Infer return type for setter $cls.$sym as $retType")
            val classId = MemberId(clsId, sym)
            // target, because setter parameter is typically used as a source for the property variable, which sets source only
            addInferredMemberType(Some(classId), Some(TypeInfo.target(retType.declType)))(()=>s"Infer return type for setter $cls.$sym as $retType")
          }

        case ObjectKeyVal(name, value) => // cannot be a class member, only part of an object pattern
          for (inObject <- scopeCtx.findTypedParent[Node.ObjectExpression]) {
            val nodeId = ScopeContext.getNodeId(inObject)
            val symId = Some(SymbolMapId(name, nodeId))
            // TODO: value may be a function expression
            value match {
              case AnyFun(pars, body: Node.BlockStatement) =>
                scopeCtx.withScope(value, body) {
                  for (retType <- scanFunctionReturns(body)) {
                    // parameters do not matter here, they are infered as separate symbols
                    val funType = FunctionType(retType.declType, IndexedSeq())
                    addInferredType(symId, Some(TypeInfo.target(funType)))(() => s"Infer property fun $name")
                  }
                }

                // ??? // TODO
              case _ =>
                val tpe = expressionType(value)
                // we avoid using addInferredMemberType, as we already know the proper scoped id
                addInferredType(symId, tpe)(() => s"Infer type for value $inObject.$name as $tpe")
            }
          }

        case Node.NewExpression(Node.Identifier(Id(clsId)), args) =>
          val cls = classes.get(clsId)
          if (cls.isDefined) {
            inferConstructorCall(args, clsId)
          }


        case Node.CallExpression(Node.Identifier(Id(call)), args) =>
          //println(s"Call ${call.name}")
          functions.get(call) match {
            case Some(fun@DefFun(_, pars, _, _, _)) =>
              // now match arguments to parameters
              inferParsOrArgs(fun, pars, args)(()=>s"Call function $call")
            case _ =>
              val cls = classes.get(call)
              if (cls.isDefined) {
                inferConstructorCall(args, call)
              } else {
                //println(s"Infer arg types for a var call ${varSym.name} as $tpe")
                val log = watched(call.name)
                val tpe = inferFunction(args, log)
                addInferredType(call, Some(TypeInfo.target(tpe)))(()=>s"Infer ${call.name} args $args")
              }
          }
        case Node.CallExpression(s: Node.Super, args) =>
          for (sup <- findSuperClass(scopeCtx)) {
            //println(s"Super call of $sup")
            inferConstructorCall(args, sup)
          }

        case Node.CallExpression(expr Dot call, args) =>
          val exprType = expressionType(expr)

          (exprType.map(_.declType),call,expr) match {
            case (Some(ArrayType(arrayElemType)), "push", SymbolInfo(sym)) =>
              if (args.nonEmpty) {
                val elemType = args.map(expressionType(_)).reduce(typeUnionOption)
                sym.addSymbolInferredType(elemType.map(_.map(ArrayType)), target)(()=>s"Array.push $sym elem: $elemType array: $arrayElemType")
              }
            case (Some(AnonymousClassType(sourcePos)), _, _) =>
              val log = call.isWatched
              val symId = SymbolMapId(call, sourcePos)
              val tpe = inferFunction(args, log)
              addInferredType(Some(symId), Some(TypeInfo.target(tpe)))(()=>s"Infer par types for an anonymous member call $call as $tpe")
              // TODO: inferArgs for the function

              // TODO: use function types only for member functions
              // fill class symbols (if they exists)
              for {
                c <- classes.getAnonymous(SymbolMapId("<anonymous>", sourcePos))
                m@AnyFun(pars, _) <- findObjectMethod(c, call)
              } {
                inferParsOrArgs(m, pars, args)(()=>s"Method call anonymous: $expr.$call offset ${expr.start}")
              }


            case (Some(ClassType(callOn)), _, _) =>
              //println(s"Dot call $call")
              // fill ctx.type function types information
              for {
                c <- getParents(callOn)(ctx) // infer for all overrides
              } {
                val memberId = MemberId(c, call)
                //println(s"memberId $memberId, args ${args.mkString(",")}")
                if (ctx.classInfo.containsMember(c, call)) {
                  val log = memberId.isWatched
                  val tpe = inferFunction(args, log)

                  //if (log) println(s"Infer par types for a member call $c.$call as $tpe")
                  //println(allTypes)
                  addInferredMemberType(Some(memberId), Some(TypeInfo.target(tpe)))(()=>s"Infer par types for a member call $c.$call as $tpe") // target or source?

                  for (funType <- ctx.types.getMember(Some(memberId))) {
                    funType.declType match {
                      case ft: FunctionType =>
                        inferArgs(ft, args)(()=>s"Function member $memberId")
                      case _ =>

                    }
                  }
                }
              }

              // TODO: use function types only for member functions
              // fill class symbols (if they exists)
              for {
                c <- getParents(callOn)(ctx) // infer for all overrides
                parentC <- classes.get(c)
                m <- findMethod(parentC, call)
                AnyFun(pars, _) <- Some(m.value)
              } {
                inferParsOrArgs(m.value, pars, args)(()=>s"Method call $callOn: $expr.$call offset ${expr.start}")
              }
            case _ =>
          }

        case SymbolInfo(symbol) Sub property =>
          val tpe = symbol.tpe(ctx.types)
          //println(s"$symbol Node.Sub $property `$tpe` -> `${tpe.map(_.declType)}`")

          tpe.map(_.declType) match {
            case Some(ObjectOrMap) =>
              // initialized as {}, cannot be an Array, must be a map

              symbol.addSymbolInferredType(Some(TypeInfo.target(MapType(NoType))), target)(()=>s"ObjectOrMap $symbol")
            case Some(CanBeMap(_)) =>
              // addressing map, we know index must be a string
              for (SymbolInfo(symbol) <- Some(property)) {
                val indexType = Some(TypeInfo.target(string))
                symbol.addSymbolInferredType(indexType, target)(()=>s"Map $symbol.$property")
              }
            case Some(CanBeArray(_)) =>
              // addressing array, we know index must be a number
              for (SymbolInfo(symbol) <- Some(property)) {
                val indexType = Some(TypeInfo.target(number))
                symbol.addSymbolInferredType(indexType, target)(()=>s"Array $symbol[$property]")
              }
              // once determined, do not change
            case _ =>
              expressionType(property).map(_.declType) match {
                case Some(`number`) =>
                  symbol.addSymbolInferredType(Some(TypeInfo.target(ArrayType(NoType))), target)(()=>s"Array index $symbol[$property]")
                case Some(`string`) =>
                  symbol.addSymbolInferredType(Some(TypeInfo.target(MapType(NoType))), target)(()=>s"Map index $symbol[$property]")
                case _ =>
              }
          }


        case _ =>
      }
      true
    }

    // TODO: protect JSDoc explicit types
    //println(s"inferred ${inferred.types}")

    val ret = n.copy(types = allTypes)
    //println(s"** n.types ${ret.types.types.filter(_._1.sourcePos>=0)}")
    ret
  }

  def multipass(n: NodeExtended): NodeExtended = {
    val maxDepth = 15
    val byMembersAfter = 3
    val log = false

    def inferTypesOneStep(n: NodeExtended, depth: Int, doByMembers: Boolean, desperate: Boolean): NodeExtended = {

      val now = System.currentTimeMillis()
      val r = if (doByMembers) ClassesByMembers(n, desperate) else inferTypes(n)
      val again = System.currentTimeMillis()
      def condString(cond: Boolean, s: String) = if (cond) s else ""
      if (log) println(s"Infer types ${condString(doByMembers, "by members ")}${condString(desperate, "desperate ")}step $depth, metrics: ${r.types.knownItems}: ${again - now} ms")
      if (r.types.knownItems > n.types.knownItems) {
        r
      } else {
        if (log) println("  inference dropped")
        n
      }
    }


    def inferTypesStep(n: NodeExtended, depth: Int, byMembers: Int, desperateDone: Boolean): NodeExtended = {
      //if (log) println(s"Type inference: ${n.types} steps $maxDepth")
      val r = inferTypesOneStep(n, depth, byMembers == 0, false)

      if (depth >= maxDepth || r == n) { // normal inference exhausted, perform final desperate by members
        if (!desperateDone) {
          val byNamesDesperate = inferTypesOneStep(r, depth, true, true)

          val lockedSymbols = byNamesDesperate.copy(types = byNamesDesperate.types.copy(locked = true))
          // start again, this time in "desperate" mode, never repeat by members again
          inferTypesStep(lockedSymbols, 0, -1, true)
        }  else {
          r
        }

        // start new inference pass, this time a "desperate" one
      } else {
        inferTypesStep(r, depth + 1, byMembers - 1, desperateDone)
      }
    }

    inferTypesStep(n, 0, byMembersAfter, false)
  }


}
