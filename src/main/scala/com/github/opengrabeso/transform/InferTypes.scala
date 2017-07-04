package com.github.opengrabeso
package transform

import JsUtils._
import Uglify._
import UglifyExt._
import UglifyExt.Import._
import Classes._
import Transform._
import SymbolTypes._
import scala.language.implicitConversions

object InferTypes {

  def scanFunctionReturns(node: AST_Lambda)(ctx: ExpressionTypeContext): Option[TypeInfo] = {
    import ctx.classOps

    var allReturns = Option.empty[TypeInfo]
    node.walk {
      // include any sub-scopes, but not local functions
      case innerFunc: AST_Lambda if innerFunc != node =>
        true
      case AST_Return(Defined(value)) =>
        //println(s"  return expression ${nodeClassName(value)}")
        val tpe = expressionType(value)(ctx)
        //println(s"  Return type $tpe: expr ${ScalaOut.outputNode(value)}")
        if (tpe.isDefined) { // unknown types introduce Any return value, which we never get rid of
          allReturns = typeUnionOption(allReturns, tpe)
        }
        false
      case _ =>
        false
    }
    allReturns.orElse {
      //println("Infer no return function")
      node.body.toSeq match {
        case Seq(AST_SimpleStatement(ex)) => expressionType(ex)(ctx)
        case _ => None
      }
    }
  }

  def inferTypes(n: AST_Extended): AST_Extended = {
    var inferred = SymbolTypes()
    val allTypes = Ref(n.types) // keep immutable reference to a mutating var

    val classes = new ClassListHarmony(n)
    //println("Classes:\n" + classes.keys)


    val classInfo = listClassMembers(n.top)
    //println("ClassInfo:\n" + classInfo)

    implicit val ctx = ExpressionTypeContext(allTypes, classInfo, classes) // note: ctx.allTypes is mutable
    import ctx.classPos
    import ctx.classOps

    def typeFromOperation(op: String, n: AST_Node) = {
      op match {
        case IsComparison() =>
          // comparison - most like the type we are comparing to
          expressionType(n)(ctx)
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

    def addInferredType(tid: Option[SymbolMapId], tpe: Option[TypeInfo], kind: TypeInferenceKind = target) = {
      if (tpe.exists(_.known)) {
        def noType = Seq("undefined", "null", "this", "super") // never infer anything about those identifiers

        if (tid.exists(t => !(noType contains t.name))) {
          val symType = kind(allTypes.get(tid), tpe)
          //println(s"  Combined $symType = ${allTypes.get(tid)} * $tpe")
          for (tp <- symType) {
            if (tp.nonEmpty) {
              //println(s"  Add type $tid: $tp")

              /*
              if (tid.exists(_.name == "x") && tpe.exists(_.declType != number)) {
                println(s"Suspicious $tid type $tpe")

              }

              if (tid.contains(SymbolMapId("value", 294250)) && tpe.exists(_.declType != number)) {
                println(s"Suspicious value $tid type $tpe")
              } else if (tid.contains(SymbolMapId("value", 294250))) {
                println(s"Inferred value $tid type $tpe")
              }
              */

              inferred += tid -> tp
              allTypes.t += tid -> tp
            }
            //println(s"All types ${allTypes.t.types}")
            //println(s"inferred ${inferred.types}")
          }
        }
      }
    }

    def addInferredMemberType(idAccess: Option[MemberId], tpe: Option[TypeInfo], kind: TypeInferenceKind = target) = {
      if (tpe.exists(_.known)) {

        val id = idAccess.flatMap { i =>
          classInfo.classContains(i.cls, i.name).map { containedIn =>
            i.copy(cls = containedIn)
          }
        }

        //println(s"Member type was $id: ${inferred.getMember(id)}")

        val symType = kind(inferred.getMember(id), tpe)
        //println(s"Adding member type $id: $tpe -> $symType")

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
          //println(s"Add member type $idAccess - $id: $tp")
          //println("  " + classInfo)
          if (tp.nonEmpty) {
            inferred = inferred addMember id -> tp
            allTypes.t = allTypes addMember id -> tp
          }
        }
      }
    }

    // list all functions so that we can look-up them from call sites
    var functions = Map.empty[AST_SymbolDeclaration, AST_Defun]
    n.top.walk {
      case defun@AST_Defun(Defined(name),_,_) =>
        functions += name -> defun
        false
      case _ =>
        false
    }

    //println(functions.map(f => f._1.name))

    def inferArgsFromPars(pars: Seq[Option[TypeInfo]], args: Seq[AST_Node]) = {
      for ((Some(par), arg) <- pars zip args) {
        arg match {
          case SymbolInfo(a) =>
            //println(s"Infer arg $a as $par")
            a.addSymbolInferredType(Some(par), source)
          case _ =>
        }
      }
    }

    def inferParsOrArgs(pars: Seq[AST_SymbolFunarg], args: Seq[AST_Node]) = {

      val parIds = pars.map(_.thedef.nonNull).flatMap(_.map(id))

      for ((Some(par), arg) <- parIds zip args) {
        val tp = expressionType(arg)(ctx)
        if (tp.exists(_.nonEmpty)) {
          //println(s"Infer par $par as $tp")
          addInferredType(Some(par), tp)
        }
      }

      inferArgsFromPars(parIds.map(allTypes.get), args)
    }

    def inferArgs(funType: FunctionType, args: Seq[AST_Node]) = {
      val argTypes = funType.args.map(par => Some(TypeInfo.target(par)))
      //println(s"Infer args for $funType, $args, $argTypes")
      inferArgsFromPars(argTypes, args)
    }


    def inferFunction(pars: Seq[AST_Node]) = {
      val parTypes = pars.map(expressionType(_)(ctx))
      //println(s"  $pars $parTypes")
      FunctionType(NoType, parTypes.map(_.fold[TypeDesc](NoType)(_.declType)).toIndexedSeq)
    }

    def inferFunctionReturn(value: AST_Node, r: TypeInfo) = {
      r.declType match {
        case fType: FunctionType =>
          walkLastNode(value) {
            // find any direct returns, when returning a function, infer argument symbol types
            case AST_Lambda(args, body) =>
              //println(s"fun ${args.map(_.name).mkString(",")} -- ${fType.args}")
              //println(s"  $allTypes")

              for {
                (a, tp) <- args zip fType.args
                //_ = println(s"${a.thedef.nonNull.map(_.name)} $tp")
                sym <- a.thedef.nonNull
              } {
                val sid = id(sym)
                if (n.types.get(sid).isEmpty) {
                  //println(s"  Infer arg ${a.name} as $tp")
                  addInferredType(sid, Some(TypeInfo.source(tp)))
                }
              }

              true
            case _ =>
              false

          }
        case _ =>
      }
    }



    /*
      def unknownType(types: SymbolTypes): Boolean = {
        symbol.fold{
          dot.fold(false) { d =>
            val p = findInParents(d.cls, d.name)(ctx)
            //println(s"Check $d => $p = ${n.types.getMember(dot)}")
            n.types.getMember(p.map(pp => d.copy(cls = pp))).isEmpty
          }
        } { s =>
          //println(s"Check $s => ${n.types.get(s)}")
          n.types.get(s).isEmpty
        }
      }
    * */
    trait SymbolAccessInfo {
      def addSymbolInferredType(tpe: Option[TypeInfo], kind: TypeInferenceKind = target): Unit
      def unknownType(types: SymbolTypes): Boolean
    }

    case class SymbolAccessSymbol(symbol: SymbolDef) extends SymbolAccessInfo {
      override def toString = s"Symbol(${id(symbol)})"

      def addSymbolInferredType(tpe: Option[TypeInfo], kind: TypeInferenceKind = target): Unit = {
        addInferredType(id(symbol), tpe, kind)
      }

      def unknownType(types: SymbolTypes) = {
        //println(s"Check $s => ${n.types.get(s)}")
        n.types.get(symbol).isEmpty
      }

    }

    case class SymbolAccessDot(symbol: MemberId) extends SymbolAccessInfo {
      override def toString = s"Member(${symbol.cls}.${symbol.name}})"

      def addSymbolInferredType(tpe: Option[TypeInfo], kind: TypeInferenceKind = target): Unit = {
        addInferredMemberType(Some(symbol), tpe, kind)
      }

      def unknownType(types: SymbolTypes) =  {
        val p = findInParents(symbol.cls, symbol.name)(ctx)
        //println(s"Check $d => $p = ${n.types.getMember(dot)}")
        types.getMember(p.map(pp => symbol.copy(cls = pp))).isEmpty
      }
    }

    case class SymbolAccessArray(symbol: SymbolDef) extends SymbolAccessInfo {
      def addSymbolInferredType(tpe: Option[TypeInfo], kind: TypeInferenceKind = target): Unit = {

        val mappedTpe = tpe.map(_.map(ArrayType))
        //println(s"${symbol.name}: Array type $tpe $mappedTpe")
        addInferredType(id(symbol), mappedTpe, kind)
      }

      def unknownType(types: SymbolTypes) = {
        // TODO: implement
        false
      }
    }

    case class SymbolAccessMap(symbol: SymbolDef) extends SymbolAccessInfo {
      def addSymbolInferredType(tpe: Option[TypeInfo], kind: TypeInferenceKind = target): Unit = {

        val mappedTpe = tpe.map(_.map(MapType))
        //println(s"${symbol.name}: Map type $tpe $mappedTpe")
        addInferredType(id(symbol), mappedTpe, kind)
      }

      def unknownType(types: SymbolTypes) = {
        // TODO: implement
        false
      }
    }

    case class SymbolAccessDotMap(symbol: MemberId) extends SymbolAccessInfo {
      def addSymbolInferredType(tpe: Option[TypeInfo], kind: TypeInferenceKind = target): Unit = {
        val mappedTpe = tpe.map(_.map(MapType))
        addInferredMemberType(Some(symbol), mappedTpe, kind)
      }

      def unknownType(types: SymbolTypes) =  { // TODO: verify
        val p = findInParents(symbol.cls, symbol.name)(ctx)
        //println(s"Check $d => $p = ${n.types.getMember(dot)}")
        types.getMember(p.map(pp => symbol.copy(cls = pp))).isEmpty
      }
    }

    case class SymbolAccessDotArray(symbol: MemberId) extends SymbolAccessInfo {
      def addSymbolInferredType(tpe: Option[TypeInfo], kind: TypeInferenceKind = target): Unit = {
        val mappedTpe = tpe.map(_.map(ArrayType))
        addInferredMemberType(Some(symbol), mappedTpe, kind)
      }

      def unknownType(types: SymbolTypes) =  { // TODO: verify
        val p = findInParents(symbol.cls, symbol.name)(ctx)
        //println(s"Check $d => $p = ${n.types.getMember(dot)}")
        types.getMember(p.map(pp => symbol.copy(cls = pp))).isEmpty
      }
    }

    object SymbolInfo {
      def unapply(arg: AST_Node) = arg match {
        case AST_SymbolRefDef(symDef) =>
          Some(SymbolAccessSymbol(symDef))

        case AST_SymbolRefDef(symDef) AST_Sub property =>
          //println(s"${symDef.name} - AST_Sub")
          expressionType(property)(ctx).flatMap {
            _.declType match {
              case `number` =>
                Some(SymbolAccessArray(symDef)) // TODO: derive property is most likely Int, not Double
              case `string` =>
                //println(s"${symDef.name} - map")
                Some(SymbolAccessMap(symDef))
              case _ =>
                None
            }
          }

        case expr AST_Dot name AST_Sub property =>
          // consider DRY with the case above
          expressionType(property)(ctx).flatMap {
            _.declType match {
              case `number` =>
                val clsId = memberId(classFromType(expressionType(expr)(ctx)), name)
                //println(s"$clsId - dot - array")
                clsId.map(SymbolAccessDotArray)
              case `string` =>
                val clsId = memberId(classFromType(expressionType(expr)(ctx)), name)
                //println(s"$clsId - dot - map")
                clsId.map(SymbolAccessDotMap)
              case _ =>
                None
            }
          }



        case expr AST_Dot name =>
          val clsId = memberId(classFromType(expressionType(expr)(ctx)), name)
          clsId.map(SymbolAccessDot)

        case _ =>
          None
      }
    }

    def inferConstructorCall(args: Seq[AST_Node], className: SymbolMapId) = {
      //println(s"Infer arg types for class $className")
      for (c <- classes.get(className)) {
        {
          val value = classInlineBody(c)
          //println(s"  Constructor inline pars ${value.argnames.map(_.name)} args ${args.map(ScalaOut.outputNode(_))}")
          inferParsOrArgs(value.argnames, args)
        }

        for (AST_ConciseMethod(_, value: AST_Accessor) <- findConstructor(c)) {
          //println(s"  Constructor pars ${value.argnames.map(_.name)} args ${args.map(ScalaOut.outputNode(_))}")
          inferParsOrArgs(value.argnames, args)
        }
      }
    }

    object KnownType {
      def unapply(arg: AST_Node)(implicit ctx: ExpressionTypeContext): Option[TypeInfo] = {
        val tpe = expressionType(arg)(ctx)
        //println(s"  check type of ${ScalaOut.outputNode(arg)} as $tpe")
        tpe
      }
    }
    object GetArrayType {
      def unapply(arg: AST_Node)(implicit ctx: ExpressionTypeContext): Option[TypeInfo] = {
        val exprType = expressionType(arg)(ctx).map(_.declType)
        exprType match {
          case Some(ArrayType(tpe)) =>
            Some(TypeInfo.source(tpe))
          case _ =>
            None
        }

      }
    }

    n.top.walkWithDescend { (node, descend, walker) =>
      //println(s"${nodeClassName(node)}")
      descend(node, walker)

      node match {
        case AST_VarDef(AST_SymbolDef(symDef), Defined(right)) =>
          val log = false
          val leftT = n.types.get(symDef)
          val rightT = expressionType(right)(ctx)
          if (log) println(s"Infer var $leftT - $rightT ${ScalaOut.outputNode(node)}")
          if (leftT != rightT) { // equal: nothing to infer (may be both None, or both same type)
            for (symInfo <- Some(SymbolAccessSymbol(symDef))) {
              if (log) println(s"  Infer var: $symInfo = $rightT")
              symInfo.addSymbolInferredType(rightT)
              if (log) println(s"  as: ${allTypes.get(symDef)}")
            }
            for (SymbolInfo(symInfo) <- Some(right)) {
              if (log) println(s"  Infer reverse var: $leftT = $symInfo")
              symInfo.addSymbolInferredType(leftT, source)
            }
          }

        case AST_SymbolFunarg(Defined(symDef), _, Defined(JsArray(init))) =>
          if (n.types.get(symDef).isEmpty) {
            val tpe = expressionType(init)(ctx)
            addInferredType(symDef, tpe)
          }

        // a few special forms of assignment should infer no type - cyclic dependencies
        case AST_Assign(SymbolInfo(symLeft), _, SymbolInfo(symRight)) if symLeft == symRight =>

        case AST_Assign(left, _, right) =>
          val log = false
          val leftT = expressionType(left)(ctx)
          val rightT = expressionType(right)(ctx)
          if (log) println(s"Infer assign $leftT - $rightT ${ScalaOut.outputNode(node)}")
          if (leftT != rightT) { // equal: nothing to infer (may be both None, or both same type)
            for (SymbolInfo(symInfo) <- Some(left)) {
              if (log) println(s"  Infer assign: $symInfo = $rightT")
              symInfo.addSymbolInferredType(rightT)
            }
            for (SymbolInfo(symInfo) <- Some(right)) {
              if (log) println(s"  Infer reverse assign: $leftT = $symInfo")
              symInfo.addSymbolInferredType(leftT, source)
            }
          }

        case AST_Binary(SymbolInfo(symLeft), IsArithmetic(), SymbolInfo(symRight))
          if symLeft.unknownType(n.types) && symRight.unknownType(n.types) =>
          //println(s"Infer arithmetic: both unknown $symLeft $symRight")
          val numType = Some(TypeInfo.both(number))
          symLeft.addSymbolInferredType(numType)
          symRight.addSymbolInferredType(numType)

        case AST_Binary(SymbolInfo(symInfo), op, expr) if symInfo.unknownType(n.types) =>
          //println(s"Infer binary: left unknown $symInfo")
          val tpe = typeFromOperation(op, expr)
          symInfo.addSymbolInferredType(tpe)

        case AST_Binary(expr, op, SymbolInfo(symInfo)) if symInfo.unknownType(n.types) =>
          //println(s"Infer binary: right unknown $symInfo")
          val tpe = typeFromOperation(op, expr)
          symInfo.addSymbolInferredType(tpe)

        case AST_Switch(SymbolInfo(symInfo), body) =>
          var allCases = Option.empty[TypeInfo]
          body.foreach {
            case cc: AST_Case =>
              val cType = expressionType(cc.expression)(ctx)
              allCases = typeUnionOption(allCases, cType)
            case _ =>
          }
          symInfo.addSymbolInferredType(allCases)

        case fun@AST_Defun(Defined(symDef), _, _) =>
          val allReturns = scanFunctionReturns(fun)(ctx)
          //println(s"${symDef.name} returns $allReturns")
          for {
            retType <- allReturns
            sd <- symDef.thedef
          } {
            // parameters do not matter here, they are infered as separate symbols
            val funType = FunctionType(retType.declType, IndexedSeq())
            addInferredType(sd, Some(TypeInfo.target(funType)))
          }

        // TODO: derive getters and setters as well
        case AST_ConciseMethod(AST_SymbolName(sym), fun: AST_Lambda) =>
          val allReturns = scanFunctionReturns(fun)(ctx)
          // method of which class is this?
          val scope = findThisClass(fun.parent_scope.nonNull)
          for {
            retType <- allReturns
            AST_DefClass(Defined(AST_SymbolDef(cls)), _, _) <- scope
            clsId <- id(cls)
          } {
            //println(s"Infer return type for method $cls.$sym as $retType")
            val classId = MemberId(clsId, sym)
            val funType = FunctionType(retType.declType, IndexedSeq())
            addInferredMemberType(Some(classId), Some(TypeInfo.target(funType)))
          }
        case AST_ObjectGetter(AST_SymbolName(sym), fun: AST_Lambda) =>
          val allReturns = scanFunctionReturns(fun)(ctx)
          // method of which class is this?
          val scope = findThisClass(fun.parent_scope.nonNull)
          //println(s"Infer getter $sym as $allReturns")
          for {
            retType <- allReturns
            AST_DefClass(Defined(AST_SymbolDef(cls)), _, _) <- scope
            clsId <- id(cls)
          } {
            //println(s"Infer return type for getter $cls.$sym as $retType")
            val classId = MemberId(clsId, sym)
            addInferredMemberType(Some(classId), Some(retType))
          }
        case AST_ObjectSetter(AST_SymbolName(sym), fun: AST_Lambda) =>
          // method of which class is this?
          val scope = findThisClass(fun.parent_scope.nonNull)
          //println(s"Infer setter $sym")

          for {
            arg <- fun.argnames.headOption
            retType <- allTypes.get(arg.thedef.nonNull.flatMap(id))
            AST_DefClass(Defined(AST_SymbolDef(cls)), _, _) <- scope
            clsId <- id(cls)
          } {
            //println(s"Infer return type for setter $cls.$sym as $retType")
            val classId = MemberId(clsId, sym)
            // target, because setter parameter is typically used as a source for the property variable, which sets source only
            addInferredMemberType(Some(classId), Some(TypeInfo.target(retType.declType)))
          }
        case AST_ObjectKeyVal(name, value) =>
          val scope = findThisClassInWalker(walker)
          for {
            AST_DefClass(Defined(AST_SymbolDef(cls)), _, _) <- scope
            clsId <- id(cls)
          } {
            val classId = MemberId(clsId, name)
            // target, because setter parameter is typically used as a source for the property variable, which sets source only
            val tpe = expressionType(value)(ctx)
            //println(s"Infer type for value $cls.$name as $tpe")
            addInferredMemberType(Some(classId), tpe)
          }


        case AST_Call(AST_SymbolRefDef(call), args@_*) =>
          //println(s"Call ${call.name}")
          call.orig.headOption match {
            case Some(clazz: AST_SymbolDefClass) => // constructor call in the new Class(x)
              for {
                clsSym <- clazz.thedef.nonNull
                clsId <- id(clsSym)
              } {
                inferConstructorCall(args, clsId)
              }

            case Some(defunSym: AST_SymbolDefun) => // normal function call
              //println(s"Infer arg types for ${defunSym.name}")
              functions.get(defunSym) match {
                case Some(AST_Defun(_, pars, _)) =>
                  // now match arguments to parameters
                  inferParsOrArgs(pars, args)
                case _ =>
              }
            case Some(varSym: AST_SymbolVar) =>
              val tpe = inferFunction(args)
              //println(s"Infer arg types for a var call ${varSym.name} as $tpe")
              varSym.thedef.foreach {
                addInferredType(_, Some(TypeInfo.target(tpe)))
              }
            // TODO: reverse inference
            case _ =>
          }
        case AST_Call(s: AST_Super, args@_*) =>
          for (sup <- findSuperClass(s.scope.nonNull)) {
            //println(s"Super call of $sup")
            inferConstructorCall(args, sup)
          }

        case AST_Call(expr AST_Dot call, args@_*) =>
          val exprType = expressionType(expr)(ctx)

          (exprType.map(_.declType),call,expr) match {
            case (Some(ArrayType(elemType)), "push", SymbolInfo(sym)) =>
              if (args.nonEmpty) {
                val elemType = args.map(expressionType(_)(ctx)).reduce(typeUnionOption)
                sym.addSymbolInferredType(elemType.map(_.map(ArrayType)))
              }
            case _ =>
              //println(s"Dot call $call")
              // fill ctx.type function types information
              for {
                TypeDecl(ClassType(callOn)) <- exprType
                c <- getParents(callOn)(ctx) // infer for all overrides
              } {
                val memberId = MemberId(c, call)
                //println(s"memberId $memberId, args ${args.mkString(",")}")
                if (ctx.classInfo.containsMember(c, call)) {
                  val tpe = inferFunction(args)

                  //println(s"Infer par types for a member call $c.$call as $tpe")
                  //println(allTypes)
                  addInferredMemberType(Some(memberId), Some(TypeInfo.target(tpe))) // target or source?

                  for (funType <- ctx.types.getMember(Some(memberId))) {
                    funType.declType match {
                      case ft: FunctionType =>
                        inferArgs(ft, args)
                      case _ =>

                    }
                  }
                }
              }

              // TODO: use function types only for member functions
              // fill class symbols (if they exists)
              for {
                TypeDecl(ClassType(callOn)) <- exprType
                clazz <- classes.get(callOn)
                c <- includeParents(clazz, Seq(clazz))(ctx) // infer for all overrides
                m <- findMethod(c, call)
              } {
                inferParsOrArgs(m.value.argnames, args)
              }
          }

        case AST_SymbolRef(_, _, Defined(sym)) AST_Sub property =>
          expressionType(property)(ctx).map(_.declType) match {
            case Some(`number`) =>
              addInferredType(sym, Some(TypeInfo.target(ArrayType(NoType))))
            case Some(`string`) =>
              addInferredType(sym, Some(TypeInfo.target(MapType(NoType))))

            case _ =>

          }

        case expr AST_Dot memberName AST_Sub property =>
          for (clsName <- SymbolTypes.classFromType(expressionType(expr)(ctx))) {
            val memberId = MemberId(clsName, memberName)

            implicit val classPos: (SymbolMapId) => Int = classes.classPos
            val symId = Some(ctx.types.symbolFromMember(memberId))
            expressionType(property)(ctx).map(_.declType) match {
              case Some(`number`) =>
                addInferredType(symId, Some(TypeInfo.target(ArrayType(NoType))))
              case Some(`string`) =>
                addInferredType(symId, Some(TypeInfo.target(MapType(NoType))))
              case _ =>
            }
          }

        case _ =>
      }
      true
    }
    // TODO: protect JSDoc explicit types
    //println(s"inferred ${inferred.types}")
    val ret = n.copy(types = n.types ++ inferred)
    println(s"n.types ${ret.types.types.filter(_._1.sourcePos>=0)}")
    ret
  }

  def multipass(n: AST_Extended): AST_Extended = {
    val maxDepth = 15
    val byMembersAfter = 3
    def inferTypesStep(n: AST_Extended, depth: Int, metrics: Int, byMembers: Int): AST_Extended = {
      val log = false
      //if (log) println(s"Type inference: ${n.types} steps $maxDepth")
      val now = System.currentTimeMillis()
      val r = if (byMembers == 0) ClassesByMembers(n) else inferTypes(n)
      val again = System.currentTimeMillis()
      if (log) println(s"Infer types ${if (byMembers == 0) "by members " else ""}step $depth, metrics: ${r.types.knownItems}: ${again - now} ms")
      val newMetrics = r.types.knownItems

      //if (log) println(s"Type inference done: ${cr.types}")
      // if metrics was not improved, use previous result, after byMembers always try another normal inference
      if ((newMetrics > metrics || byMembers == 0) && depth < maxDepth) {
        inferTypesStep(r, depth + 1, newMetrics, byMembers - 1) // never repeat byMembers
      }
      else if (byMembers > 0) {
        inferTypesStep(n, depth + 1, metrics, 0) // normal inference exhausted, perform byMembers
      } else {
        n
      }
    }

    inferTypesStep(n, 0, 0, byMembersAfter)
  }


}
