package com.github.opengrabeso
package transform

import JsUtils._
import Uglify._
import UglifyExt._
import UglifyExt.Import._
import Classes._
import Transform._
import SymbolTypes._

import scala.scalajs.js
import scala.language.implicitConversions

object InferTypes {
  def inferTypes(n: AST_Extended): AST_Extended = {
    var inferred = SymbolTypes()
    val allTypes = Ref(n.types) // keep immutable reference to a mutating var

    val classes = classListHarmony(n)
    //println("Classes:\n" + classes)

    val classInfo = listClassMembers(n.top)

    implicit val ctx = ExpressionTypeContext(allTypes, classInfo, classes) // note: ctx.allTypes is mutable

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

    def inferParsOrArgs(pars: js.Array[AST_SymbolFunarg], args: Seq[AST_Node]) = {

      val parIds = pars.toSeq.map(_.thedef.nonNull).flatMap(_.map(id))

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
      val pars = funType.args.map(par => Some(TypeInfo.target(par)))
      inferArgsFromPars(pars, args)
    }


    def inferFunction(args: Seq[AST_Node]) = {
      val pars = args.map(expressionType(_)(ctx))
      FunctionType(NoType, pars.map(_.fold[TypeDesc](NoType)(_.declType)).toIndexedSeq)
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



    def scanFunctionReturns(node: AST_Lambda) = {
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
      allReturns
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
      def addSymbolInferredType(tpe: Option[TypeInfo], kind: TypeInferenceKind = target): Unit = {
        addInferredType(id(symbol), tpe, kind)
      }

      def unknownType(types: SymbolTypes) = {
        //println(s"Check $s => ${n.types.get(s)}")
        n.types.get(symbol).isEmpty
      }

    }

    case class SymbolAccessDot(symbol: MemberId) extends SymbolAccessInfo {
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

        val arrayTpe = tpe.map { t =>
          TypeInfo(ArrayType(t.source), ArrayType(t.target))
        }
        //println(s"${symbol.name}: Array type $tpe $arrayTpe")
        addInferredType(id(symbol), arrayTpe, kind)
      }

      def unknownType(types: SymbolTypes) = {
        // TODO: implement
        false
      }
    }

    object SymbolInfo {
      def unapply(arg: AST_Node) = arg match {
        case AST_SymbolRefDef(symDef) =>
          Some(SymbolAccessSymbol(symDef))

        case AST_Sub(AST_SymbolRefDef(symDef), property) if expressionType(property)(ctx).forall(_.declType != string) =>
          Some(SymbolAccessArray(symDef))

        case AST_Dot(expr, name) =>
          val clsId = memberId(classFromType(expressionType(expr)(ctx)), name)
          clsId.map(SymbolAccessDot)

        case _ =>
          None
      }
    }

    def inferConstructorCall(args: Seq[AST_Node], className: String) = {
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

    n.top.walkWithDescend { (node, descend, walker) =>
      //println(s"${nodeClassName(node)}")
      descend(node, walker)

      node match {
        case AST_VarDef(AST_Symbol(_, _, Defined(symDef)), Defined(src)) =>
          if (n.types.get(symDef).isEmpty) {
            val tpe = expressionType(src)(ctx)
            //println(s"vardef ${symDef.name} ${id(symDef)} ${nodeClassName(src)} tpe $tpe")
            addInferredType(symDef, tpe)
            // if this is an inline constructor member, infer also the member type from it
            for {
              cls <- findThisClass(Some(symDef.scope))
              AST_SymbolName(clsName) <- cls.name
              fun <- findThisFunction(Some(symDef.scope))
              body <- findInlineBody(cls)
              if body.value == fun
            } {
              //println(s"vardef ${symDef.name} ${nodeClassName(src)} tpe $tpe")
              //println(s"  inline body cls $clsName")
              addInferredMemberType(Some(MemberId(clsName, symDef.name)), tpe)
            }
          }

        case AST_SymbolFunarg(Defined(symDef), _, Defined(JsArray(init))) =>
          if (n.types.get(symDef).isEmpty) {
            val tpe = expressionType(init)(ctx)
            addInferredType(symDef, tpe)
          }

        case AST_Assign(left, _, right) =>
          val leftT = expressionType(left)(ctx)
          val rightT = expressionType(right)(ctx)
          if (leftT != rightT) { // equal: nothing to infer (may be both None, or both same type)
            for (SymbolInfo(symInfo) <- Some(left)) {
              //println(s"Infer assign: $symInfo = $rightT")
              symInfo.addSymbolInferredType(rightT)
            }
            for (SymbolInfo(symInfo) <- Some(right)) {
              //println(s"Infer reverse assign: $leftT = $symInfo")
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
          val allReturns = scanFunctionReturns(fun)
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
          val allReturns = scanFunctionReturns(fun)
          // method of which class is this?
          val scope = findThisClass(fun.parent_scope.nonNull)
          for {
            retType <- allReturns
            AST_DefClass(Defined(AST_SymbolName(cls)), _, _) <- scope
          } {
            //println(s"Infer return type for method $cls.$sym as $retType")
            val classId = MemberId(cls, sym)
            val funType = FunctionType(retType.declType, IndexedSeq())
            addInferredMemberType(Some(classId), Some(TypeInfo.target(funType)))
          }

        case AST_Call(AST_SymbolRefDef(call), args@_*) =>
          //println(s"Call ${call.name}")
          call.orig.headOption match {
            case Some(clazz: AST_SymbolDefClass) => // constructor call in the new Class(x)
              inferConstructorCall(args, clazz.name)

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
          for (sup <- findSuperClass(s.scope.nonNull)(ctx)) {
            //println(s"Super call of $sup")
            inferConstructorCall(args, sup)
          }

        case AST_Call(AST_Dot(expr, call), args@_*) =>

          //println(s"Dot call $call")
          // fill ctx.type function types information
          for {
            TypeDecl(ClassType(callOn)) <- expressionType(expr)(ctx)
            c <- getParents(callOn)(ctx) // infer for all overrides
          } {
            val memberId = MemberId(c, call)
            //println(s"memberId $memberId, args ${args.length}")
            if (ctx.classInfo.containsMember(c, call)) {
              val tpe = inferFunction(args)

              //println(s"Infer par types for a var member call $c.$call as $tpe")
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
            TypeDecl(ClassType(callOn)) <- expressionType(expr)(ctx)
            clazz <- classes.get(callOn)
            c <- includeParents(clazz, Seq(clazz))(ctx) // infer for all overrides
            m <- findMethod(c, call)
          } {
            inferParsOrArgs(m.value.argnames, args)
          }

        case AST_Sub(AST_SymbolRef(name, _, Defined(sym)), property) =>
          expressionType(property)(ctx).map(_.declType) match {
            case Some(`number`) =>
              addInferredType(sym, Some(TypeInfo.target(ArrayType(NoType))))
            case _ =>

          }
        case _ =>
      }
      true
    }
    // TODO: protect JSDoc explicit types
    n.copy(types = n.types ++ inferred)
  }

  def multipass(n: AST_Extended): AST_Extended = {
    val maxDepth = 15
    def inferTypesStep(n: AST_Extended, depth: Int = 0): AST_Extended = {
      //println(s"Type inference: ${n.types} steps $maxDepth")
      val now = System.currentTimeMillis()
      val r = inferTypes(n)
      val again = System.currentTimeMillis()
      println(s"Infer types step $depth: ${again - now} ms")
      val cr = ClassesByMembers(r)
      println(s"Classes by members types: ${{System.currentTimeMillis()} - again} ms")

      //println(s"Type inference done: ${r.types}")
      if (cr.types != n.types && depth < maxDepth) inferTypesStep(cr, depth + 1)
      else cr
    }

    inferTypesStep(n)
  }


}
