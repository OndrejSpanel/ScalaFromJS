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

    def addInferredMemberType(idAccess: Option[MemberId], tpe: Option[TypeInfo], kind: TypeInferenceKind = target) = {

      val id = idAccess.flatMap { i =>
        classInfo.classContains(i.cls, i.name).map { containedIn =>
          i.copy(cls = containedIn)
        }
      }

      val symType = kind(inferred.getMember(id), tpe)
      for (tp <- symType) {
        //println(s"Add member type $idAccess - $id: $tp")
        //println("  " + classInfo)
        if (tp.nonEmpty) {
          inferred = inferred addMember id -> tp
          allTypes.t = allTypes addMember id -> tp
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


    def inferParsOrArgs(pars: js.Array[AST_SymbolFunarg], args: Seq[AST_Node]) = {
      for {
        (Some(par), arg) <- pars.map(_.thedef.nonNull) zip args
      } {
        val tp = expressionType(arg)(ctx)
        if (tp.exists(_.nonEmpty)) {
          //println(s"Infer par ${par.name} as $tp")
          addInferredType(par, tp)
        }

        arg match {
          case AST_SymbolRefDef(a) => // TODO: SymbolInfo
            val tp = allTypes.get(par)
            if (tp.exists(_.nonEmpty)) {
              //println(s"Infer arg ${a.name} as $tp")
              addInferredType(a, tp, source)
            }
          case _ =>
        }
      }
    }

    def inferFunction(args: Seq[AST_Node]) = {
      val pars = args.map(expressionType(_)(ctx))
      FunctionType(AnyType, pars.flatMap(_.map(_.declType)).toIndexedSeq)
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
          allReturns = typeUnionOption(allReturns, tpe)
          false
        case _ =>
          false
      }
      allReturns
    }

    case class SymbolAccessInfo(symbol: Option[SymbolDef] = None, dot: Option[MemberId] = None) {
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

      def addSymbolInferredType(tpe: Option[TypeInfo], kind: TypeInferenceKind = target): Unit = {
        for (s <- symbol) {
          addInferredType(s, tpe, kind)
        }
        for (d <- dot) {
          addInferredMemberType(dot, tpe, kind)
        }
      }

      override def toString = {
        symbol.fold(dot.fold("None")(_.toString))(_.name)
      }
    }

    object SymbolInfo {
      def unapply(arg: AST_Node) = arg match {
        case AST_SymbolRefDef(symDef) =>
          Some(SymbolAccessInfo(symbol = Some(symDef)))
        case AST_Dot(expr, name) =>
          val clsId = memberId(classFromType(expressionType(expr)(ctx)), name)
          Some(SymbolAccessInfo(dot = clsId))
        case AST_Call(AST_Dot(expr, name), _*) =>
          val clsId = memberId(classFromType(expressionType(expr)(ctx)), name)
          Some(SymbolAccessInfo(dot = clsId))
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

        case fun@AST_Defun(Defined(symDef), _, _) =>
          val allReturns = scanFunctionReturns(fun)
          //println(s"${symDef.name} returns $allReturns")
          addInferredType(symDef.thedef.get, allReturns)

        // TODO: derive getters and setters as well
        case AST_ConciseMethod(AST_SymbolName(sym), fun: AST_Lambda) =>
          val tpe = scanFunctionReturns(fun)
          // method of which class is this?
          val scope = findThisClass(fun.parent_scope.nonNull)
          for (AST_DefClass(Defined(AST_SymbolName(cls)), _, _) <- scope) {
            //println(s"Infer return type for method $cls.$sym as $tpe")
            val classId = MemberId(cls, sym)
            addInferredMemberType(Some(classId), tpe)
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
          // infer types for class member calls
          for {
            TypeDecl(ClassType(callOn)) <- expressionType(expr)(ctx)
            clazz <- classes.get(callOn)
            c <- includeParents(clazz, Seq(clazz))(ctx) // infer for all overrides
          } {
            findMethod(c, call).fold {
              val tpe = inferFunction(args)

              //println(s"Infer arg types for a var member call ${c.name.get.name} as $tpe")
              val memberId = c.name.nonNull.map(n => MemberId(n.name, call))
              addInferredMemberType(memberId, Some(TypeInfo.target(tpe))) // target or source?

              for {
                AST_ObjectKeyVal(p, a) <- findProperty(c, call)
                r <- allTypes.getMember(memberId)
              } {
                //println(s"Infer $p $r ${nodeClassName(a)}")
                inferFunctionReturn(a, r)
              }

              // if there are any return statements, we can infer types for them
              // beware of IIFE
              // TODO: reverse inference
            } { m =>
              inferParsOrArgs(m.value.argnames, args)
            }
          }

        case _ =>
      }
      true
    }
    // TODO: protect JSDoc explicit types
    n.copy(types = n.types ++ inferred)
  }

  def multipass(n: AST_Extended): AST_Extended = {

    def inferTypesStep(n: AST_Extended, maxDepth: Int = 50): AST_Extended = {
      //println(s"Type inference: ${n.types}")
      val r = inferTypes(n)
      //println(s"Type inference done: ${r.types}")
      if (r.types != n.types && maxDepth > 0) inferTypesStep(r, maxDepth - 1)
      else r
    }

    inferTypesStep(n)
  }


}
