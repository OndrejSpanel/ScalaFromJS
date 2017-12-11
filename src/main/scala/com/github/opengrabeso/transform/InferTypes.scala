package com.github.opengrabeso
package transform

import JsUtils._
import net.gamatron.esprima._
import esprima._

import Classes._
import Transform._
import SymbolTypes._
import scala.language.implicitConversions

object InferTypes {

  def scanFunctionReturns(node: Node.Lambda)(ctx: ExpressionTypeContext): Option[TypeInfo] = {
    import ctx.classOps

    var allReturns = Option.empty[TypeInfo]
    node.walk {
      // include any sub-scopes, but not local functions
      case innerFunc: Node.Lambda if innerFunc != node =>
        true
      case Node.Return(Defined(value)) =>
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
        case Seq(Node.SimpleStatement(ex)) => expressionType(ex)(ctx)
        case _ => None
      }
    }
  }

  def inferTypes(n: NodeExtended): NodeExtended = {

    // TODO: cleanup inferred, was used for members, not for variables, now it is not used at all
    val allTypes = Ref(n.types) // keep immutable reference to a mutating var

    val classes = new ClassListHarmony(n)
    //println("Classes:\n" + classes.keys)


    val classInfo = listClassMembers(n)
    //println("ClassInfo:\n" + classInfo)

    implicit val ctx = ExpressionTypeContext(allTypes, classInfo, classes) // note: ctx.allTypes is mutable
    import ctx.classPos
    import ctx.classOps

    def typeFromOperation(op: String, n: Node.Node) = {
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

    def addInferredType(tid: Option[SymbolMapId], tpe: Option[TypeInfo], kind: TypeInferenceKind = target)(debug: String*) = {
      if (tpe.exists(_.known)) {
        def noType = Seq("undefined", "null", "this", "super") // never infer anything about those identifiers

        if (tid.exists(t => !(noType contains t.name))) {
          val oldType = allTypes.get(tid)
          val symType = kind(oldType, tpe)
          //println(s"  Combined $symType = ${allTypes.get(tid)} * $tpe")
          for (tp <- symType) {

            if (tp.nonEmpty && tp.acceptable && !oldType.exists(tp.equivalent)) {
              //println(s"  Add type $tid: $tp")

              if (tid.exists(watchedSym)) {
                println(s"Watched ${tid.get} result $tp, type ${tpe.get}, was $oldType")
                debug.foreach(s => println("  " + s))
              }

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

              if (!allTypes.t.locked) {
                allTypes.t += tid -> tp
              } else {
                // allow only unknown type replacement
                if (allTypes.t.get(tid).forall(tp.isSafeReplacementOf)) {
                  allTypes.t += tid -> tp
                }
              }
            } else if (false) {
              if (tid.exists(watchedSym)) {
                println(s"Watched ${tid.get} type $oldType === $tp from ${tpe.get}")
                debug.foreach(s => println("  " + s))
              }
            }
            //println(s"All types ${allTypes.t.types}")
            //println(s"inferred ${inferred.types}")
          }
        }
      }
    }

    def addInferredMemberType(idAccess: Option[MemberId], tpe: Option[TypeInfo], kind: TypeInferenceKind = target)(debug: String*) = {
      if (tpe.exists(_.known)) {

        val id = idAccess.flatMap { i =>
          classInfo.classContains(i.cls, i.name).map { containedIn =>
            i.copy(cls = containedIn)
          }
        }

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
          if (tp.nonEmpty && !oldType.exists(tp.equivalent)) {

            if (id.exists(_.isWatched)) {
              println(s"Watched ${id.get} result $tp, type ${tpe.get}, was $oldType")
              debug.foreach(s => println("  " + s))
            }


            allTypes.t = allTypes addMember id -> tp
          } else  if (false) {
            if (id.exists(_.isWatched)) {
              println(s"Watched ${id.get} type $oldType === $tp from ${tpe.get}")
              debug.foreach(s => println("  " + s))
            }
          }
        }
      }
    }

    // list all functions so that we can look-up them from call sites
    var functions = Map.empty[Node.SymbolDeclaration, Node.Defun]
    n.top.walk {
      case defun@Node.Defun(Defined(name),_,_) =>
        functions += name -> defun
        false
      case _ =>
        false
    }

    //println(functions.map(f => f._1.name))

    def inferArgsFromPars(pars: Seq[(Option[SymbolMapId], Option[TypeInfo])], args: Seq[Node.Node])(debug: String*) = {
      for (((parId, Some(par)), arg) <- pars zip args) {
        arg match {
          case SymbolInfo(a) =>
            //println(s"Infer arg $a as $par")
            a.addSymbolInferredType(Some(par), source)(s"inferArgsFromPars $parId: $par $arg" +: debug:_*)
          case _ =>
        }
      }
    }

    def inferParsOrArgs(pars: Seq[Node.SymbolFunarg], args: Seq[Node.Node])(debug: String*) = {

      //println(s"inferParsOrArgs $pars")
      val parIds = pars.map(symbolFromPar).flatMap(_.map(id))
      //println(s"  parIds $parIds")

      for ((Some(par), arg) <- parIds zip args) {
        val tp = expressionType(arg)(ctx)
        if (tp.exists(_.nonEmpty)) {
          //println(s"Infer par $par as $tp")
          addInferredType(Some(par), tp)(s"inferParsOrArgs $par $arg" +: debug:_*)
        }
      }

      inferArgsFromPars(parIds.map(p => p -> allTypes.get(p)), args)(debug:_*)
    }

    def inferArgs(funType: FunctionType, args: Seq[Node.Node])(debug: String*) = {
      val argTypes = funType.args.map(par => None -> Some(TypeInfo.target(par)))
      //println(s"Infer args for $funType, $args, $argTypes")
      inferArgsFromPars(argTypes, args)(debug:_*)
    }


    def inferFunction(pars: Seq[Node.Node], log: Boolean) = {
      val parTypes = pars.map(expressionType(_, log)(ctx))
      //println(s"  $pars $parTypes")
      FunctionType(AnyType, parTypes.map(_.fold[TypeDesc](NoType)(_.target)).toIndexedSeq)
    }

    def inferFunctionReturn(value: Node.Node, r: TypeInfo) = {
      r.declType match {
        case fType: FunctionType =>
          walkLastNode(value) {
            // find any direct returns, when returning a function, infer argument symbol types
            case Node.Lambda(args, body) =>
              //println(s"fun ${args.map(_.name).mkString(",")} -- ${fType.args}")
              //println(s"  $allTypes")

              for {
                (a, tp) <- args zip fType.args
                //_ = println(s"${a.thedef.nonNull.map(_.name)} $tp")
                sym <- symbolFromPar(a)
              } {
                val sid = id(sym)
                if (n.types.get(sid).isEmpty) {
                  //println(s"  Infer arg ${a.name} as $tp")
                  addInferredType(sid, Some(TypeInfo.source(tp)))(s"inferFunctionReturn $value")
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
      def addSymbolInferredType(tpe: Option[TypeInfo], kind: TypeInferenceKind = target)(debug: String*): Unit
      def tpe(types: SymbolTypes): Option[TypeInfo]
      def unknownType(types: SymbolTypes): Boolean = tpe(types).isEmpty
    }

    case class SymbolAccessSymbol(symbol: SymbolDef) extends SymbolAccessInfo {
      override def toString = s"Symbol(${id(symbol)})"

      def addSymbolInferredType(tpe: Option[TypeInfo], kind: TypeInferenceKind = target)(debug: String*): Unit = {
        //println(s"SymbolAccessSymbol: addSymbolInferredType $this $tpe")
        addInferredType(id(symbol), tpe, kind)(debug:_*)
      }

      def tpe(types: SymbolTypes) = {
        //println(s"Check $s => ${n.types.get(s)}")
        n.types.get(symbol)
      }

    }

    case class SymbolAccessDot(symbol: MemberId) extends SymbolAccessInfo {
      override def toString = s"Member(${symbol.cls}.${symbol.name})"

      def addSymbolInferredType(tpe: Option[TypeInfo], kind: TypeInferenceKind = target)(debug: String*): Unit = {
        //println(s"SymbolAccessDot: addSymbolInferredType $this $tpe")
        addInferredMemberType(Some(symbol), tpe, kind)(s"Member $symbol" +: debug:_*)
      }

      def tpe(types: SymbolTypes) =  {
        val p = findInParents(symbol.cls, symbol.name)(ctx)
        //println(s"Check $d => $p = ${n.types.getMember(dot)}")
        types.getMember(p.map(pp => symbol.copy(cls = pp)))
      }
    }

    case class SymbolAccessArray(symbol: SymbolDef) extends SymbolAccessInfo {
      def addSymbolInferredType(tpe: Option[TypeInfo], kind: TypeInferenceKind = target)(debug: String*): Unit = {
        //println(s"SymbolAccessArray: addSymbolInferredType $this $tpe")

        val mappedTpe = tpe.map(_.map(ArrayType))
        //println(s"${symbol.name}: Array type $tpe $mappedTpe")
        addInferredType(id(symbol), mappedTpe, kind)("Array" +: debug:_*)
      }

      def tpe(types: SymbolTypes) =  {
        val t = types.get(id(symbol))
        t.map(_.map(ArrayType))
      }
    }

    case class SymbolAccessMap(symbol: SymbolDef) extends SymbolAccessInfo {
      def addSymbolInferredType(tpe: Option[TypeInfo], kind: TypeInferenceKind = target)(debug: String*): Unit = {
        //println(s"SymbolAccessMap: addSymbolInferredType $this $tpe")

        val mappedTpe = tpe.map(_.map(MapType))
        //println(s"${symbol.name}: Map type $tpe $mappedTpe")
        addInferredType(id(symbol), mappedTpe, kind)("Map" +: debug:_*)
      }

      def tpe(types: SymbolTypes) =  {
        val t = types.get(id(symbol))
        t.map(_.map(MapType))
      }
    }

    case class SymbolAccessDotMap(symbol: MemberId) extends SymbolAccessInfo {
      def addSymbolInferredType(tpe: Option[TypeInfo], kind: TypeInferenceKind = target)(debug: String*): Unit = {
        val mappedTpe = tpe.map(_.map(MapType))
        addInferredMemberType(Some(symbol), mappedTpe, kind)(s"Dot $symbol" +: debug:_*)
      }

      def tpe(types: SymbolTypes) =  { // TODO: verify
        val p = findInParents(symbol.cls, symbol.name)(ctx)
        //println(s"Check $d => $p = ${n.types.getMember(dot)}")
        types.getMember(p.map(pp => symbol.copy(cls = pp)))
      }
    }

    case class SymbolAccessDotArray(symbol: MemberId) extends SymbolAccessInfo {
      def addSymbolInferredType(tpe: Option[TypeInfo], kind: TypeInferenceKind = target)(debug: String*): Unit = {
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
      def unapply(arg: Node.Node): Option[SymbolAccessInfo] = arg match {
        case Node.Identifier(symDef) =>
          Some(SymbolAccessSymbol(symDef))

        case Node.Identifier(symDef) Node.Sub property =>
          //println(s"Node.Sub - ${symDef.name}")
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

        case expr Node.StaticMemberExpression name Node.Sub property =>
          //println(s"Node.StaticMemberExpression Node.Sub - $name $property")
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



        case expr Node.StaticMemberExpression name =>
          val clsId = memberId(classFromType(expressionType(expr)(ctx)), name)
          clsId.map(SymbolAccessDot)

        case _ =>
          None
      }
    }

    def inferConstructorCall(args: Seq[Node.Node], className: SymbolMapId) = {
      //println(s"Infer arg types for class $className")
      for (c <- classes.get(className)) {
        {
          val value = classInlineBody(c, transform.classes.classTokenSource(c))
          //println(s"  Constructor inline pars ${value.argnames.map(_.name)} args ${args.map(ScalaOut.outputNode(_))}")
          inferParsOrArgs(value.argnames, args)(s"Constructor call body $className")
        }

        for (Node.ConciseMethod(_, value: Node.Accessor) <- findConstructor(c)) {
          //println(s"  Constructor pars ${value.argnames.map(_.name)} args ${args.map(ScalaOut.outputNode(_))}")
          inferParsOrArgs(value.argnames, args)(s"Constructor call $className")
        }
      }
    }

    object KnownType {
      def unapply(arg: Node.Node)(implicit ctx: ExpressionTypeContext): Option[TypeInfo] = {
        val tpe = expressionType(arg)(ctx)
        //println(s"  check type of ${ScalaOut.outputNode(arg)} as $tpe")
        tpe
      }
    }
    object GetArrayType {
      def unapply(arg: Node.Node)(implicit ctx: ExpressionTypeContext): Option[TypeInfo] = {
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
      //println(s"${nodeClassName(node)} ${node.toLocaleString()}")
      descend(node, walker)

      node match {
        case Node.VarDef(Node.Identifier(symDef), Defined(right)) =>
          val log = watched(symDef.name)
          val symId = id(symDef)
          val leftT = n.types.get(symId)
          val rightT = expressionType(right, log)(ctx)


          if (log) println(s"Infer var $symId $leftT - $rightT `$node` $right")
          if (leftT != rightT) { // equal: nothing to infer (may be both None, or both same type)
            for (symInfo <- Some(SymbolAccessSymbol(symDef))) {
              if (log) println(s"  Infer var: $symInfo = $rightT")
              symInfo.addSymbolInferredType(rightT)(s"Infer var $symInfo = $rightT")
              if (log) println(s"  as: ${allTypes.get(symDef)}")
            }
            for (SymbolInfo(symInfo) <- Some(right)) {
              if (log) println(s"  Infer reverse var: $leftT = $symInfo")
              symInfo.addSymbolInferredType(leftT, source)(s"  Infer reverse var: $leftT = $symInfo")
            }
          }

        case Node.SymbolFunarg(Defined(symDef), _, Defined(JsArray(init))) =>
          if (n.types.get(symDef).isEmpty) {
            val tpe = expressionType(init)(ctx)
            addInferredType(symDef, tpe)(s"Node.SymbolFunarg ${symDef.name}")
          }

        // a few special forms of assignment should infer no type - cyclic dependencies
        case Node.Assign(SymbolInfo(symLeft), _, SymbolInfo(symRight)) if symLeft == symRight =>

        case Node.Assign(left, _, right) =>
          val log = left match {
            case Node.Identifier(symDef) if watched(symDef.name) => true
            case _ => false
          }
          val leftT = expressionType(left, log)(ctx)
          val rightT = expressionType(right, log)(ctx)
          //if (log) println(s"Infer assign $leftT - $rightT ${ScalaOut.outputNode(node)}")
          if (leftT != rightT) { // equal: nothing to infer (may be both None, or both same type)
            for (SymbolInfo(symInfo) <- Some(left)) {
              if (log) println(s"Infer assign: $symInfo = $rightT")
              symInfo.addSymbolInferredType(rightT)(s"  Infer assign: $symInfo = $rightT right $right")
            }
            for (SymbolInfo(symInfo) <- Some(right)) {
              if (log) println(s"Infer reverse assign: $leftT = $symInfo")
              symInfo.addSymbolInferredType(leftT, source)(s"  Infer reverse assign: $leftT = $symInfo left $left")
            }
          }

        case Node.BinaryExpression(SymbolInfo(symLeft), IsArithmetic(), SymbolInfo(symRight))
          if symLeft.unknownType(n.types) && symRight.unknownType(n.types) =>
          //println(s"Infer arithmetic: both unknown $symLeft $symRight")
          val numType = Some(TypeInfo.both(number))
          symLeft.addSymbolInferredType(numType)(s"Infer arithmetic: both unknown $symLeft $symRight")
          symRight.addSymbolInferredType(numType)(s"Infer arithmetic: both unknown $symLeft $symRight")

        case Node.BinaryExpression(SymbolInfo(symInfo), op, expr) if symInfo.unknownType(n.types) =>
          val tpe = typeFromOperation(op, expr)
          //println(s"Infer binary: left unknown $symInfo $tpe")
          symInfo.addSymbolInferredType(tpe)(s"Infer binary: left unknown $symInfo $tpe")

        case Node.BinaryExpression(expr, op, SymbolInfo(symInfo)) if symInfo.unknownType(n.types) =>
          val tpe = typeFromOperation(op, expr)
          //println(s"Infer binary: right unknown $symInfo $tpe")
          symInfo.addSymbolInferredType(tpe)(s"Infer binary: right unknown $symInfo $tpe")

        case Node.Switch(SymbolInfo(symInfo), body) =>
          var allCases = Option.empty[TypeInfo]
          body.foreach {
            case cc: Node.Case =>
              val cType = expressionType(cc.expression)(ctx)
              allCases = typeUnionOption(allCases, cType)
            case _ =>
          }
          symInfo.addSymbolInferredType(allCases)(s"Infer switch")

        case fun@Node.Defun(Defined(symDef), _, _) =>
          val allReturns = scanFunctionReturns(fun)(ctx)
          //println(s"${symDef.name} returns $allReturns")
          for {
            retType <- allReturns
            sd <- symDef.thedef
          } {
            // parameters do not matter here, they are infered as separate symbols
            val funType = FunctionType(retType.declType, IndexedSeq())
            addInferredType(sd, Some(TypeInfo.target(funType)))(s"Infer fun ${symDef.name}")
          }

        // TODO: derive getters and setters as well
        case Node.ConciseMethod(Node.SymbolName(sym), fun: Node.Lambda) =>
          val allReturns = scanFunctionReturns(fun)(ctx)
          // method of which class is this?
          val scope = findThisClass(fun.parent_scope.nonNull)
          for {
            retType <- allReturns
            Node.DefClass(Defined(Node.Identifier(cls)), _, _) <- scope
            clsId <- id(cls)
          } {
            //println(s"Infer return type for method ${cls.name}.$sym as $retType")
            val classId = MemberId(clsId, sym)
            val funType = FunctionType(retType.declType, IndexedSeq())
            addInferredMemberType(Some(classId), Some(TypeInfo.target(funType)))(s"Infer return type for method ${cls.name}.$sym as $retType")
          }
        case Node.ObjectGetter(Node.SymbolName(sym), fun: Node.Lambda) =>
          val allReturns = scanFunctionReturns(fun)(ctx)
          // method of which class is this?
          val scope = findThisClass(fun.parent_scope.nonNull)
          //println(s"Infer getter $sym as $allReturns")
          for {
            retType <- allReturns
            Node.DefClass(Defined(Node.Identifier(cls)), _, _) <- scope
            clsId <- id(cls)
          } {
            //println(s"Infer return type for getter ${cls.name}.$sym as $retType")
            val classId = MemberId(clsId, sym)
            addInferredMemberType(Some(classId), Some(retType))(s"Infer return type for getter ${cls.name}.$sym as $retType")
          }
        case Node.ObjectSetter(Node.SymbolName(sym), fun: Node.Lambda) =>
          // method of which class is this?
          val scope = findThisClass(fun.parent_scope.nonNull)
          //println(s"Infer setter $sym")

          for {
            arg <- fun.argnames.headOption
            retType <- allTypes.get(symbolFromPar(arg).flatMap(id))
            Node.DefClass(Defined(Node.Identifier(cls)), _, _) <- scope
            clsId <- id(cls)
          } {
            //println(s"Infer return type for setter $cls.$sym as $retType")
            val classId = MemberId(clsId, sym)
            // target, because setter parameter is typically used as a source for the property variable, which sets source only
            addInferredMemberType(Some(classId), Some(TypeInfo.target(retType.declType)))(s"Infer return type for setter $cls.$sym as $retType")
          }
        case Node.ObjectKeyVal(name, value) =>
          val scope = findThisClassInWalker(walker)
          for {
            Node.DefClass(Defined(Node.Identifier(cls)), _, _) <- scope
            clsId <- id(cls)
          } {
            val classId = MemberId(clsId, name)
            // target, because setter parameter is typically used as a source for the property variable, which sets source only
            val tpe = expressionType(value)(ctx)
            //println(s"Infer type for value $cls.$name as $tpe")
            addInferredMemberType(Some(classId), tpe)(s"Infer type for value $cls.$name as $tpe")
          }


        case Node.Call(Node.Identifier(call), args@_*) =>
          //println(s"Call ${call.name}")
          call.orig.headOption match {
            case Some(clazz: Node.SymbolDefClass) => // constructor call in the new Class(x)
              for {
                clsSym <- clazz.thedef.nonNull
                clsId <- id(clsSym)
              } {
                inferConstructorCall(args, clsId)
              }

            case Some(defunSym: Node.SymbolDefun) => // normal function call
              //println(s"Infer arg types for ${defunSym.name}")
              functions.get(defunSym) match {
                case Some(Node.Defun(_, pars, _)) =>
                  // now match arguments to parameters
                  inferParsOrArgs(pars, args)(s"Call function ${defunSym.name}")
                case _ =>
              }
            case Some(varSym: Node.SymbolVar) =>
              //println(s"Infer arg types for a var call ${varSym.name} as $tpe")
              varSym.thedef.foreach { td =>
                val log = watched(td.name)
                val tpe = inferFunction(args, log)
                addInferredType(td, Some(TypeInfo.target(tpe)))(s"Infer ${td.name} args $args")
              }
            // TODO: reverse inference
            case _ =>
          }
        case Node.Call(s: Node.Super, args@_*) =>
          for (sup <- findSuperClass(s.scope.nonNull)) {
            //println(s"Super call of $sup")
            inferConstructorCall(args, sup)
          }

        case Node.Call(expr Node.StaticMemberExpression call, args@_*) =>
          val exprType = expressionType(expr)(ctx)

          (exprType.map(_.declType),call,expr) match {
            case (Some(ArrayType(arrayElemType)), "push", SymbolInfo(sym)) =>
              if (args.nonEmpty) {
                val elemType = args.map(expressionType(_)(ctx)).reduce(typeUnionOption)
                sym.addSymbolInferredType(elemType.map(_.map(ArrayType)))(s"Array.push $sym elem: $elemType array: $arrayElemType")
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
                  val log = memberId.isWatched
                  val tpe = inferFunction(args, log)

                  //if (log) println(s"Infer par types for a member call $c.$call as $tpe")
                  //println(allTypes)
                  addInferredMemberType(Some(memberId), Some(TypeInfo.target(tpe)))(s"Infer par types for a member call $c.$call as $tpe") // target or source?

                  for (funType <- ctx.types.getMember(Some(memberId))) {
                    funType.declType match {
                      case ft: FunctionType =>
                        inferArgs(ft, args)(s"Function member $memberId")
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
                inferParsOrArgs(m.value.argnames, args)(s"Method call $callOn: $expr.$call offset ${expr.start.nonNull.map(_.pos)}")
              }
          }

        case SymbolInfo(symbol) Node.Sub property =>
          val tpe = symbol.tpe(ctx.types)
          //println(s"$symbol Node.Sub $property `$tpe` -> `${tpe.map(_.declType)}`")
          tpe.map(_.declType) match {
            case Some(ObjectOrMap) =>
              // initialized as {}, cannot be an Array, must be a map

              symbol.addSymbolInferredType(Some(TypeInfo.target(MapType(NoType))))(s"ObjectOrMap $symbol")
            case Some(_: MapType) =>
              // addressing map, we know index must be a string
              for (SymbolInfo(symbol) <- Some(property)) {
                val indexType = Some(TypeInfo.target(string))
                symbol.addSymbolInferredType(indexType)(s"Map $symbol.$property")
              }
            case Some(_: ArrayType) =>
              // addressing array, we know index must be a number
              for (SymbolInfo(symbol) <- Some(property)) {
                val indexType = Some(TypeInfo.target(number))
                symbol.addSymbolInferredType(indexType)(s"Array $symbol[$property]")
              }
              // once determined, do not change
            case _ =>
              expressionType(property)(ctx).map(_.declType) match {
                case Some(`number`) =>
                  symbol.addSymbolInferredType(Some(TypeInfo.target(ArrayType(NoType))))(s"Array index $symbol[$property]")
                case Some(`string`) =>
                  symbol.addSymbolInferredType(Some(TypeInfo.target(MapType(NoType))))(s"Map index $symbol[$property]")
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
