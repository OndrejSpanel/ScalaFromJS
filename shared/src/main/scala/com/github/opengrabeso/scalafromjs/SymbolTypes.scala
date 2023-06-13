package com.github.opengrabeso.scalafromjs

import scala.language.implicitConversions
import SymbolTypes._
import com.github.opengrabeso.esprima.Node.{FunctionParameter, FunctionParameterWithType}
import com.github.opengrabeso.scalafromjs.esprima.symbols
import com.github.opengrabeso.scalafromjs.esprima.Defined
import com.github.opengrabeso.scalafromjs.transform.TypesRule.typeInfoFromAST

import scala.collection.Seq
import scala.reflect.ClassTag

object SymbolTypes {

  val watch = false

  def watchCondition(cond: => Boolean): Boolean = if (watch) cond else false

  def watched(name: String): Boolean = watchCondition {
    val watched = Set[String]("context", "_gl")
    name.startsWith("watch_") || watched.contains(name)
  }

  def watchedMember(cls: String, name: String): Boolean = watchCondition {
    val watched = Set[(String, String)](
      //("WebGLUniformsGroups", "gl"),
      ("WebGLRenderer", "_gl"),
      ("WebGLRendererParameters", "context"),
      //("WebGLMorphtargets", "gl")
    )
    val watchedAllClasses = Set[String](
      "context"
    )
    name.startsWith("watch_") || watched.contains(cls, name) || watchedAllClasses.contains(name)
  }

  def watchedSym(sym: SymbolMapId): Boolean = {
    watched(sym.name)
  }

  implicit class IsWatched(name: String) {
    def isWatched: Boolean = watched(name)
  }

  implicit class IsWatchedMember(id: MemberId) {
    def isWatched: Boolean = watchedMember(id.cls.name, id.name)
  }

  trait TypeResolver {
    def resolveClass(t: ClassType): TypeDesc

    def recurse(t: TypeDesc, depth: Int)(f: TypeDesc => TypeDesc): TypeDesc = {
      assert(depth < 500)
      t match {
        case c: ClassType =>
          resolveClass(c)
        case ArrayType(a) =>
          ArrayType(recurse(a, depth + 1)(f))
        case MapType(a) =>
          MapType(recurse(a, depth + 1)(f))
        case UnionType(a) =>
          UnionType(a.map(recurse(_, depth + 1)(f)).distinct)
        case FunctionType(ret, args) =>
          FunctionType(recurse(ret, depth + 1)(f), args.map(recurse(_, depth + 1)(f)))
        case x =>
          f(x)
      }
    }

    def resolveType(t: TypeDesc): TypeDesc = {
      recurse(t, 0)(identity)
    }

    /** type resolution for output is more aggressive o*/
    def resolveTypeForOut(t: TypeDesc): TypeDesc = {
      recurse(t, 0)(_.normalizeForOut)
    }
  }

  sealed trait TypeDesc {
    def normalizeForOut: TypeDesc = this

    def scalaConstruct: String = "_"
    // should type be written explicitly when initializing a variable of this type?
    def typeOnInit: Boolean = true

    def knownItems: Int = 0

    def depthComplexity: Int = 0

    def acceptable: Boolean = depthComplexity <= 5

    def isSafeReplacementOf(that: TypeDesc): Boolean = {
      if (that ==  NoType || that == AnyType) {
        true
      } else {
        (this, that) match {
          case (ArrayType(thisElem), ArrayType(thatElem)) =>
            thisElem isSafeReplacementOf thatElem
          case (MapType(thisElem), MapType(thatElem)) =>
            thisElem isSafeReplacementOf thatElem
          case (FunctionType(thisRet, thisArgs), FunctionType(thatRet, thatArgs)) =>
            (thisRet isSafeReplacementOf thatRet) && (thisArgs zip thatArgs).forall(tt => tt._1 isSafeReplacementOf tt._2)
          case _ =>
            this == that
        }
      }
    }

    def toOut: String
  }
  case object ObjectOrMap extends TypeDesc {
    override def toOut = AnyType.toOut
    override def scalaConstruct: String = "new {}"
    override def typeOnInit = false
    override def knownItems = 0
  }

  // when any of the following types is used as a global class, it probably means we have failed to convert a TS type
  val forbiddenGlobalTypes = Set("Array", "number", "boolean", "string", "void", "any", "this")

  case class LiteralTypeDesc(value: Any) extends TypeDesc {
    override def toString = value.toString
    override def normalizeForOut: TypeDesc = {
      value match {
        case _: Int => number
        case _: Double => number
        case _: String => string
        case _: Boolean => boolean
        case _ => this
      }
    }
    override def toOut = value match {
      case x: String => s"\"$value\""
      case x => x.toString
    }

    override def knownItems = 1
  }

  case class SimpleType(name: String) extends TypeDesc {
    assert(!forbiddenGlobalTypes.contains(name))
    override def toString = name
    override def toOut = name

    override def knownItems = 1
  }
  case class ClassTypeEx(parents: Seq[String], name: SymbolMapId, typePars: Seq[TypeDesc] = Seq.empty) extends TypeDesc {
    if(forbiddenGlobalTypes.contains(name.name)) {
      throw new UnsupportedOperationException(s"Forbidden type name ${name.name}")
    }
    private def toSomething(x: TypeDesc => String) = {
      val baseName = (parents :+ name.name).mkString(".")
      if (typePars.isEmpty) {
        baseName
      } else {
        baseName + typePars.map(x).mkString("[", ",", "]")
      }
    }
    override def toString = toSomething(_.toString)
    override def toOut = toSomething(_.toOut)

    override def knownItems = 1

    def isSafeReplacement(source: TypeDesc): Boolean = source == this
  }
  type ClassType = ClassTypeEx
  object ClassType {
    def apply(name: SymbolMapId): ClassTypeEx = new ClassTypeEx(Seq.empty, name)
    def unapply(arg: ClassTypeEx) = ClassTypeEx.unapply(arg).map(_._2)
  }
  case class AnonymousClassType(sourcePos: (Int, Int)) extends TypeDesc {
    override def toString = s"anonymous_${sourcePos._1}.${sourcePos._2}"
    override def toOut = "AnyRef"
    override def scalaConstruct: String = "new {}"
    override def typeOnInit = false

    override def knownItems = 1

    def isSafeReplacement(source: TypeDesc): Boolean = source == this
  }

  case class ArrayType(elem: TypeDesc) extends TypeDesc {
    override def toString = s"Array[${elem.toString}]"
    override def toOut = s"Array[${elem.toOut}]"

    override def scalaConstruct: String = s"Array.empty[${elem.toOut}]"

    override def typeOnInit = false

    override def knownItems = 1 + elem.knownItems

    override def depthComplexity = elem.depthComplexity + 1

    def union(that: ArrayType)(implicit classOps: ClassOps) = ArrayType(typeUnion(elem, that.elem))
    def intersect(that: ArrayType)(implicit classOps: ClassOps) = ArrayType(typeIntersect(elem, that.elem))
  }

  case class MapType(elem: TypeDesc) extends TypeDesc {
    override def toString = s"Map[String, ${elem.toString}]"
    override def toOut = s"Map[String, ${elem.toOut}]"

    override def scalaConstruct: String = s"Map.empty[String, ${elem.toOut}]"

    override def typeOnInit = false

    override def knownItems = 1 + elem.knownItems

    override def depthComplexity = elem.depthComplexity + 1

    def union(that: MapType)(implicit classOps: ClassOps) = MapType(typeUnion(elem, that.elem))
    def intersect(that: MapType)(implicit classOps: ClassOps) = MapType(typeIntersect(elem, that.elem))
  }


  case class FunctionType(ret: TypeDesc, args: IndexedSeq[TypeDesc]) extends TypeDesc {

    //println(s"FunctionType ${args.mkString("(",",",")")} => $ret")
    override def knownItems = 1 + ret.knownItems + args.map(_.knownItems).sum

    override def depthComplexity = ret.depthComplexity + args.headOption.map(_ => args.map(_.depthComplexity).max).getOrElse(0)

    override def toString = {
      def outputType(o: TypeDesc) = o.toString
      args.map(outputType).mkString("(", ", ",")") + " => " + outputType(ret)
    }

    override def toOut = {
      def outputType(o: TypeDesc) = o.toOut
      args.map(outputType).mkString("(", ", ",")") + " => " + outputType(ret)
    }

    def op(that: FunctionType, parsCombine: (TypeDesc, TypeDesc) => TypeDesc, retCombine: (TypeDesc, TypeDesc) => TypeDesc)(implicit classOps: ClassOps) = {
      val ret = retCombine(this.ret, that.ret)
      val args = for ((a1, a2) <- this.args.zipAll(that.args, NoType, NoType)) yield {
        parsCombine(a1, a2)
      }
      FunctionType(ret, args)
    }

    def union(that: FunctionType)(implicit classOps: ClassOps) = op(that, typeUnion, typeIntersect)

    def intersect(that: FunctionType)(implicit classOps: ClassOps) = op(that, typeIntersect, typeUnion)
  }

  object UnionType {
    /** sometimes we create a union type of a single type intentionally, as we need it for some operation */
    def single(tpe: TypeDesc): UnionType = new UnionType(Seq(tpe))
    def apply(types: Seq[TypeDesc]): TypeDesc = {
      def mergeLiterals[Primitive: ClassTag](t: Seq[TypeDesc], merged: TypeDesc) = {
        if (t.contains(merged)) t.filterNot {
          case LiteralTypeDesc(_: Primitive) =>
            true
          case _ =>
            false
        } else t
      }
      def mergeDoubles(t: Seq[TypeDesc]) = mergeLiterals[Int](mergeLiterals[Double](t, number), number)
      def mergeStrings(t: Seq[TypeDesc]) = mergeLiterals[String](t, string)
      def mergeBooleans(t: Seq[TypeDesc]) = mergeLiterals[Boolean](t, boolean)

      val res = mergeBooleans(mergeStrings(mergeDoubles(types))).distinct
      if (res.sizeIs > 1) new UnionType(res)
      else if (res.isEmpty) NoType
      else res.head
    }
  }
  case class UnionType(types: Seq[TypeDesc]) extends TypeDesc {
    override def knownItems = 1

    override def normalizeForOut: TypeDesc = {
      val ts = types.distinct
      if (ts.sizeIs == 1) ts.head
      else UnionType(ts)
    }

    // distinct because there may be e.g. several anonymous classes which are output as AnyRef
    override def toOut = types.map(_.normalizeForOut.toOut).distinct.mkString(" | ")
    override def toString = types.map(_.toString).mkString(" | ")

    override def depthComplexity = types.map(_.depthComplexity).max

    def union(that: UnionType): TypeDesc = {
      UnionType(types ++ that.types)
    }
    def intersect(that: UnionType): TypeDesc = {
      // TODO: try smart class intersection using common base
      val tpe = types.intersect(that.types)
      if (tpe.isEmpty) NoType
      else if (tpe.size == 1) tpe.head
      else UnionType(tpe)
    }
  }

  case object AnyType extends TypeDesc { // supertype of all
    override def toString = "Any"
    override def toOut = "Any"

  }
  case object NoType extends TypeDesc { // subtype of all
    override def toString = "Unit"
    override def toOut = "Unit"
  }
  case object NothingType extends TypeDesc { // subtype of all
    override def toString = "Nothing"
    override def toOut = "Nothing"
  }
  case object NullType extends TypeDesc {
    override def toString = "Null"
    override def toOut = "Null"
  }

  val any = AnyType
  val number = SimpleType("Double")
  val boolean = SimpleType("Boolean")
  val string = SimpleType("String")

  def parseType(str: String): Option[TypeDesc] = {
    str match {
      case "number" => Some(number)
      case "boolean" => Some(boolean)
      case "string" => Some(string)
      case "any" => Some(any)
      case _ => None
    }
  }

  // SymbolDef instances (including ids) are recreated on each figure_out_scope
  // we need a stable id. Original source location + name should be unique and stable
  type SymbolMapId = esprima.symbols.SymId
  val SymbolMapId = esprima.symbols.SymId

  case class MemberId(cls: SymbolMapId, name: String)

  def memberId(maybeDesc: Option[SymbolMapId], name: String): Option[MemberId] = {
    maybeDesc.map(MemberId(_, name))
  }

  implicit def id(sym: SymbolMapId): Option[SymbolMapId] = {
    Some(sym)
  }

  def classFromType(tpe: Option[TypeInfo]): Option[SymbolMapId] = {
    tpe.map(_.declType) match {
      case Some(ClassType(name)) => Some(name)
      case Some(AnonymousClassType(sourcePos)) => Some(SymbolMapId(":anonymous", sourcePos))
      case _ => None
    }
  }

  trait ClassOps extends TypeResolver {
    def mostDerived(c1: ClassType, c2: ClassType): TypeDesc
    def commonBase(c1: ClassType, c2: ClassType): TypeDesc
  }

  object ClassOpsDummy extends ClassOps {
    def mostDerived(c1: ClassType, c2: ClassType): TypeDesc = c1
    def commonBase(c1: ClassType, c2: ClassType): TypeDesc = c2
    def resolveClass(c: ClassType): TypeDesc = c
  }

  object ClassOpsUnion extends ClassOps {
    def mostDerived(c1: ClassType, c2: ClassType) = AnyType
    def commonBase(c1: ClassType, c2: ClassType) = UnionType(Seq(c1, c2))
    def resolveClass(c: ClassType): TypeDesc = c
  }

  object IsObject {
    def unapply(t: TypeDesc) = {
      t match {
        case _: AnonymousClassType =>
          true
        case ObjectOrMap =>
          true
        case _ =>
          false
      }
    }
  }

  def typeIntersect(tpe1Input: TypeDesc, tpe2Input: TypeDesc)(implicit classOps: ClassOps): TypeDesc = {
    typeIntersect(tpe1Input, tpe2Input, 0)(classOps)
  }

    // intersect: assignment source
  def typeIntersect(tpe1Input: TypeDesc, tpe2Input: TypeDesc, depth: Int)(implicit classOps: ClassOps): TypeDesc = {
    assert(depth < 500)
    val tpe1 = classOps.resolveType(tpe1Input)
    val tpe2 = classOps.resolveType(tpe2Input)
    val r = (tpe1, tpe2) match {
      case _ if tpe1 == tpe2 =>
        tpe1
      case (t, AnyType) => t
      case (AnyType, t) => t
      case (t, NoType) => t // while technically incorrect, we always prefer some types against no type
      case (NoType, t) => t
      case (c1: ClassType, c2: ClassType) =>
        classOps.mostDerived(c1, c2)
      case (f1: FunctionType, f2: FunctionType) =>
        f1 intersect f2
      case (a1: ArrayType, a2: ArrayType) =>
        //println(s"a1 intersect a2 $a1 $a2")
        a1 intersect a2
      case (a1: MapType, a2: MapType) =>
        //println(s"a1 intersect a2 $a1 $a2")
        a1 intersect a2
      case (u1: UnionType, u2: UnionType) =>
        u1 intersect u2
      case (u: UnionType, t: TypeDesc) =>
        u intersect UnionType.single(t)
      case (t: TypeDesc, u: UnionType) =>
        u intersect UnionType.single(t)
      case (a: MapType, IsObject()) => a
      case (IsObject(), a: MapType) => a
      case (c1: ClassType, _) => // while technically incorrect, we always prefer a class type against a non-class one
        c1
      case (_, c2: ClassType) =>
        c2
      case _ =>
        NoType // should be Nothing, but we rather keep any of the types
    }
    //println(s"  typeIntersect $tpe1, $tpe2 = $r")
    r
  }

  def mapFromArray(a: ArrayType): MapType = MapType(a.elem)

  def findArrayTypes(t: Seq[TypeDesc]) = {
    val arrayTypes = t.collect {
      case ArrayType(elem) => elem
    }
    val otherTypes = t diff arrayTypes.map(ArrayType)
    (arrayTypes, otherTypes)
  }

  def unionManyTypes(uTypesInput: Seq[TypeDesc])(implicit classOps: ClassOps): TypeDesc = {
    // merge literal types with their normal counterparts
    val uTypes = uTypesInput.filterNot(_ == NoType).distinct
    if (uTypes.isEmpty) NoType
    else if (uTypes.contains(AnyType)) AnyType
    else if (uTypes.size == 1) uTypes.head
    else {
      val (arrayTypes, otherTypes) = findArrayTypes(uTypes)

      if (arrayTypes.size > 1) { // WIP: merge Array, ArrayLike, Iterable together
        // handle a special case: Array[S]|S or Array[Unit]|S, which is most likely a result of false recursion
        // use just S instead
        val nonRecursive = arrayTypes.map {
          case UnionType(Seq(ArrayType(a), b)) if a == b || a == NoType =>
            a
          case UnionType(Seq(b, ArrayType(a))) if a == b || a == NoType =>
            a
          case x =>
            x
        }

        val aType = nonRecursive.reduce(typeUnion(_, _))
        UnionType(ArrayType(aType) +: otherTypes)
      } else {
        if (uTypes.size > 4) {
          // WIP: use commonBase for all class types
          AnyType
        } else {
          UnionType(uTypes)
        }
      }
    }
  }

  def normalizeType(tpe: TypeDesc): TypeDesc = {
    tpe match {
      case u: UnionType =>
        val innerTypes = u.types.map(normalizeType)
        val (unions, other) = innerTypes.partition(_.isInstanceOf[UnionType])
        val unionsContent = unions.map(_.asInstanceOf[UnionType]).flatMap(_.types)
        UnionType(unionsContent ++ other)
      case x =>
        x
    }
  }

  // union: assignment target
  def typeUnion(tpe1Input: TypeDesc, tpe2Input: TypeDesc)(implicit classOps: ClassOps): TypeDesc = {
    // special case optimization for a very common case
    if (tpe1Input == tpe2Input) return tpe1Input
    object UndefinedOrNullType {
      def unapply(t: TypeDesc): Boolean = {
        t match {
          case ClassType(id) if id.name == "undefined" || id.name == "null" =>
            true
          case NullType =>
            true
          case _ =>
            false
        }
      }
    }
    val tpe1 = classOps.resolveType(tpe1Input)
    val tpe2 = classOps.resolveType(tpe2Input)
    (tpe1, tpe2) match {
      case _ if tpe1 == tpe2 =>
        tpe1
      case (UndefinedOrNullType(), t) => t
      case (t, UndefinedOrNullType()) => t
      case (_, AnyType) => AnyType
      case (AnyType, _) => AnyType
      case (t, NoType) => t
      case (NoType, t) => t
      case (IsObject(), c: ClassType) => c
      case (c: ClassType, IsObject()) => c
      case (c1: ClassType, c2: ClassType) =>
        classOps.commonBase(c1, c2)
      case (c: ClassType, SimpleType("Double")) =>
        c // TODO: detect if c is enum and return AnyType or UnionType if it is not
      case (SimpleType("Double"), c: ClassType) =>
        c // TODO: detect if c is enum and return AnyType or UnionType if it is not
      case (f1: FunctionType, f2: FunctionType) =>
        f1 union f2
      case (a1: ArrayType, a2: ArrayType) =>
        a1 union a2
      case (a1: ArrayType, a2: MapType) =>
        mapFromArray(a1) union a2 // what looks like array can be actually a map, as array access is possible on maps
      case (a1: MapType, a2: ArrayType) =>
        a1 union mapFromArray(a2)
      case (a1: MapType, a2: MapType) => a1 union a2
      case (IsObject(), a: MapType) => a
      case (a: MapType, IsObject()) => a
      case (ObjectOrMap, a: ArrayType) => a
      case (a: ArrayType, ObjectOrMap) => a
      case (u1: UnionType, u2: UnionType) =>
        unionManyTypes(u1.types ++ u2.types)
      case (u: UnionType, t: TypeDesc) =>
        unionManyTypes(u.types :+ t)
      case (t: TypeDesc, u: UnionType) =>
        unionManyTypes(t +: u.types)
      case (a: TypeDesc, b: TypeDesc) =>
        unionManyTypes(Seq(a, b))
      case _ =>
        AnyType
    }
  }

  private def typeFromOption(tpe: Option[TypeInfo]) = {
    tpe.getOrElse(TypeInfo.unknown)
  }
  def typeUnionOption(tpe1: Option[TypeInfo], tpe2: Option[TypeInfo])(implicit classOps: ClassOps): Option[TypeInfo] = {
    val t1 = typeFromOption(tpe1)
    val t2 = typeFromOption(tpe2)
    val union = typeUnion(t1.target, t2.target)
    //println(s"  union $t1 $t2 -> $union")
    Some(t1.copy(target = union, certain = false))
  }

  def typeIntersectOption(tpe1: Option[TypeInfo], tpe2: Option[TypeInfo])(implicit classOps: ClassOps): Option[TypeInfo] = {
    val t1 = typeFromOption(tpe1)
    val t2 = typeFromOption(tpe2)
    val srcType = typeIntersect(t1.declType, t2.declType)
    //println(s"  intersect $t1 $t2 -> ${t1.declType} x ${t2.declType} = $srcType")
    Some(t1.copy(source = srcType, certain = false))
  }

  def apply(): SymbolTypes = SymbolTypes.std
  def apply(syms: Seq[(SymbolMapId, TypeDesc)]) = {
    val idMap = syms.map { case (k,v) => id(k) -> v }.toMap - None
    new SymbolTypes(StdLibraries(), idMap.map{ case (k, v) => k.get -> TypeInfo.target(v)})
  }

  case class ClassInfo(members: Map[SymbolMapId, Seq[String]] = Map.empty, parents: Map[SymbolMapId, SymbolMapId] = Map.empty) {

    override def toString: String = {
      // useful for faster debugging
      s"ClassInfo(members:${members.size},parents:${parents.size})"
    }

    lazy val children = parents.groupBy(_._2).view.mapValues(_.keys.toSet).toMap

    //println(s"parents $parents")
    //println(s"children $children")
    //println(s"listChildren ${listChildren("X")}")

    def containsMember(cls: SymbolMapId, member: String): Boolean = {
      members.get(cls).exists(_.contains(member))
    }

    def classContains(cls: SymbolMapId, member: String): Option[SymbolMapId] = {
      val r = if (containsMember(cls, member)) Some(cls)
      else parents.get(cls).flatMap { c =>
        //println(s"  parent $c")
        classContains(c, member)
      }
      //println(s"Check $cls contains $member: $r")
      r
    }

    // list parents, the first in the list is the hierarchy root (no more parents), the last is the class itself
    def listParents(cls: SymbolMapId): Seq[SymbolMapId] = {
      def listParentsRecurse(cls: SymbolMapId, ret: Seq[SymbolMapId]): Seq[SymbolMapId] = {
        val p = parents.get(cls)
        p match {
          case Some(pp) => listParentsRecurse(pp, pp +: ret)
          case None => ret
        }
      }

      listParentsRecurse(cls, Seq(cls))
    }

    // list all children in no particular order
    def listChildren(cls: SymbolMapId): Set[SymbolMapId] = {
      def listChildrenRecurse(cls: SymbolMapId): Set[SymbolMapId] = {
        val p = children.get(cls)
        p match {
          case Some(pp) =>
            //println(s"  Some $pp")
            Set(cls) ++ pp.flatMap(listChildrenRecurse)
          case None =>
            //println(s"  None $cls")
            Set(cls)
        }
      }

      listChildrenRecurse(cls)
    }



    def mostDerived(c1: SymbolMapId, c2: SymbolMapId): Option[SymbolMapId] = {
      //println(s"  Parents of $c1: ${listParents(c1)}")
      //println(s"  Parents of $c2: ${listParents(c2)}")
      // check if one is parent of the other
      if (listParents(c1) contains c2) Some(c1)
      else if (listParents(c2) contains c1) Some(c2)
      else None
    }

    def commonBase(c1: SymbolMapId, c2: SymbolMapId): Option[SymbolMapId] = {
      val p1 = listParents(c1)
      val p2 = listParents(c2)
      (p1 zip p2).takeWhile(p => p._1 == p._2).lastOption.map(_._1)
    }

    def ++ (that: ClassInfo) = ClassInfo(this.members ++ that.members, this.parents ++ that.parents)

  }

  val libs = Map(
    // -1 is a special handling for global symbols
    SymbolMapId("Math", (-1, -1)) -> Seq( // TODO: consider using a predefined header instead
      "min", "max", "abs", // TODO: all these are static members
      "sin", "cos", "tan", "asin", "acos", "atan",
      "sqrt", "ceil", "floor",
      "round"
    )
  )
  val libNames = libs.keys.toSeq

  private val numberFunction = FunctionType(number, IndexedSeq(number))

  val stdLibraries: Map[SymbolMapId, TypeInfo] = libs.keys.map { k =>
    k -> TypeInfo.target(ClassType(k))
  }.toMap

  val stdLibraryMembers: Map[MemberId, TypeInfo] = (for {
    (cls, members) <- libs.toSeq
    member <- members
  } yield {
    MemberId(cls, member) -> TypeInfo.target(numberFunction) // TODO: type data driven
  }).toMap


  // unique ID for std lib classes
  // once ClassType contains offset uniquely identifying a class, this class can be deleted
  case class StdLibraries(libs: Seq[SymbolMapId] = Seq.empty) {
    val base = -10000
    val index = libs.zipWithIndex.map {case (v, i) => v -> (base - i)}.toMap

    def symbolFromClass(cls: SymbolMapId): SymbolMapId = {
      val id = index.get(cls)
      id.fold(cls)(c => cls.copy(sourcePos = c -> c))
    }

    def symbolFromMember(cls: SymbolMapId, name: String): Option[SymbolMapId] = {
      val id = index.get(cls)
      id.map(i => SymbolMapId(name, i -> i))
    }
  }

  val stdLibs = StdLibraries(libNames)

  val stdLibraryMemberSymbols: Map[SymbolMapId, TypeInfo] = stdLibraryMembers.toSeq.zipWithIndex.flatMap {case ((k,v), index) =>
    stdLibs.symbolFromMember(k.cls, k.name).map(_ -> v)
  }.toMap

  lazy val std: SymbolTypes = SymbolTypes(stdLibs, stdLibraries ++ stdLibraryMemberSymbols)

  lazy val stdClassInfo: ClassInfo = ClassInfo(libs, Map.empty)

  def isMemberCall(kind: String): Boolean = {
    kind != "value" && kind != "get" && kind != "set"
  }
}


object TypeInfo {
  def source(tpe: TypeDesc): TypeInfo = {
    //println(s"source $tpe")
    TypeInfo(tpe, NoType)
  }
  def target(tpe: TypeDesc): TypeInfo = {
    //println(s"target $tpe")
    TypeInfo(AnyType, tpe)
  }
  def both(tpe: TypeDesc): TypeInfo = {
    //println(s"both $tpe")
    TypeInfo(tpe, tpe)
  }
  def certain(tpe: TypeDesc): TypeInfo = {
    //println(s"both $tpe")
    TypeInfo(tpe, tpe, true)
  }
  def unknown: TypeInfo = TypeInfo(AnyType, NoType)

}

/**
  *
  * @param source inferred type when used as an assignment source
  * @param target inferred type when used as an assignment target
  * @param certain types imported from d.ts can never be overridden
  */
case class TypeInfo(source: TypeDesc, target: TypeDesc, certain: Boolean = false) {
  assert(!certain || source == target) // when certain, source and target need to be the same
  def equivalent(tp: TypeInfo): Boolean = source == tp.source && target == tp.target

  def knownItems = source.knownItems max target.knownItems

  def nonEmpty = source != AnyType || target != NoType
  def acceptable = source.acceptable && target.acceptable

  def known = source != AnyType && source!=NoType || target != NoType && target != AnyType

  def isSafeReplacementOf(that: TypeInfo): Boolean = {
    source.isSafeReplacementOf(that.source) && target.isSafeReplacementOf(that.target)
  }

  //assert(source == AnyType || target == NoType)
  // source should be more specific than a target (target is a supertype, source a subtype)
  // ... target is an upper bound, source a lower bound
  //assert(typeIntersect(source, target) == source)
  //assert(typeUnion(source, target) == target)

  def declType: TypeDesc = (source, target) match {
    case (_, NoType) => source
    case (AnyType, _) => target
    case _ => typeUnion(source, target)(ClassOpsDummy)
  }

  def sourceTypeFromTarget: TypeDesc = target match {
    case NoType => AnyType
    case AnyType => NoType
    case _ => target
  }

  def sourceFromTarget: TypeInfo = TypeInfo(sourceTypeFromTarget, NoType, certain) // TODO: certain should not be here

  def map(f: TypeDesc => TypeDesc): TypeInfo = TypeInfo(f(source), f(target), certain) // TODO: certain should not be here
}

sealed trait Hint

case object IsConstructorParameter extends Hint

/**
  Once locked, perform only "desperate" inference: i.e. from Unit to some type, never change a variable type
*/

case class SymbolTypes(stdLibs: StdLibraries, types: Map[SymbolMapId, TypeInfo], hints: Map[SymbolMapId, Hint] = Map.empty, locked: Boolean = false)
  extends SymbolTypes.TypeResolver {

  def get(id: SymbolMapId): Option[TypeInfo] = types.get(id)
  def getHint(id: SymbolMapId): Option[Hint] = hints.get(id)

  def get(id: Option[SymbolMapId]): Option[TypeInfo] = id.flatMap(types.get)
  def getHint(id: Option[SymbolMapId]): Option[Hint] = id.flatMap(hints.get)

  override def resolveClass(t: ClassType) = {
    // never resolve to an anonymous class - prefer unresolved named class instead
    types.get(t.name).map(_.declType).filterNot { t =>
      // (resolving to an anonymous class caused a test failure for "d.ts enum conversion"
      t.isInstanceOf[AnonymousClassType] ||
      // resolving to Any or Unit is almost never what we want
      t == AnyType || t == NoType
    }.getOrElse(t)
  }

  def getResolved(id: Option[SymbolMapId]): Option[TypeInfo] = {
    id.flatMap(types.get).map { ti =>
      TypeInfo(resolveType(ti.source), resolveType(ti.target), ti.certain)
    }
  }

  def renameHint(oldId: SymbolMapId, newId: SymbolMapId): SymbolTypes = {
    val mod = for (hint <- hints.get(oldId)) yield {
      var newHints = hints - oldId + (newId -> hint)
      copy(hints = newHints)
    }
    mod.getOrElse(this)
  }

  // TODO: move stdLibs and symbolFromMember out of SymbolTypes
  def symbolFromMember(memberId: MemberId)(implicit classPos: SymbolMapId => (Int, Int)): SymbolMapId = {
    // first check stdLibraries, if not found, try normal lookup
    stdLibs.symbolFromMember(memberId.cls, memberId.name).getOrElse {
      val clsPos = classPos(memberId.cls)
      SymbolMapId(memberId.name, clsPos)
    }
  }

  def getMember(clsId: Option[MemberId])(implicit classId: SymbolMapId => (Int, Int)): Option[TypeInfo] = get(clsId.map(symbolFromMember))

  def getAsScala(id: Option[SymbolMapId]): String = {
    get(id).fold (any.toOut) (t => t.declType.toOut)
  }

  def ++ (that: SymbolTypes): SymbolTypes = SymbolTypes(stdLibs, types ++ that.types, hints ++ that.hints, locked || that.locked)

  def add(kv: (SymbolMapId, TypeInfo)): SymbolTypes = {
    //assert(!kv._1.isGlobal)

    val id = kv._1
    if (watched(id.name)) {
      if (types.get(id).exists(_.equivalent(kv._2))) {
        println(s"++ Watched $id type == ${kv._2}")
      } else {
        println(s"++ Watched $id type ${kv._2}")
      }
    }
    copy(types = types + kv)
  }

  def remove(id: SymbolMapId): SymbolTypes = {
    if (watched(id.name)) {
      println(s"-- Watched $id")
    }
    copy(types = types - id)
  }

  def + (kv: (Option[SymbolMapId], TypeInfo)): SymbolTypes = {
    // not generally true, but can be useful during debugging to catch symbols without a proper scope
    //assert(kv._1.forall(!_.isGlobal))

    kv._1.fold(this)(k => add(k -> kv._2))
  }

  def addHint(kv: (Option[SymbolMapId], Hint)): SymbolTypes = {
    kv._1.fold(this)(id => copy(hints = hints + (id -> kv._2)))
  }


  def addMember(kv: (Option[MemberId], TypeInfo))(implicit classId: SymbolMapId => (Int, Int)): SymbolTypes = {
    //assert(kv._1.forall(!_.cls.isGlobal))
    val symId = kv._1.map(symbolFromMember)
    this + (symId -> kv._2)
  }


  def knownItems: Int = {
    def sumTypeInfo(types: Iterable[TypeInfo]) = types.foldLeft(0)((s, i) => s + i.knownItems)
    sumTypeInfo(types.values)
  }

  override def toString: String = if (types.size < 20) {
    // useful for faster debugging
    types.mkString(s"SymbolTypes(locked=$locked,types:\n{","\n", ")}")
  } else {
    s"SymbolTypes(locked=$locked,types:${types.size})"
  }

  def getParameterTypes(dtsPars: Seq[FunctionParameter])(implicit context: symbols.ScopeContext) = {
    // match parameters by position, their names may differ
    dtsPars.map {
      case FunctionParameterWithType(_, Defined(t), defValue, optional) =>
        typeInfoFromAST(t)(context)
      case _ =>
        None
    }
  }

  /**
    * Read parameter types from AST
    * */

  def handleParameterTypes
    (symId: SymbolMapId, tt: Option[TypeDesc], astPars: Seq[FunctionParameter], dtsPars: Seq[FunctionParameter], scopeId: symbols.ScopeContext.ScopeId)
    (implicit context: symbols.ScopeContext): SymbolTypes = {
    var types = this.types

    val funName = symId.name
    // scopeId is a scope of the function node used for the parameters
    // this is different from the scope in which the function identifier is defined (in symId)
    // in some cases it may be the same, though, like for synthetic functions

    val parNames = astPars.map(Transform.nameFromPar)
    def inferParType(pjs: String, tt: TypeInfo) = {
      val id = symbols.SymId(pjs, scopeId)
      val log = watched(pjs)
      if (log) println(s"Set from d.ts $id $tt")
      types += id -> tt
      tt
    }
    val parTypes = if (astPars.size == dtsPars.size) {
      // match parameters by position, their names may differ
      val parTypes = getParameterTypes(dtsPars)
      for {
        (pjs, tt) <- parNames zip parTypes
      } yield {
        pjs.flatMap(p => tt.map(t => inferParType(p, t)))
      }
    } else { // the signature is wrong, we need to guess
      for {
        pn <- parNames
      } yield {
        for {
          pjs <- pn
          pd <- dtsPars.find(p => Transform.nameFromPar(p).contains(pjs))
          t <- Transform.typeFromPar(pd)
          tt <- typeInfoFromAST(t)(context)
        } yield {
          inferParType(pjs, tt)
        }
      }
    }

    // an alternative implementation could store a function type as a type of the member
    // in current implementation we store the result type. Parameter types are stored for the parameter identifiers
    //FunctionType(tt, parTypes.map(_.map(_.declType).getOrElse(AnyType)).toIndexedSeq)

    for (t <- tt) {
      if (watched(funName)) println(s"Set from d.ts $symId $tt")
      types += symId -> TypeInfo.certain(t)
    }

    this.copy(types = types)
  }

}
