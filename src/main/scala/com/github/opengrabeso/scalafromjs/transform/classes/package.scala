package com.github.opengrabeso.scalafromjs
package transform

import Transform._
import Classes._
import com.github.opengrabeso.scalafromjs.esprima._
import com.github.opengrabeso.esprima._
import JsUtils._
import SymbolTypes.SymbolMapId
import com.github.opengrabeso.scalafromjs.esprima.symbols.{Id, ScopeContext, SymId}

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.language.implicitConversions
import scala.util.matching.Regex

package object classes {

  type ClassId = SymbolMapId

  object ClassId {
    // TODO: avoid get, use something safe instead
    def apply(sym: SymId): ClassId = sym
    def apply(name: String)(implicit context: ScopeContext): ClassId = Id(name)
    def apply(sym: Node.Identifier)(implicit context: ScopeContext): ClassId = ClassId(Id(sym))
  }

  object ClassDefineValue {
    // function C() {return {a: x, b:y}
    def unapply(arg: DefFun) = arg match {
      case DefFun(sym, args, Node.BlockStatement(body :+ Node.ReturnStatement(OObject(proto))), _) =>
        Some(sym, args, body, proto)
      case _ =>
        None
    }

  }
  object ClassDefine {
    def unapply(arg: Node.Node)(implicit context: ScopeContext): Option[(Node.Identifier, Seq[Node.FunctionParameter], Seq[Node.Statement])] = arg match {
      // function ClassName() {}
      case DefFun(sym, args, body, _) =>
        Some(sym, args, Block.statements(body))

      // ClassName = function() {}
      case Assign(sym: Node.Identifier, "=", AnyFun(args, body)) =>
        Some(sym, args, Block.statements(body))

      // var ClassName = function() {}
      case VarDecl(sym, AnyFun(args, body), _) =>
        Some(Node.Identifier(sym), args, Block.statements(body))

      case _ =>
        //println(nodeClassName(arg))
        None
    }
  }

  sealed trait ClassMember {
    def definedFrom(init: Node.Node): Boolean
  }

  case class ClassFunMember(args: Seq[Node.FunctionParameter], body: Seq[Node.StatementListItem]) extends ClassMember {
    def definedFrom(init: Node.Node) = init match {
      case func: Node.FunctionExpression =>
        // reference equality of the first member is enough, nodes are unique
        //println(s"Defined func from: ${ScalaOut.outputNode(body.head)} ${ScalaOut.outputNode(func.body.head)}")
        func.body.body.head == body.head
      case _ => false
    }
  }

  case class ClassVarMember(value: Node.Expression) extends ClassMember {
    def definedFrom(init: Node.Node) = {
      //println(s"Defined value from: ${ScalaOut.outputNode(value)} ${ScalaOut.outputNode(init)}")
      value == init
    }
  }

  case class ClassDef(
    base: Option[ClassId] = None,
    // members (both data and function)
    members: ListMap[String, ClassMember] = ListMap.empty,
    // value properties
    values: ListMap[String, ClassVarMember] = ListMap.empty,
    getters: ListMap[String, ClassFunMember] = ListMap.empty,
    setters: ListMap[String, ClassFunMember] = ListMap.empty,
    // static members (both data and functions)
    membersStatic: ListMap[String, ClassMember] = ListMap.empty,
    // when empty, no need to emit class, only object - no JS corresponding constructor exists
    staticOnly: Boolean = false
  ) {
    def addMember(name: String, m: ClassMember) = copy(members = members + (name -> m))
    def addValue(name: String, m: ClassVarMember) = copy(values = values + (name -> m))
    def renameMember(name: String, newName: String) = copy(members = members.map { case (k, v) =>
      if (k == name) newName -> v else k -> v
    })
  }

  object ClassPropertyDef {
    def unapply(arg: Node.Node)(implicit context: ScopeContext) = arg match {
      case Node.ExpressionStatement(Assign(Node.Identifier(symDef) Dot "prototype" Dot funName, "=", value)) =>
        Some(ClassId(symDef), funName, value)
      case _ => None
    }
  }

  object ClassMemberDef {
    def unapply(arg: Node.Node)(implicit context: ScopeContext) = arg match {
      case ClassPropertyDef(name, funName, AnyFun(args, body)) =>
        Some(name, funName, args, body)
      case _ => None
    }
  }

  object DefineProperties {

    object DefinePropertiesObject {
      def unapply(arg: Node.Node)(implicit context: ScopeContext): Option[(ClassId, Node.ArgumentListElement)] = arg match {
        case Node.ExpressionStatement(Node.CallExpression(Node.Identifier("Object") Dot "defineProperties",
          Seq(Node.Identifier(Id(symDef)) Dot "prototype", properties))) =>
          Some(ClassId(symDef), properties)

        case Node.ExpressionStatement(Node.CallExpression(Node.Identifier("Object") Dot "defineProperties",
          Seq(Node.Identifier(Id(symDef)), properties))) =>
          Some(ClassId(symDef), properties)

        case _ => None
      }
    }

    def unapply(arg: Node.Node)(implicit context: ScopeContext) = arg match {
      case DefinePropertiesObject(name, OObject(properties)) =>
        Some(name, properties)
      case _ =>
        None

    }
  }

  object DefineProperty {
    def unapply(arg: Node.Node)(implicit context: ScopeContext) = arg match {
      // Object.defineProperty(XXXX.prototype, "name", {...} //
      case Node.ExpressionStatement(Node.CallExpression(
      Node.Identifier("Object") Dot "defineProperty",
      Seq(Node.Identifier(sym) Dot "prototype",
      StringLiteral(prop),
      OObject(properties)))) =>
        Some(ClassId(sym), prop, properties)
      case _ => None

    }
  }

  object DefineStaticMember {
    def unapply(arg: Node.Node)(implicit context: ScopeContext) = arg match {
      // Cls.defX = 0;
      // Cls.defY = function() {return 0;};
      case Node.ExpressionStatement(Assign(Node.Identifier(Id(clsSym)) Dot member, "=", value)) =>
        if (member == "prototype") {
          None
        } else {
          Some(clsSym, member, value)
        }
      case _ => None
    }
  }

  object ClassParentAndPrototypeDef {
    def unapply(arg: Node.ExpressionStatement)(implicit context: ScopeContext) = arg match {
      // name.prototype = Object.assign( Object.create( sym.prototype ), {... prototype object ... } )
      case Node.ExpressionStatement(Assign(
        Node.Identifier(name) Dot "prototype", "=",
        Node.CallExpression(
          Node.Identifier("Object") Dot "assign",
          Seq(Node.CallExpression(Node.Identifier("Object") Dot "create", Seq(Node.Identifier(sym) Dot "prototype")), prototypeDef: OObject)
        )
      )) =>
        //println(s"ClassParentAndPrototypeDef $name extends ${sym.name}")
        Some(ClassId(name), ClassId(sym), prototypeDef)

      // Object.assign( name.prototype, sym.prototype, {prototype object} )
      case Node.ExpressionStatement(Node.CallExpression(
        Node.Identifier("Object") Dot "assign",
        Seq(Node.Identifier(name) Dot "prototype",
          Node.Identifier(sym) Dot "prototype",
          prototypeDef: OObject
      ))) =>
        //println(s"ClassParentAndPrototypeDef2 $name extends $sym")
        Some(ClassId(name), ClassId(sym), prototypeDef)
      case _ =>
        None
    }

  }

  object ClassParentDef {
    def unapply(arg: Node.ExpressionStatement)(implicit context: ScopeContext) = arg match {
      // name.prototype = new sym.prototype
      case Node.ExpressionStatement(Assign(Node.Identifier(name) Dot "prototype", "=", Node.NewExpression(Node.Identifier(sym), _))) =>
        Some(ClassId(name), sym)

      // name.prototype = Object.create( sym.prototype );
      case Node.ExpressionStatement(Assign(
      Node.Identifier(name) Dot "prototype", "=",
      Node.CallExpression(Node.Identifier("Object") Dot "create", Seq(Node.Identifier(sym) Dot "prototype"))
      )) =>
        Some(ClassId(name), sym)

      // Object.assign( name.prototype, sym.prototype)
      case Node.ExpressionStatement(Node.CallExpression(
      Node.Identifier("Object") Dot "assign",
      Seq(Node.Identifier(name) Dot "prototype",
      Node.Identifier(sym) Dot "prototype"
      ))) =>
        Some(ClassId(name), sym)

      case _ => None
    }
  }

  object ClassPrototypeDef {
    def unapply(arg: Node.Node)(implicit context: ScopeContext) = arg match {

      //Object.assign( XXX.prototype, { ... })
      case Node.ExpressionStatement(Node.CallExpression(
      Node.Identifier("Object") Dot "assign",
      Seq(Node.Identifier(name) Dot "prototype", prototypeDef: OObject)
      )) =>
        //println(s"Match prototype def $name")
        Some(ClassId(name),prototypeDef)

      /// XXX.prototype = new { ... }
      case Node.ExpressionStatement(Assign(Node.Identifier(name) Dot "prototype", "=", prototypeDef: OObject)) =>
        //println(s"Match prototype def $name")
        Some(ClassId(name),prototypeDef)

      case _ => None
    }
  }


  def convertProtoClassesRecursive(n: Node.Node): Node.Node = {
    n.transformAfter { (node, transformer) =>
      implicit val ctx = transformer.context
      node match {
        //case _: Node.Program =>
        //  node
        case IsDeclScope() =>
          //println(s"scope $node")
          convertProtoClasses(node)(ctx)
        case _ =>
          node
      }
    }
  }

  def convertProtoClasses(n: Node.Node)(ctx: ScopeContext): Node.Node = {
    // for any class types try to find constructors and prototypes and try to transform them
    // start with global classes (are local classes even used in JS?)

    val classes = ClassList(n)(ctx)

    // optimization: avoid traversal when there are no classes detected
    // this is common for inner scopes
    if (classes.isEmpty) return n

    //println(s"Classes ${classes.classes.keys}")

    // we delete only initialization for static members, nothing else
    var staticMemberDeleted = Set.empty[(ClassId, String)]

    def verifyStaticMember(clsName: ClassId, member: String) = {
      val verify = classes.get(clsName).exists(_.membersStatic.contains(member))
      println(s"Verifying $clsName.$member: $verify")
      verify
    }

    def verifyStaticMemberOnce(clsName: ClassId, member: String) = {
      if (verifyStaticMember(clsName, member)) {
        if (staticMemberDeleted(clsName -> member)) {
          false
        } else {
          staticMemberDeleted += clsName -> member
          true
        }
      } else false
    }

    val deleteProtos = n.transformAfter(ctx) { (node, transformer) =>
      implicit val ctx = transformer.context
      node match {
        case ClassMemberDef(name, _, _, _) if classes contains name =>
          Node.EmptyStatement()
        case ClassPropertyDef(name, _, _) if classes contains name =>
          Node.EmptyStatement()
        case ClassParentDef(name, _) if classes contains name =>
          Node.EmptyStatement()
        case ClassPrototypeDef(name, _) if classes contains name =>
          Node.EmptyStatement()
        case ClassParentAndPrototypeDef(name, _, _) if classes contains name =>
          Node.EmptyStatement()
        case DefineStaticMember(name, member, statement) if classes contains name =>
          // verify we are deleting only the initialization, not any other use
          val clsMember = classes.get(name).flatMap(_.membersStatic.get(member))
          val isInit = clsMember.exists(_.definedFrom(statement))
          //println(s"Static member $name.$member - init $isInit")
          if (isInit) Node.EmptyStatement() else node
        case _  =>
          node
      }

    }

    val createClasses = deleteProtos.transformAfter { (node, transformer) =>
      implicit val ctx = transformer.context

      def emptyNode = Node.EmptyStatement()

      class Helper(tokensFrom: Node.Node) {

        object AsFunction {
          def onlyVariables(ss: Seq[Node.Statement]) = ss.forall(s => s.isInstanceOf[Node.VariableDeclaration])

          object ReturnValue {
            def unapply(arg: Node.Statement) = arg match {
              case Node.ReturnStatement(Defined(body)) =>
                Some(body)
              case Node.ExpressionStatement(body) =>
                Some(body)
              case _ =>
                None
            }
          }

          def unapply(arg: (ClassMember, Boolean)) = arg match {
            case (ClassFunMember(args, body), _) =>
              Some(args, body)

            // non-static functions should always be represented as functions if possible
            case (ClassVarMember(ScalaNode.StatementExpression(Node.BlockStatement(ss :+ ReturnValue(AnyFun(args, body))))), false) /*if onlyVariables(ss)*/ =>
              //println(nodeClassName(f))
              val newBody = ss ++ Block.statements(body)
              Some(args, newBody)

            // some var members should also be converted to fun members
            // expected structure:
            // - variable prefix + function body

            case _ =>
              None
          }
        }

        def newValue(k: String, v: Node.Expression, isStatic: Boolean): Node.ClassBodyElement = {
          //println(s"newValue $k $v $isStatic")
          Node.MethodDefinition(
            Node.Identifier(k),
            false,
            v,
            "value",
            isStatic

          )
        }

        def newGetterOrSetter(kind: String, k: String, args: Seq[Node.FunctionParameter], body: Seq[Node.StatementListItem], isStatic: Boolean) = {
          Node.MethodDefinition(
            Node.Identifier(k),
            false,
            Node.FunctionExpression(null, args, Node.BlockStatement(body).withTokens(tokensFrom), false).withTokens(tokensFrom),
            if (args.isEmpty) "get" else "set",
            isStatic
          )
        }

        def newMember(k: String, v: ClassMember, isStatic: Boolean = false) = {
          (v, isStatic) match {
            case AsFunction(args, body) =>
              newMethod(k, args, Node.BlockStatement(body).withTokens(tokensFrom), tokensFrom, isStatic)

            case (m: ClassVarMember, false) =>
              newGetter(k, Seq(), Seq(Node.ExpressionStatement(m.value.asInstanceOf[Node.Expression])), isStatic)
            case (m: ClassVarMember, true) =>
              newValue(k, m.value, isStatic)

            case _ =>
              ???

          }
        }

        def newGetter(k: String, args: Seq[Node.FunctionParameter], body: Seq[Node.StatementListItem], isStatic: Boolean = false) = {
          newGetterOrSetter("get", k, args, body, isStatic)
        }


        def newSetter(k: String, args: Seq[Node.FunctionParameter], body: Seq[Node.StatementListItem], isStatic: Boolean = false) = {
          newGetterOrSetter("set", k, args, body, isStatic)
        }

        def newClass(sym: Node.Identifier, base: Option[Node.Identifier], props: Seq[Node.ClassBodyElement], tokensFrom: Node.Node): Node.ClassDeclaration = {
          val cls = new Node.ClassDeclaration(sym, base.orNull, Node.ClassBody(props).withTokens(tokensFrom)).withTokens(tokensFrom)
          cls.body.range = (
            sym.range._1 min tokensFrom.range._1,
            sym.range._2 max tokensFrom.range._2
          )
          cls
        }
      }

      def classProperties(clazz: ClassDef): Seq[Node.ClassBodyElement] = {
        object helper extends Helper(node)
        import helper._

        val mappedMembers = clazz.members.map { case (k, v) => newMember(k, v) }.toSeq
        val mappedGetters = clazz.getters.map { case (k, v) => newGetter(k, v.args, v.body) }
        val mappedSetters = clazz.setters.map { case (k, v) => newSetter(k, v.args, v.body) }
        val mappedValues = clazz.values.map { case (k, v) => newValue(k, v.value, false) }
        val mappedStatic = clazz.membersStatic.map { case (k, v) => newMember(k, v, true) }

        mappedMembers ++ mappedGetters ++ mappedSetters ++ mappedValues ++ mappedStatic
      }

      def classDefine(sym: Node.Identifier, tokensFrom: Node.Node) = {
        val clsId = ClassId(sym)
        //println(s"  ${transformer.stack.map(nodeClassName).mkString("|")}")
        // check if there exists a type with this name
        val clazz = classes(clsId)

        object helper extends Helper(node)
        import helper._

        //val baseDef = clazz.base.flatMap(classes.get)

        val base = clazz.base.map(b => Node.Identifier(b.name))

        val properties = classProperties(clazz)

        //println(s"classDefine ${sym.name} ${sym.thedef.map(SymbolTypes.id)} ${clazz.base} $base ${base.flatMap(_.thedef.map(SymbolTypes.id))}")
        newClass(sym, base, properties, tokensFrom)
      }

      if (false) node match {
        case ex: Node.ExportNamedDeclaration =>
          println(s"walk Node.Export $node ${ex.declaration} ${ex.source}")
        case _: Node.BlockStatement =>
          println(s"walk Node.Block $node")
        case _ =>
      }
      node match {
        case ClassDefineValue(sym, _, _, _) if classes contains ClassId(sym) =>
          classDefine(sym, node)

        case ClassDefine(sym, _, _) if classes contains ClassId(sym) =>
          classDefine(sym, node)

        case DefineProperties(name, _) if classes.contains(name) =>
          emptyNode
        case DefineProperty(name, _, _) if classes.contains(name) =>
          emptyNode
        case classNode@Node.ClassDeclaration(Defined(sym), _, _) if classes contains ClassId(sym) =>
          // add any prototype member definitions as needed

          val mergeProperties = classProperties(classes(ClassId(sym)))

          classNode.body.body ++= mergeProperties
          //println(s"Node.ClassDeclaration $classNode - merge members (${mergeProperties.size})")

          classNode
        //case DefineStaticMember(name, member, _) if verifyStaticMemberOnce(name, member) =>
        //  emptyNode
        case classNode@Node.ClassDeclaration(_, _, _)  =>
          //println(s"Node.ClassDeclaration $classNode - not in ${classes.keys}")

          node
        case _ =>
          node
      }
    }

    val cleanupClasses = createClasses.transformAfter { (node, walker) =>
      implicit val ctx = walker.context
      // find enclosing class (if any)
      def thisClass = findThisClassInWalker(walker.context)

      object IsSuperClass {
        def unapply(name: SymId): Boolean = {
          //println(s"thisClass $thisClass ${name.name}")
          //println(s"superClass ${thisClass.flatMap(superClass)} ${name.name}")

          // name does not contain thedef, we did not provide it
          // find a corresponding symbol
          val cls = thisClass
          val baseSym = cls.flatMap(superClassSymbolDef)
          val baseId = baseSym.flatMap(SymbolTypes.id)

          //println(s"baseSym ${baseSym.map(_.name)} baseId $baseId")
          baseId.exists(thisClass.flatMap(superClass).contains)
        }
      }

      def removeHeadThis(args: Seq[Node.ArgumentListElement]) = {
        if (args.headOption.exists(_.isInstanceOf[Node.ThisExpression])) args.tail else args
      }

      def getClassArguments: Seq[String] = {
        val args = findThisClassInWalker(ctx).toSeq.flatMap { defClass =>
          findInlineBody(defClass).orElse(findConstructor(defClass)).toSeq.map(_.value).collect{case AnyFun(a, _) => a.map(parameterNameString)}.flatten
        }
        args
      }

      node match {
        // Animal.apply(this, Array.prototype.slice.call(arguments))
        case call@Node.CallExpression(
        Node.Identifier(Id(IsSuperClass())) Dot "apply",
        Seq(_: Node.ThisExpression,
        Node.CallExpression(Node.Identifier("Array") Dot "prototype" Dot "slice" Dot "call", Seq(Node.Identifier("arguments")))
        )) =>
          //println(s"Super constructor call in ${thisClass.map(_.name.get.name)}")
          call.callee = Node.Super().withTokens(node)
          call.arguments = getClassArguments.map(Node.Identifier.apply)
          call
        // Super.apply(this, arguments)
        case call@Node.CallExpression(Node.Identifier(Id(IsSuperClass())) Dot "apply", Seq(_: Node.ThisExpression, Node.Identifier("arguments"))) =>
          // TODO: check class constructor arguments as pass them
          call.callee = Node.Super().withTokens(node)
          call.arguments = getClassArguments.map(Node.Identifier.apply)
          call

        // Light.call( this, skyColor, intensity )
        case call@Node.CallExpression(Node.Identifier(Id(IsSuperClass())) Dot "call", args) =>
          //println(s"Super constructor call in ${thisClass.map(_.name.get.name)}")
          call.callee = Node.Super().withTokens(node)
          call.arguments = removeHeadThis(args)
          call


        // Animal.prototype.move.call(this, 5)
        case call@Node.CallExpression(
        Node.Identifier(Id(IsSuperClass())) Dot "prototype" Dot func Dot "call",
        Seq(_: Node.ThisExpression, args@_*)
        ) =>
          //println(s"Super call of $func in ${thisClass.map(_.name.get.name)}")
          call.callee = new Dot (Node.Super(), Node.Identifier(func))
          call.arguments = removeHeadThis(args)
          call

        // call(this, args) may be already transformed out by processCall
        case call@Node.CallExpression(
        Node.Identifier(Id(IsSuperClass())) Dot "prototype" Dot func,
        args
        ) =>
          //println(s"Super call of $func in ${thisClass.map(_.name.get.name)}")
          call.callee = new Dot (Node.Super(), Node.Identifier(func))
          call.arguments = args
          call

        // this.constructor, typically as new this.constructor( ... )
        case (_: Node.ThisExpression) Dot "constructor" =>
          //println("this.constructor")
          thisClass.map(_.id.name).fold(node)(cls => Node.Identifier(cls))

        case _ =>
          //println(nodeClassName(node))
          node
      }
    }

    cleanupClasses
  }

  def classTokenSource(cls: Node.ClassDeclaration): Node.Node = {
    cls.body
  }

  // convert class members represented as ObjectKeyVal into inline class body variables
  def convertClassMembers(n: Node.Node): Node.Node = {
    val ret = n.transformAfter { (node, _) =>
      node match {
        case cls: Node.ClassDeclaration  =>
          //println(s"convertClassMembers Node.ClassDeclaration ${ClassId(cls.name.get)}")

          val newMembers = mutable.ArrayBuffer.empty[Node.VariableDeclaration]
          cls.body.body.foreach {
            //case Node.MethodDefinition(Node.SymbolName(p), _) =>
            case kv@ObjectKeyVal(p, v) if !propertyIsStatic(kv) =>
              //println(s"newMembers append $cls $p $v")
              newMembers append VarDecl(p, Option(v).map(_.asInstanceOf[Node.Expression]), "var")
            //case s: Node.ObjectSetter =>
            //case s: Node.ObjectGetter =>
            case _ =>
          }
          if (newMembers.nonEmpty) {
            val inlineBody = classInlineBody(cls, classTokenSource(cls))
            //println(s"inlineBody ${inlineBody.body}")
            //println(s"convertClassMembers newMembers ${newMembers.mkString(",")}")

            val properties = inlineBody.value.asInstanceOf[Node.FunctionExpression].body

            properties.body ++= (newMembers: Iterable[Node.Statement])

            // remove overwritten members
            //println(s"  props ${cls.properties}")
            cls.body.body = cls.body.body.filterNot(p => newMembers.exists(_.declarations.exists(_.id == p.asInstanceOf[Node.MethodDefinition].key)))
            //println(s"  => props ${cls.properties}")
          }

          cls
        case _ =>
          node
      }
    }
    ret
  }


  def processAllClasses(n: NodeExtended, c: Option[Regex] = None)(p: (Node.ClassDeclaration, ScopeContext) => Node.ClassDeclaration): NodeExtended = {
    val ret = n.top.transformAfter { (node, transformer) =>
      implicit val ctx = transformer.context
      node match {
        case cls@Node.ClassDeclaration(Node.Identifier(cName), _, _) if c.forall(_.findFirstIn(cName).isDefined)=>
          p(cls, ctx)
        case _ =>
          node
      }
    }
    n.copy (top = ret)
  }


  def applyRules(n: NodeExtended): NodeExtended = {
    n.config.rules.foldLeft(n){ (n, rule) =>
      val r = rule(n)
      //n.top.figure_out_scope()
      r
    }

  }


  /**
    * motivation: handle KeyframeTrack.prototype = KeyframeTrackPrototype in Three.js
    * */
  def inlinePrototypeVariables(n: Node.Node): Node.Node = {
    // convert:
    // XXX.prototype = YYYY
    // YYYY = ....
    // to
    // XXX.prototype = ....
    // and replace YYYY with XXX.prototype
    // is order a problem?

    var prototypeVariableSymbols = Map.empty[SymId, Node.Identifier]

    object PrototypeVariable {
      def unapply(arg: Node.Node) = arg match  {
        case Node.ExpressionStatement(Assign((clsSym: Node.Identifier) Dot "prototype", "=", Node.Identifier(protoFunSym))) =>
          Some(clsSym, protoFunSym)
        case _ => None
      }
    }
    n.walkWithScope { (node, context) =>
      implicit val ctx = context
      node match {
        case PrototypeVariable(clsSym, Id(protoFunSym)) =>
          //println(s"Detected prototype variable ${protoFunSym.name} for ${clsSym.name}")
          prototypeVariableSymbols += protoFunSym -> clsSym
          false
        case _ =>
          false
      }
    }

    var prototypeVariableDefs = Map.empty[SymId, Node.Node]

    object PrototypeVariableDef {
      def unapply(arg: Node.Node)(implicit context: ScopeContext) = arg match {
        case VarDecl(Id(symDef), Some(init), _) if prototypeVariableSymbols contains symDef =>
          Some(symDef, init)
        case _ => None

      }
    }

    n.walkWithScope { (node, context) =>
      implicit val ctx = context
      node match {
        case PrototypeVariableDef(symDef, init) =>
          //println(s"Detected prototype variable init ${symDef.name}")
          prototypeVariableDefs += symDef -> init
          false
        case _ =>
          false
      }
    }
    n.transformAfter { (node, context) =>
      implicit val ctx = context.context
      node match {
        case PrototypeVariable(clsName, Id(protoFunSym)) =>
          prototypeVariableDefs.get(protoFunSym).map(pv => Assign(clsName, "=", pv.asInstanceOf[Node.Expression])).getOrElse(node)
        case PrototypeVariableDef(_, _) =>
          Node.EmptyStatement()
        case _ =>
          node
      }
    }.transformAfter { (node, context) =>
      implicit val ctx = context.context
      node match {
        case symRef@Node.Identifier(Id(symDef)) =>
          prototypeVariableSymbols.get(symDef).fold[Node.Node](symRef) { clsSymRef =>
            Dot(clsSymRef.cloneNode(), Node.Identifier("prototype"))
          }
        case _ =>
          node
      }
    }

  }

  /**
    * motivation: handle const pp = Parser.prototype in Acorn
    * */

  def inlinePrototypeConstants(n: Node.Node): Node.Node = {
    // convert:
    // const pp = XXX.prototype
    // pp.f = ....
    // to
    // XXX.prototype.f = ....

    var prototypeSymbols = Map.empty[SymId, SymId]

    object PrototypeConstant {
      def unapply(arg: Node.Node)(implicit context: ScopeContext) = arg match  {
        case VarDecl(Id(protoSym), Some(Node.Identifier(clsSym) Dot "prototype"), _) =>
          Some(clsSym, protoSym)
        case _ => None
      }
    }
    n.walkWithScope { (node, context) =>
      implicit val ctx = context
      node match {
        case PrototypeConstant(Id(clsSym), protoFunSym) =>
          //println(s"Detected prototype constant ${protoFunSym.name} for ${clsSym.name}")
          prototypeSymbols += protoFunSym -> clsSym
          false
        case _ =>
          false
      }
    }

    n.transformAfter { (node, transformer) =>
      implicit val ctx = transformer.context
      node match {
        case symRef@Node.Identifier(Id(symDef)) =>
          prototypeSymbols.get(symDef).fold[Node.Node](symRef) { clsSymDef =>
            Dot (
              Node.Identifier(clsSymDef.name),
              Node.Identifier("prototype")
            )
          }
        case _ =>
          node
      }
    }

  }


  /**
    * motivation: handle KeyframeTrack constructor implemented using KeyframeTrackConstructor.apply call
    * */
  def inlineConstructorFunction(n: Node.Node): Node.Node = {
    // list constructor functions
    // XXXXX.prototype.constructor = XXXXX
    var constructorSymbols = Map.empty[SymId, Node.Statement]

    object PrototypeConstructor {
      def unapply(arg: Node.Node)(implicit context: ScopeContext) = arg match {
        case s@Node.ExpressionStatement(Assign(
        Node.Identifier(Id(clsSym)) Dot "prototype" Dot "constructor", "=", Node.Identifier(Id(protoFunSym))
        )) if clsSym.name == protoFunSym.name =>
          Some(clsSym, s)
        case _ => None
      }
    }

    n.walkWithScope { (node, context) =>
      implicit val ctx = context
      node match {
        case PrototypeConstructor(clsSym, s) =>
          //println(s"Detected constructor function for ${clsSym.name}")
          constructorSymbols += clsSym -> s
          true
        case _ =>
          false
      }
    }

    var implToConstructor = Map.empty[SymId, SymId] // implementation -> class constructor
    // find constructors implemented by calling another function
    // function XXXXX( name, times, values, interpolation ) { YYYYY.apply( this, arguments ); }
    n.walkWithScope { (node, context) =>
      implicit val ctx = context
      node match {
        case DefFun(
        Node.Identifier(Id(fun)), args, Seq(Node.ExpressionStatement(Node.CallExpression(
        Node.Identifier(Id(implementFun)) Dot "apply", Seq(_: Node.ThisExpression, Node.Identifier("arguments"))
        ))), _
        ) if constructorSymbols contains fun =>
          //println(s"Defined function ${fun.name} using ${implementFun.name}")
          implToConstructor += implementFun -> fun
          false
        case _ =>
          false
      }
    }

    var constructorFunctionDefs = Map.empty[SymId, DefFun]
    n.walkWithScope { (node, context) =>
      implicit val ctx = context
      node match {
        case defun@DefFun(Defined(Node.Identifier(Id(fun))), _, _, _) if implToConstructor contains fun =>
          //println(s"Stored function def ${fun.name}")
          constructorFunctionDefs += fun -> defun
          true
        case _ =>
          false
      }
    }

    val constructorToImpl = implToConstructor.map(_.swap)

    n.transformAfter { (node, transformer) =>
      implicit val ctx = transformer.context
      node match {
        // rewrite the symbol YYYYY use to  XXXXX
        case n@Node.Identifier(Id(symDef)) =>
          implToConstructor.get(symDef).fold(node) { impl =>
            n.name = impl.name
            n
          }
        // inline XXXXX.apply(this, arguments) - was already rewritten from YYYYY (transformAfter transforms children first)
        case defun@DefFun(_, _, Seq(Node.ExpressionStatement(Node.CallExpression(
        Node.Identifier(Id(symDef)) Dot "apply", Seq(_: Node.ThisExpression, Node.Identifier("arguments"))
        ))), _) =>
          //println(s"Detect ${symDef.name}.apply")
          constructorToImpl.get(symDef).flatMap(constructorFunctionDefs.get).fold(node) { funDef =>
            //println(s"Found body of ${funDef.name.map(_.name)} in ${defun.name.map(_.name)}")
            defun.body = funDef.body
            defun
          }
        // remove the original implementation
        case defun@DefFun(Defined(Node.Identifier(Id(sym))), _, _, _) if implToConstructor contains sym =>
          Node.EmptyStatement()
        case _ => node
      }
    }
  }


  val transforms = Seq[NodeExtended => NodeExtended](
    onTopNode(inlinePrototypeVariables),
    onTopNode(inlinePrototypeConstants),
    onTopNode(inlineConstructorFunction),
    onTopNode(convertProtoClassesRecursive),
    onTopNode(convertClassMembers),
    // privateVariables before FillVarMembers, so that variables are introduced correctly
    onTopNode(transform.classes.InlineConstructors.privateVariables),
    // privateFunctions after privateVariables, are already converted to this.member references
    // privateFunctions before FillVarMembers, so that variables for the functions are not created yet
    onTopNode(transform.classes.InlineConstructors.privateFunctions),
    transform.classes.FillVarMembers.apply,
    // applyRules after fillVarMembers - we cannot delete members before they are created
    // applyRules before inlineConstructors, so that constructor is a single function
    applyRules,
    transform.classes.InlineConstructors.apply
  )
}
