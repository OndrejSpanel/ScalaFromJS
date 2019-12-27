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
      case DefFun(sym, args, Node.BlockStatement(body :+ Node.ReturnStatement(OObject(proto))), _, _) =>
        Some(sym, args, body, proto)
      case _ =>
        None
    }

  }

  object ClassDefine {
    def unapply(arg: Node.Node)(implicit context: ScopeContext): Option[(Node.Identifier, Seq[Node.FunctionParameter], Seq[Node.Statement])] = arg match {
      // function ClassName() {}
      case DefFun(sym, args, body, _, _) =>
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

    def tokensFrom: Node.Node
  }

  case class ClassFunMember(args: Seq[Node.FunctionParameter], body: Seq[Node.StatementListItem], tokensFrom: Node.Node) extends ClassMember {
    def definedFrom(init: Node.Node) = init match {
      case func: Node.FunctionExpression =>
        // reference equality of the first member is enough, nodes are unique
        //println(s"Defined func from: ${ScalaOut.outputNode(body.head)} ${ScalaOut.outputNode(func.body.head)}")
        func.body.body.head == body.head
      case _ => false
    }
  }

  case class ClassVarMember(value: Node.Expression, tokensFrom: Node.Node) extends ClassMember {
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
        Some(ClassId(name), prototypeDef)

      /// XXX.prototype = new { ... }
      case Node.ExpressionStatement(Assign(Node.Identifier(name) Dot "prototype", "=", prototypeDef: OObject)) =>
        //println(s"Match prototype def $name")
        Some(ClassId(name), prototypeDef)

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
        case _ =>
          node
      }

    }

    val logging = false

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
            case (ClassFunMember(args, body, _), _) =>
              Some(args, body)

            // non-static functions should always be represented as functions if possible
            case (ClassVarMember(ScalaNode.StatementExpression(Node.BlockStatement(ss :+ ReturnValue(AnyFun(args, body)))), _), false) /*if onlyVariables(ss)*/ =>
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
            Node.Identifier(k).withTokens(tokensFrom),
            null,
            false,
            v,
            "value",
            isStatic

          )
        }

        def newGetterOrSetter(kind: String, k: String, tokensFrom: Node.Node, args: Seq[Node.FunctionParameter], body: Seq[Node.StatementListItem], isStatic: Boolean) = {
          Node.MethodDefinition(
            Node.Identifier(k).withTokens(tokensFrom),
            null,
            false,
            Node.FunctionExpression(null, args, Node.BlockStatement(body).withTokens(tokensFrom), false, null).withTokens(tokensFrom),
            if (args.isEmpty) "get" else "set",
            isStatic
          ).withTokens(tokensFrom)
        }

        def newMember(k: String, v: ClassMember, isStatic: Boolean = false) = {
          (v, isStatic) match {
            case AsFunction(args, body) =>
              newMethod(k, args, Node.BlockStatement(body).withTokens(v.tokensFrom), v.tokensFrom, isStatic)

            case (m: ClassVarMember, false) =>
              newGetter(k, m.tokensFrom, Seq(), Seq(Node.ExpressionStatement(m.value)), isStatic)
            case (m: ClassVarMember, true) =>
              newValue(k, m.value, isStatic)

            case _ =>
              ???

          }
        }

        def newGetter(k: String, tokensFrom: Node.Node, args: Seq[Node.FunctionParameter], body: Seq[Node.StatementListItem], isStatic: Boolean = false) = {
          newGetterOrSetter("get", k, tokensFrom, args, body, isStatic)
        }

        def newSetter(k: String, tokensFrom: Node.Node, args: Seq[Node.FunctionParameter], body: Seq[Node.StatementListItem], isStatic: Boolean = false) = {
          newGetterOrSetter("set", k, tokensFrom, args, body, isStatic)
        }

        def newClass(sym: Node.Identifier, base: Option[Node.Identifier], props: Seq[Node.ClassBodyElement], tokensFrom: Node.Node): Node.ClassDeclaration = {
          val cls = new Node.ClassDeclaration(sym, base.orNull, Nil, Node.ClassBody(props).withTokens(tokensFrom), "class").withTokens(tokensFrom)
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
        val mappedGetters = clazz.getters.map { case (k, v) => newGetter(k, v.tokensFrom, v.args, v.body) }
        val mappedSetters = clazz.setters.map { case (k, v) => newSetter(k, v.tokensFrom, v.args, v.body) }
        val mappedValues = clazz.values.map { case (k, v) => newValue(k, v.value, false) }
        val mappedStatic = clazz.membersStatic.map { case (k, v) => newMember(k, v, true) }

        (mappedGetters ++ mappedSetters ++ mappedValues ++ mappedMembers ++ mappedStatic).toSeq
      }

      def classDefine(sym: Node.Identifier, tokensFrom: Node.Node) = {
        val clsId = ClassId(sym)
        //println(s"  ${transformer.stack.map(nodeClassName).mkString("|")}")
        // check if there exists a type with this name
        val clazz = classes(clsId)

        object helper extends Helper(node)
        import helper._

        //val baseDef = clazz.base.flatMap(classes.get)

        val base = clazz.base.map(b => Node.Identifier(b.name).withTokens(tokensFrom))

        val properties = classProperties(clazz)

        if (logging) println(s"classDefine ${sym.name} ${Id(sym)} ${clazz.base} $base")
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
        case classNode@Node.ClassDeclaration(Defined(sym), _, _, _, _) if classes contains ClassId(sym) =>
          // add any prototype member definitions as needed

          val mergeProperties = classProperties(classes(ClassId(sym)))

          if (mergeProperties.nonEmpty || classNode.body != null) { // classNode.body == null: trait or enum
            classNode.body.body ++= mergeProperties
            //println(s"Node.ClassDeclaration $classNode - merge members (${mergeProperties.size})")
          }

          classNode
        //case DefineStaticMember(name, member, _) if verifyStaticMemberOnce(name, member) =>
        //  emptyNode
        case Node.ClassDeclaration(_, _, _, _, _) =>
          //println(s"Node.ClassDeclaration node - not in ${classes.keys}")
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

          baseId.contains(name)
        }
      }

      def removeHeadThis(args: Seq[Node.ArgumentListElement]) = {
        if (args.headOption.exists(_.isInstanceOf[Node.ThisExpression])) args.tail else args
      }

      def getClassArguments: Seq[String] = {
        val args = findThisClassInWalker(ctx).toSeq.flatMap { defClass =>
          findInlineBody(defClass).orElse(findConstructor(defClass)).toSeq.map(_.value).collect { case AnyFun(a, _) => a.map(parameterNameString) }.flatten
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
          call.arguments = getClassArguments.map(a => Node.Identifier(a).withTokens(node))
          call

        // Super(arguments)
        case call@Node.CallExpression(Node.Identifier(Id(IsSuperClass())), _) =>
          // TODO: check class constructor arguments as we pass them
          call.callee = Node.Super().withTokens(node)
          //call.arguments = args
          call

        // Super.apply(this, arguments)
        case call@Node.CallExpression(Node.Identifier(Id(IsSuperClass())) Dot "apply", Seq(_: Node.ThisExpression, Node.Identifier("arguments"))) =>
          call.callee = Node.Super().withTokens(node)
          call.arguments = getClassArguments.map(a => Node.Identifier(a).withTokens(node))
          call

        // Super.apply(this, arg1 ... argN)
        case call@Node.CallExpression(Node.Identifier(Id(IsSuperClass())) Dot "apply", (_: Node.ThisExpression) +: args) =>
          call.callee = Node.Super().withTokens(node)
          call.arguments = args
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
          call.callee = Dot(Node.Super(), Node.Identifier(func).withTokens(call)).withTokens(call)
          call.arguments = removeHeadThis(args)
          call

        // call(this, args) may be already transformed out by processCall
        case call@Node.CallExpression(
        Node.Identifier(Id(IsSuperClass())) Dot "prototype" Dot func,
        args
        ) =>
          //println(s"Super call of $func in ${thisClass.map(_.name.get.name)}")
          call.callee = Dot(Node.Super(), Node.Identifier(func).withTokens(call)).withTokens(call)
          call.arguments = args
          call

        // this.constructor, typically as new this.constructor( ... )
        case (_: Node.ThisExpression) Dot "constructor" =>
          //println("this.constructor")
          thisClass.map(_.id.name).fold(node)(cls => Node.Identifier(cls).withTokens(node))

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
        case cls: Node.ClassDeclaration =>
          //println(s"convertClassMembers Node.ClassDeclaration ${ClassId(cls.name.get)}")

          val newMembers = mutable.ArrayBuffer.empty[Node.VariableDeclaration]
          if (cls.body != null) cls.body.body.foreach {
            //case Node.MethodDefinition(Node.SymbolName(p), _) =>
            case node@ObjectKeyVal(p, v) if !propertyIsStatic(node) => // probably relic from Uglify - Node.Property is not ClassBodyElement
              //println(s"newMembers append $cls $p $v")
              newMembers append VarDecl(p, Option(v).map(_.asInstanceOf[Node.Expression]), "var")(node)
            case Node.MethodDefinition(identNode@Node.Identifier(name), tpe, false, value, "value", false) =>
              newMembers append VarDecl(name, Option(value).map(_.asInstanceOf[Node.Expression]), "var", Option(tpe))(identNode)
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
        case cls@Node.ClassDeclaration(Node.Identifier(cName), _, _, _, _) if c.forall(_.findFirstIn(cName).isDefined) =>
          p(cls, ctx)
        case _ =>
          node
      }
    }
    n.copy(top = ret)
  }


  def applyRules(n: NodeExtended): NodeExtended = {
    n.config.rules.foldLeft(n) { (n, rule) =>
      val r = rule(n)
      //n.top.figure_out_scope()
      r
    }

  }


  /**
    * motivation: handle KeyframeTrack.prototype = KeyframeTrackPrototype in Three.js
    **/
  def inlinePrototypeVariables(n: Node.Node): Node.Node = {
    // convert:
    // XXX.prototype = YYYY
    // YYYY = ....
    // to
    // XXX.prototype = ....
    // and replace YYYY with XXX.prototype
    // is order a problem?

    val logging = false

    var prototypeVariableSymbols = Map.empty[SymId, Node.Expression]

    object PrototypeVariable {
      def unapply(arg: Node.Node) = arg match {
        case Node.ExpressionStatement(Assign(left@((clsSym: Node.Identifier) Dot "prototype"), "=", Node.Identifier(protoFunSym))) =>
          Some(left, protoFunSym)
        case _ => None
      }
    }
    n.walkWithScope { (node, context) =>
      implicit val ctx = context
      node match {
        case PrototypeVariable(left, Id(protoFunSym)) =>
          if (logging) println(s"Detected prototype variable $protoFunSym for $left")
          prototypeVariableSymbols += protoFunSym -> left
          false
        case _ =>
          false
      }
    }

    var prototypeVariableDefs = Map.empty[SymId, Node.Expression]

    object PrototypeVariableDef {
      def unapply(arg: Node.Node)(implicit context: ScopeContext) = arg match {
        case VarDecl(Id(symDef), Some(init), _) if prototypeVariableSymbols contains symDef =>
          Some(symDef, init)
        case Node.ExpressionStatement(Assign(Node.Identifier(Id(symDef)), "=", init)) if prototypeVariableSymbols contains symDef =>
          Some(symDef, init)
        case _ => None

      }
    }
    n.walkWithScope { (node, context) =>
      implicit val ctx = context
      node match {
        case PrototypeVariableDef(symDef, init) =>
          if (logging) println(s"Detected prototype variable init $symDef $init")
          prototypeVariableDefs += symDef -> init
          false
        case _ =>
          false
      }
    }
    n.transformAfter { (node, context) =>
      implicit val ctx = context.context
      node match {
        case PrototypeVariable(left, Id(protoFunSym)) =>
          prototypeVariableDefs.get(protoFunSym).map { pv =>
            if (logging) println(s"replace PrototypeVariable '$left' = '$protoFunSym':'$pv'" )
            Node.ExpressionStatement(Assign(left, "=", pv))
          }.getOrElse(node)
        case _ =>
          //if (logging) println(s"copy '$node'" )
          node
      }
    }

    n.transformBefore { (node, descend, transformer) =>
      implicit val ctx = transformer.context
      node match {
        case VarDecl(name, init, _) =>
          if (logging) println(s"Descend into VarDecl of $name = $init")
          init.foreach { i =>
            descend(transformer.before(i, descend), transformer)
          }
          node

        // do not rename inside of imports / exports
        case _: Node.ImportSpecifier =>
          node

        case _: Node.ExportSpecifier =>
          node

        case Node.Identifier(Id(symDef)) =>
          prototypeVariableSymbols.get(symDef).fold[Node.Node](node) { s =>
            if (logging) println(s"Replace Identifier $symDef with $s")
            s.cloneNode()
          }

        case _ =>
          descend(node, transformer)
          node
      }
    }

    n.transformAfter { (node, context) =>
      implicit val ctx = context.context
      node match {
        case PrototypeVariableDef(_, _) =>
          if (logging) println(s"remove PrototypeVariable '$node'")
          Node.EmptyStatement()
        case _ =>
          node
      }
    }


  }

  /**
    * motivation: handle const pp = Parser.prototype in Acorn
    **/

  def inlinePrototypeConstants(n: Node.Node): Node.Node = {
    // convert:
    // const pp = XXX.prototype
    // pp.f = ....
    // to
    // XXX.prototype.f = ....

    var prototypeSymbols = Map.empty[SymId, SymId]

    object PrototypeConstant {
      def unapply(arg: Node.Node)(implicit context: ScopeContext) = arg match {
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
            Dot(
              Node.Identifier(clsSymDef.name),
              Node.Identifier("prototype")
            ).withTokens(node)
          }
        case _ =>
          node
      }
    }

  }


  /**
    * motivation: handle KeyframeTrack constructor implemented using KeyframeTrackConstructor.apply call
    **/
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
        Node.Identifier(Id(fun)), args, Node.BlockStatement(Seq(Node.ExpressionStatement(Node.CallExpression(
        Node.Identifier(Id(implementFun)) Dot "apply", Seq(_: Node.ThisExpression, Node.Identifier("arguments"))
        )))), _, _
        ) if constructorSymbols contains fun =>
          //println(s"Defined function ${fun.name} using ${implementFun.name}")
          implToConstructor += implementFun -> fun
          false
        case DefFun(
        Node.Identifier(Id(fun)), pars, Node.BlockStatement(Seq(Node.ExpressionStatement(Node.CallExpression(
        Node.Identifier(Id(implementFun)) Dot "apply", (_: Node.ThisExpression) +: args)
        ))), _, _
        ) if (constructorSymbols contains fun) && (args == pars) =>
          // TODO: match args with pars
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
        case defun@DefFun(Defined(Node.Identifier(Id(fun))), _, _, _, _) if implToConstructor contains fun =>
          //println(s"Stored function def ${fun.name}")
          constructorFunctionDefs += fun -> defun
          true
        case _ =>
          false
      }
    }

    val constructorToImpl = implToConstructor.map(_.swap)

    val ret = n.transformAfter { (node, transformer) =>
      implicit val ctx = transformer.context
      node match {
        // rewrite the symbol YYYYY use to  XXXXX
        case n@Node.Identifier(Id(symDef)) if !ctx.parent().exists(_.isInstanceOf[DefFun]) =>
          implToConstructor.get(symDef).fold(node) { impl =>
            n.name = impl.name
            n
          }
        // inline XXXXX.apply(this, arguments) - was already rewritten from YYYYY (transformAfter transforms children first)
        case defun@DefFun(_, _, Node.BlockStatement(Seq(Node.ExpressionStatement(Node.CallExpression(
        Node.Identifier(Id(symDef)) Dot "apply", Seq(_: Node.ThisExpression, Node.Identifier("arguments"))
        )))), _, _) =>
          //println(s"Detect ${symDef.name}.apply")
          constructorToImpl.get(symDef).flatMap(constructorFunctionDefs.get).fold(node) { funDef =>
            //println(s"Found body of ${funDef.name.map(_.name)} in ${defun.name.map(_.name)}")
            defun.body = funDef.body
            defun
          }
        case defun@DefFun(_, pars, Node.BlockStatement(Seq(Node.ExpressionStatement(Node.CallExpression(
        Node.Identifier(Id(symDef)) Dot "apply", (_: Node.ThisExpression) +: args
        )))), _, _) if pars == args =>
          //println(s"Detect ${symDef.name}.apply")
          constructorToImpl.get(symDef).flatMap(constructorFunctionDefs.get).fold(node) { funDef =>
            //println(s"Found body of ${funDef.name.map(_.name)} in ${defun.name.map(_.name)}")
            defun.body = funDef.body
            defun
          }
        // remove the original implementation
        case defun@DefFun(Defined(Node.Identifier(Id(sym))), _, _, _, _) if implToConstructor contains sym =>
          Node.EmptyStatement()
        case _ => node
      }
    }

    ret
  }


  val transforms = Seq[(Symbol, NodeExtended => NodeExtended)](
    'inlinePrototypeVariables -> onTopNode(inlinePrototypeVariables),
    'inlinePrototypeConstants -> onTopNode(inlinePrototypeConstants),
    'inlineConstructorFunction -> onTopNode(inlineConstructorFunction),
    'convertProtoClassesRecursive -> onTopNode(convertProtoClassesRecursive),
    'convertClassMembers -> onTopNode(convertClassMembers),
    // privateVariables before FillVarMembers, so that variables are introduced correctly
    'privateVariables -> onTopNode(transform.classes.InlineConstructors.privateVariables),
    // privateFunctions after privateVariables, are already converted to this.member references
    // privateFunctions before FillVarMembers, so that variables for the functions are not created yet
    'privateFunctions -> onTopNode(transform.classes.InlineConstructors.privateFunctions),
    'FillVarMembers -> transform.classes.FillVarMembers.apply,
    // applyRules after fillVarMembers - we cannot delete members before they are created
    // applyRules before inlineConstructors, so that constructor is a single function
    'applyRules -> applyRules,
    'InlineConstructors -> transform.classes.InlineConstructors.apply
  )
}
