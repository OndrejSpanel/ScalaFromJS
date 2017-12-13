package com.github.opengrabeso
package transform

import Transform._
import Classes._
import com.github.opengrabeso.esprima._
import _root_.esprima._

import JsUtils._

import SymbolTypes.SymbolMapId

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.language.implicitConversions

package object classes {

  type ClassId = SymbolMapId

  object ClassId {
    // TODO: avoid get, use something safe instead
    def apply(sym: SymbolDef): ClassId = {
      SymbolTypes.id(sym).getOrElse(SymbolMapId(sym.name, 0))
    }
    def apply(sym: Node.Identifier): ClassId = ClassId(sym.thedef.get)
  }

  object ClassDefineValue {
    // function C() {return {a: x, b:y}
    def unapply(arg: DefFun) = arg match {
      case DefFun(Defined(sym), args, body :+ Node.ReturnStatement(OObject(proto))) =>
        Some(sym, args, body, proto)
      case _ =>
        None
    }

  }
  object ClassDefine {
    def unapply(arg: Node.Node): Option[(Node.Identifier, Seq[Node.FunctionParameter], Seq[Node.Statement])] = arg match {
      // function ClassName() {}
      case DefFun(Defined(sym), args, body) =>
        Some(sym, args, body)

      // ClassName = function() {}
      case Node.Assign(sym: Node.Identifier, "=", Node.FunctionExpression(args, body)) =>
        Some(sym, args, body)

      // var ClassName = function() {}
      case Node.VariableDeclaration(Node.VariableDeclarator(sym: Node.Identifier, Defined(Node.FunctionExpression(args, body)))) =>
        Some(sym, args, body)

      case _ =>
        //println(nodeClassName(arg))
        None
    }
  }

  sealed trait ClassMember {
    def definedFrom(init: Node.Node): Boolean
  }

  case class ClassFunMember(args: Seq[Node.FunctionParameter], body: Seq[Node.Statement]) extends ClassMember {
    def definedFrom(init: Node.Node) = init match {
      case func: Node.FunctionExpression =>
        // reference equality of the first member is enough, nodes are unique
        //println(s"Defined func from: ${ScalaOut.outputNode(body.head)} ${ScalaOut.outputNode(func.body.head)}")
        func.body.head == body.head
      case _ => false
    }
  }

  case class ClassVarMember(value: Node.Node) extends ClassMember {
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
    def unapply(arg: Node.Node) = arg match {
      case Node.ExpressionStatement(Node.Assign(Node.Identifier(_, _, Defined(symDef)) Dot "prototype" Dot funName, "=", value)) =>
        Some(ClassId(symDef), funName, value)
      case _ => None
    }
  }

  object ClassMemberDef {
    def unapply(arg: Node.Node) = arg match {
      case ClassPropertyDef(name, funName, Node.Function(args, body)) =>
        Some(name, funName, args, body)
      case _ => None
    }
  }

  object DefineProperties {

    object DefinePropertiesObject {
      def unapply(arg: Node.Node) = arg match {
        case Node.ExpressionStatement(Node.CallExpression(Node.Identifier("Object") Dot "defineProperties",
        Node.Identifier(_, _, Defined(symDef)) Dot "prototype", properties@_*)) =>
          Some(ClassId(symDef), properties)

        case Node.ExpressionStatement(Node.CallExpression(Node.Identifier("Object") Dot "defineProperties",
        Node.Identifier(_, _, Defined(symDef)), properties@_*)) =>
          Some(ClassId(symDef), properties)

        case _ => None
      }
    }

    def unapply(arg: Node.Node) = arg match {
      case DefinePropertiesObject(name, Seq(OObject(properties))) => Some(name, properties)
      case _ => None

    }
  }

  object DefineProperty {
    def unapply(arg: Node.Node) = arg match {
      // Object.defineProperty(XXXX.prototype, "name", {...} //
      case Node.ExpressionStatement(Node.CallExpression(
      Node.Identifier("Object") Dot "defineProperty",
      Node.Identifier(sym) Dot "prototype",
      prop: Node.String,
      OObject(properties))) =>
        Some(ClassId(sym), prop.value, properties)
      case _ => None

    }
  }

  object DefineStaticMember {
    def unapply(arg: Node.Node) = arg match {
      // Cls.defX = 0;
      // Cls.defY = function() {return 0;};
      case Node.ExpressionStatement(Node.Assign(Node.Identifier(_, Defined(scope), Defined(clsSym)) Dot member, "=", value)) =>
        Some(ClassId(clsSym), clsSym, scope, member, value)
      case _ => None
    }
  }

  object ClassParentAndPrototypeDef {
    def unapply(arg: Node.Node) = arg match {
      // name.prototype = Object.assign( Object.create( sym.prototype ), {... prototype object ... } )
      case Node.ExpressionStatement(
      Node.Assign(Node.Identifier(name) Dot "prototype", "=",
      Node.CallExpression(
      Node.Identifier("Object") Dot "assign",
      Node.CallExpression(Node.Identifier("Object") Dot "create", Node.Identifier(sym) Dot "prototype"), prototypeDef: OObject)
      )) =>
        //println(s"ClassParentAndPrototypeDef $name extends ${sym.name}")
        Some(ClassId(name), ClassId(sym), prototypeDef)

      // Object.assign( name.prototype, sym.prototype, {prototype object} )
      case Node.ExpressionStatement(Node.CallExpression(
      Node.Identifier("Object") Dot "assign",
      Node.Identifier(name) Dot "prototype",
      Node.Identifier(sym) Dot "prototype",
      prototypeDef: OObject
      )) =>
        //println(s"ClassParentAndPrototypeDef2 $name extends $sym")
        Some(ClassId(name), ClassId(sym), prototypeDef)
      case _ =>
        None
    }

  }

  object ClassParentDef {
    def unapply(arg: Node.Node) = arg match {
      // name.prototype = new sym.prototype
      case Node.ExpressionStatement(Node.Assign(Node.Identifier(name) Dot "prototype", "=", Node.New(Node.Identifier(sym), _*))) =>
        Some(ClassId(name), sym)

      // name.prototype = Object.create( sym.prototype );
      case Node.ExpressionStatement(Node.Assign(
      Node.Identifier(name) Dot "prototype", "=",
      Node.CallExpression(Node.Identifier("Object") Dot "create", Node.Identifier(sym) Dot "prototype")
      )) =>
        Some(ClassId(name), sym)

      // Object.assign( name.prototype, sym.prototype)
      case Node.ExpressionStatement(Node.CallExpression(
      Node.Identifier("Object") Dot "assign",
      Node.Identifier(name) Dot "prototype",
      Node.Identifier(sym) Dot "prototype"
      )) =>
        Some(ClassId(name), sym)

      case _ => None
    }
  }

  object ClassPrototypeDef {
    def unapply(arg: Node.Node) = arg match {

      //Object.assign( XXX.prototype, { ... })
      case Node.ExpressionStatement(Node.CallExpression(
      Node.Identifier("Object") Dot "assign",
      Node.Identifier(name) Dot "prototype", prototypeDef: OObject
      )) =>
        //println(s"Match prototype def $name")
        Some(ClassId(name),prototypeDef)

      /// XXX.prototype = new { ... }
      case Node.ExpressionStatement(Node.Assign(Node.Identifier(name) Dot "prototype", "=", prototypeDef: OObject)) =>
        //println(s"Match prototype def $name")
        Some(ClassId(name),prototypeDef)

      case _ => None
    }
  }


  def convertProtoClassesRecursive(n: Node.Node): Node.Node = {
    n.transformAfter { (node, _) =>
      node match {
        //case _: Node.Program =>
        //  node
        case _: Node.Scope =>
          //println(s"scope $node")
          convertProtoClasses(node)
        case _ =>
          node
      }
    }
  }

  def convertProtoClasses(n: Node.Node): Node.Node = {
    // for any class types try to find constructors and prototypes and try to transform them
    // start with global classes (are local classes even used in JS?)

    val classes = ClassList(n)

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

    val deleteProtos = n.transformAfter { (node, transformer) =>
      node match {
        case t: Node.Block =>
          val newBody = t.body.filter {
            case ClassMemberDef(name, _, _, _) if classes contains name =>
              false
            case ClassPropertyDef(name, _, _) if classes contains name =>
              false
            case ClassParentDef(name, _) if classes contains name =>
              false
            case ClassPrototypeDef(name, _) if classes contains name =>
              false
            case ClassParentAndPrototypeDef(name, _, _) if classes contains name =>
              false
            case DefineStaticMember(name, _, _, member, statement) if classes contains name =>
              // verify we are deleting only the initialization, not any other use
              val clsMember = classes.get(name).flatMap(_.membersStatic.get(member))
              val isInit = clsMember.exists(_.definedFrom(statement))
              //println(s"Static member $name.$member - init $isInit")
              !isInit // return false for init to filter it out
            case _  =>
              true
          }
          t.body = newBody
          t
        case _ =>
          node
      }

    }

    val createClasses = deleteProtos.transformAfter { (node, walker) =>
      def emptyNode = Node.EmptyStatement(node)

      class Helper(tokensFrom: Node.Node) {

        object AsFunction {
          def onlyVariables(ss: Seq[Node.Statement]) = ss.forall(s => s.isInstanceOf[Node.Var])

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
            case (ClassVarMember(Node.BlockStatement(ss :+ ReturnValue(Node.Function(args, body)))), false) /*if onlyVariables(ss)*/ =>
              //println(nodeClassName(f))
              val newBody = ss ++ body
              Some(args, newBody)

            // some var members should also be converted to fun members
            // expected structure:
            // - variable prefix + function body

            case _ =>
              None
          }
        }

        def newValue(k: String, v: Node.Node, isStatic: Boolean) = {
          //println(s"newValue $k $v $isStatic")
          new ObjectKeyVal {
            fillTokens(this, v)
            key = k
            value = v
            // hack - use quote to mark static values
            quote = if (isStatic) "'" else "\""

          }
        }

        def newGetterOrSetter(node: Node.ObjectSetterOrGetter, k: String, args: Seq[Node.FunctionParameter], body: Seq[Node.Statement], isStatic: Boolean) = {
          fillTokens(node, tokensFrom)
          node.key = keyNode(tokensFrom, k)
          node.`static` = isStatic
          node.value = new Node.Function {
            fillTokens(this, tokensFrom)
            argnames = args.toJSArray
            this.body = body.toJSArray
          }
          node
        }

        def newMember(k: String, v: ClassMember, isStatic: Boolean = false) = {
          (v, isStatic) match {
            case AsFunction(args, body) =>
              newMethod(k, args, body, tokensFrom, isStatic): Node.ObjectProperty

            case (m: ClassVarMember, false) =>
              newGetter(k, Seq(), Seq(Node.ExpressionStatement(tokensFrom)(m.value)), isStatic)
            case (m: ClassVarMember, true) =>
              newValue(k, m.value, isStatic)

          }
        }

        def newGetter(k: String, args: Seq[Node.FunctionParameter], body: Seq[Node.Statement], isStatic: Boolean = false): Node.ObjectProperty = {
          newGetterOrSetter(new Node.ObjectGetter, k, args, body, isStatic)
        }


        def newSetter(k: String, args: Seq[Node.FunctionParameter], body: Seq[Node.Statement], isStatic: Boolean = false): Node.ObjectProperty = {
          newGetterOrSetter(new Node.ObjectSetter, k, args, body, isStatic)
        }

        def newClass(sym: Node.Identifier, base: Option[Node.Node], props: Iterable[Node.ObjectProperty]): Node.ClassDeclaration = {
          new Node.ClassDeclaration {
            fillTokens(this, tokensFrom)
            name = new Node.SymbolDefClass {
              /*_*/
              fillTokens(this, tokensFrom)
              /*_*/
              name = sym.name
              scope = sym.scope
              thedef = sym.thedef
              //init = this
            }

            //println(s"${sym.name} extends ${clazz.base}")
            `extends` = base

            properties = props.toJSArray
          }

        }
      }

      def classProperties(clazz: ClassDef) = {
        object helper extends Helper(node)
        import helper._

        val mappedMembers = clazz.members.map { case (k, v) => newMember(k, v) }
        val mappedGetters = clazz.getters.map { case (k, v) => newGetter(k, v.args, v.body) }
        val mappedSetters = clazz.setters.map { case (k, v) => newSetter(k, v.args, v.body) }
        val mappedValues = clazz.values.map { case (k, v) => newValue(k, v.value, false) }
        val mappedStatic = clazz.membersStatic.map { case (k, v) => newMember(k, v, true) }

        mappedMembers ++ mappedGetters ++ mappedSetters ++ mappedValues ++ mappedStatic
      }

      def classDefine(sym: Node.Identifier) = {
        val clsId = ClassId(sym)
        //println(s"  ${walker.stack.map(nodeClassName).mkString("|")}")
        // check if there exists a type with this name
        val clazz = classes(clsId)

        object helper extends Helper(node)
        import helper._

        //val baseDef = clazz.base.flatMap(classes.get)

        val base = clazz.base.fold(js.undefined: Option[Node.Identifier])(b => Node.Identifier(node)(b.name))

        val properties = classProperties(clazz)

        //println(s"classDefine ${sym.name} ${sym.thedef.map(SymbolTypes.id)} ${clazz.base} $base ${base.flatMap(_.thedef.map(SymbolTypes.id))}")
        newClass(sym, base, properties)
      }

      if (false) node match {
        case ex: Node.Export =>
          println(s"walk Node.Export $node ${ex.exported_value} ${ex.exported_definition}")
        case _: Node.Block =>
          println(s"walk Node.Block $node")
        case _ =>
      }
      node match {
        case ClassDefineValue(sym, _, _, _) if classes contains ClassId(sym) =>
          classDefine(sym)

        case ClassDefine(sym, _, _) if classes contains ClassId(sym) =>
          classDefine(sym)

        case DefineProperties(name, _) if classes.contains(name) =>
          emptyNode
        case DefineProperty(name, _, _) if classes.contains(name) =>
          emptyNode
        case classNode@Node.ClassDeclaration(Defined(sym), _, _) if classes contains ClassId(sym) =>
          // add any prototype member definitions as needed

          val mergeProperties = classProperties(classes(ClassId(sym)))

          classNode.properties ++= mergeProperties
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
      // find enclosing class (if any)
      def thisClass = findThisClassInWalker(walker)

      object IsSuperClass {
        def unapply(name: SymbolDef): Boolean = {
          //println(s"thisClass $thisClass ${name.name}")
          //println(s"superClass ${thisClass.flatMap(superClass)} ${name.name}")

          // name does not contain thedef, we did not provide it
          // find a corresponding symbol
          val baseSym = thisClass.flatMap(superClassSymbolDef)
          val baseId = baseSym.flatMap(SymbolTypes.id)

          //println(s"baseSym ${baseSym.map(_.name)} baseId $baseId")
          baseId.exists(thisClass.flatMap(superClass).contains)
        }
      }

      def removeHeadThis(args: Seq[Node.Node]) = {
        if (args.headOption.exists(_.isInstanceOf[Node.This])) args.tail else args
      }

      def getClassArguments: Seq[Node.Node] = {
        val args = findThisClassInWalker(walker).toSeq.flatMap { defClass =>
          findInlineBody(defClass).orElse(findConstructor(defClass)).toSeq.flatMap(_.value.argnames)
        }
        args.map(a => Node.Identifier.sym(a)(a))
      }

      node match {
        // Animal.apply(this, Array.prototype.slice.call(arguments))
        case call@Node.CallExpression(
        Node.Identifier(IsSuperClass()) Dot "apply",
        _: Node.This,
        Node.CallExpression(Node.Identifier("Array") Dot "prototype" Dot "slice" Dot "call", Node.Identifier("arguments"))
        ) =>
          //println(s"Super constructor call in ${thisClass.map(_.name.get.name)}")
          call.expression = Node.Super().withTokens(node)
          call.args = getClassArguments.toJSArray
          call
        // Super.apply(this, arguments)
        case call@Node.CallExpression(Node.Identifier(IsSuperClass()) Dot "apply", _: Node.This, Node.Identifier("arguments")) =>
          // TODO: check class constructor arguments as pass them
          call.expression = Node.Super().withTokens(node)
          call.args = getClassArguments.toJSArray
          call

        // Light.call( this, skyColor, intensity )
        case call@Node.CallExpression(Node.Identifier(IsSuperClass()) Dot "call", args@_*) =>
          //println(s"Super constructor call in ${thisClass.map(_.name.get.name)}")
          call.expression = Node.Super().withTokens(node)
          call.args = removeHeadThis(args).toJSArray
          call


        // Light.prototype.call( this, source );
        case call@Node.CallExpression(
        Node.Identifier(IsSuperClass()) Dot "prototype" Dot func Dot "call",
        _: Node.This, args@_*
        ) =>
          //println(s"Super call of $func in ${thisClass.map(_.name.get.name)}")
          call.expression = new Dot {
            fillTokens(this, node)
            expression = Node.Super().withTokens(node)
            property = func
          }
          call.args = removeHeadThis(args).toJSArray
          call


        // this.constructor, typically as new this.constructor( ... )
        case (_: Node.This) Dot "constructor" =>
          //println("this.constructor")
          thisClass.flatMap(_.name.map(_.name)).fold(node)(cls => Node.Identifier(node)(cls))

        case _ =>
          //println(nodeClassName(node))
          node
      }
    }

    cleanupClasses
  }

  def classTokenSource(cls: Node.ClassDeclaration): Node.Node = {
    // prefer class name symbol as the token source
    // ES6 classes parse class name as it is, we do not want to change this
    // instead we change the class token root so that all symbols in the class have the same offset as the class symbol
    val name = cls.name
    // symbol declaration may sometimes be someplace else than the class location ifself. In such case prefer the declaration
    // this happens with import directives
    val declName = name.flatMap(_.thedef.flatMap(_.orig.headOption))

    declName.orElse(name).getOrElse(cls)
  }

  // convert class members represented as ObjectKeyVal into inline class body variables
  def convertClassMembers(n: Node.Node): Node.Node = {
    val ret = n.transformAfter { (node, _) =>
      node match {
        case cls: Node.ClassDeclaration  =>
          //println(s"convertClassMembers Node.ClassDeclaration ${ClassId(cls.name.get)}")

          val newMembers = mutable.ArrayBuffer.empty[Node.Var]
          cls.properties.foreach {
            //case Node.MethodDefinition(Node.SymbolName(p), _) =>
            case kv@ObjectKeyVal(p, v) if !propertyIsStatic(kv) =>
              //println(s"newMembers append $cls $p $v")
              newMembers append Node.Var(cls)(Node.VariableDeclarator.initialized(cls)(p, v))
            //case s: Node.ObjectSetter =>
            //case s: Node.ObjectGetter =>
            case _ =>
          }
          if (newMembers.nonEmpty) {
            val inlineBody = classInlineBody(cls, classTokenSource(cls))
            //println(s"inlineBody ${inlineBody.body}")
            //println(s"convertClassMembers newMembers ${newMembers.mkString(",")}")

            inlineBody.body ++= (newMembers: Iterable[Node.Statement]).toJSArray

            // remove overwritten members
            //println(s"  props ${cls.properties}")
            cls.properties = cls.properties.filterNot(p => newMembers.exists(_.definitions.exists(_.name.name == propertyName(p))))
            //println(s"  => props ${cls.properties}")
          }

          cls
        case _ =>
          node
      }
    }
    ret
  }


  def processAllClasses(n: NodeExtended, c: Option[RegExp] = None)(p: Node.ClassDeclaration => Node.ClassDeclaration): NodeExtended = {
    val ret = n.top.transformAfter { (node, _) =>
      node match {
        case cls@Node.ClassDeclaration(Defined(Node.SymbolName(cName)), _, _) if c.forall(_ test cName)=>
          p(cls)
        case _ =>
          node
      }
    }
    n.copy (top = ret)
  }


  def applyRules(n: NodeExtended): NodeExtended = {
    n.config.rules.foldLeft(n){ (n, rule) =>
      rule(n)
      n.top.figure_out_scope()
      n
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

    var prototypeVariableSymbols = Map.empty[SymbolDef, Node.Identifier]

    object PrototypeVariable {
      def unapply(arg: Node.Node) = arg match  {
        case Node.ExpressionStatement(assign@Node.Assign((clsSym: Node.Identifier) Dot "prototype", "=", Node.Identifier(protoFunSym))) =>
          Some(clsSym, protoFunSym, assign)
        case _ => None
      }
    }
    n.walk {
      case PrototypeVariable(clsSym, protoFunSym, _) =>
        //println(s"Detected prototype variable ${protoFunSym.name} for ${clsSym.name}")
        prototypeVariableSymbols += protoFunSym -> clsSym
        false
      case _ =>
        false
    }

    var prototypeVariableDefs = Map.empty[SymbolDef, Node.Node]

    object PrototypeVariableDef {
      def unapply(arg: Node.Node) = arg match {
        case Node.VariableDeclaration(Node.VariableDeclarator(Node.Identifier(_, _, Defined(symDef)), Defined(init))) if prototypeVariableSymbols contains symDef =>
          Some(symDef, init)
        case _ => None

      }
    }

    n.walk {
      case PrototypeVariableDef(symDef, init) =>
        //println(s"Detected prototype variable init ${symDef.name}")
        prototypeVariableDefs += symDef -> init
        false
      case _ =>
        false
    }

    n.transformAfter { (node, _) =>
      node match {
        case PrototypeVariable(clsName, protoFunSym, assign) =>
          prototypeVariableDefs.get(protoFunSym).foreach (pv => assign.right = pv)
          node
        case PrototypeVariableDef(_, _) =>
          Node.EmptyStatement(node)
        case _ =>
          node
      }
    }.transformAfter { (node, _) =>
      node match {
        case symRef@Node.Identifier(_,_,Defined(symDef)) =>
          prototypeVariableSymbols.get(symDef).fold[Node.Node](symRef) { clsSymRef =>
            new Dot {
              fillTokens(this, symRef)
              expression = clsSymRef.clone()
              property = "prototype"
            }
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

    var prototypeSymbols = Map.empty[SymbolDef, SymbolDef]

    object PrototypeConstant {
      def unapply(arg: Node.Node) = arg match  {
        case Node.VariableDeclaration(Node.VariableDeclarator(Node.Identifier(_, _, Defined(protoSym)), Node.Identifier(clsSym) Dot "prototype")) =>
          Some(clsSym, protoSym)
        case _ => None
      }
    }
    n.walk {
      case PrototypeConstant(clsSym, protoFunSym) =>
        //println(s"Detected prototype constant ${protoFunSym.name} for ${clsSym.name}")
        prototypeSymbols += protoFunSym -> clsSym
        false
      case _ =>
        false
    }

    n.transformAfter { (node, _) =>
      node match {
        case symRef@Node.Identifier(_,_,Defined(symDef)) =>
          prototypeSymbols.get(symDef).fold[Node.Node](symRef) { clsSymDef =>
            new Dot {
              fillTokens(this, symRef)
              expression = Node.Identifier.symDef(symRef)(clsSymDef)
              property = "prototype"
            }
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
    var constructorSymbols = Map.empty[SymbolDef, Node.Statement]

    object PrototypeConstructor {
      def unapply(arg: Node.Node) = arg match {
        case s@Node.ExpressionStatement(Node.Assign(
        Node.Identifier(clsSym) Dot "prototype" Dot "constructor", "=", Node.Identifier(protoFunSym)
        )) if clsSym.name == protoFunSym.name =>
          Some(clsSym, s)
        case _ => None
      }
    }

    n.walk {
      case PrototypeConstructor(clsSym, s) =>
        //println(s"Detected constructor function for ${clsSym.name}")
        constructorSymbols += clsSym -> s
        true
      case _ =>
        false
    }

    var implToConstructor = Map.empty[SymbolDef, SymbolDef] // implementation -> class constructor
    // find constructors implemented by calling another function
    // function XXXXX( name, times, values, interpolation ) { YYYYY.apply( this, arguments ); }
    n.walk {
      case DefFun(
      Defined(Node.Identifier(fun)), args, Seq(Node.ExpressionStatement(Node.CallExpression(
      Node.Identifier(implementFun) Dot "apply", _: Node.This, Node.Identifier("arguments")
      )))) if constructorSymbols contains fun =>
        //println(s"Defined function ${fun.name} using ${implementFun.name}")
        implToConstructor += implementFun -> fun
        false
      case _ =>
        false
    }

    var constructorFunctionDefs = Map.empty[SymbolDef, DefFun]
    n.walk {
      case defun@DefFun(Defined(Node.Identifier(fun)), _, _) if implToConstructor contains fun =>
        //println(s"Stored function def ${fun.name}")
        constructorFunctionDefs += fun -> defun
        true
      case _  =>
        false
    }


    val constructorToImpl = implToConstructor.map(_.swap)

    n.transformAfter { (node, _) =>
      node match {
        // rewrite the symbol YYYYY use to  XXXXX
        case n@Node.Identifier(_, _, Defined(symDef)) =>
          implToConstructor.get(symDef).fold(node) { impl =>
            n.thedef = impl
            n.name = impl.name
            n
          }
        // inline XXXXX.apply(this, arguments) - was already rewritten from YYYYY (transformAfter transforms children first)
        case defun@DefFun(_, _, Seq(Node.ExpressionStatement(Node.CallExpression(
        Node.Identifier(symDef) Dot "apply", _: Node.This, Node.Identifier("arguments")
        )))) =>
          //println(s"Detect ${symDef.name}.apply")
          constructorToImpl.get(symDef).flatMap(constructorFunctionDefs.get).fold(node) { funDef =>
            //println(s"Found body of ${funDef.name.map(_.name)} in ${defun.name.map(_.name)}")
            defun.body = funDef.body.toJSArray
            defun
          }
        // remove the original implementation
        case defun@DefFun(Defined(Node.Identifier(sym)), _, _) if implToConstructor contains sym =>
          Node.EmptyStatement(defun)
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
