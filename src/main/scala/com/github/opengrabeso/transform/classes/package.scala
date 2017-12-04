package com.github.opengrabeso
package transform

import Transform._
import Classes._
import Uglify._
import UglifyExt._
import JsUtils._
import UglifyExt.Import._
import SymbolTypes.SymbolMapId

import scala.scalajs.js
import js.JSConverters._
import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.language.implicitConversions
import scala.scalajs.js.RegExp

package object classes {

  type ClassId = SymbolMapId

  object ClassId {
    // TODO: avoid get, use something safe instead
    def apply(sym: SymbolDef): ClassId = {
      SymbolTypes.id(sym).getOrElse(SymbolMapId(sym.name, 0))
    }
    def apply(sym: AST_Symbol): ClassId = ClassId(sym.thedef.get)
  }

  object ClassDefineValue {
    // function C() {return {a: x, b:y}
    def unapply(arg: AST_Defun) = arg match {
      case AST_Defun(Defined(sym), args, body :+ AST_Return(AST_Object(proto))) =>
        Some(sym, args, body, proto)
      case _ =>
        None
    }

  }
  object ClassDefine {
    def unapply(arg: AST_Node): Option[(AST_Symbol, Seq[AST_SymbolFunarg], Seq[AST_Statement])] = arg match {
      // function ClassName() {}
      case AST_Defun(Defined(sym), args, body) =>
        Some(sym, args, body)

      // ClassName = function() {}
      case AST_Assign(sym: AST_SymbolRef, "=", AST_Lambda(args, body)) =>
        Some(sym, args, body)

      // var ClassName = function() {}
      case AST_Definitions(AST_VarDef(sym: AST_Symbol, Defined(AST_Lambda(args, body)))) =>
        Some(sym, args, body)

      case _ =>
        //println(nodeClassName(arg))
        None
    }
  }

  sealed trait ClassMember {
    def definedFrom(init: AST_Node): Boolean
  }

  case class ClassFunMember(args: Seq[AST_SymbolFunarg], body: Seq[AST_Statement]) extends ClassMember {
    def definedFrom(init: AST_Node) = init match {
      case func: AST_Lambda =>
        // reference equality of the first member is enough, nodes are unique
        //println(s"Defined func from: ${ScalaOut.outputNode(body.head)} ${ScalaOut.outputNode(func.body.head)}")
        func.body.head == body.head
      case _ => false
    }
  }

  case class ClassVarMember(value: AST_Node) extends ClassMember {
    def definedFrom(init: AST_Node) = {
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
    def unapply(arg: AST_Node) = arg match {
      case AST_SimpleStatement(AST_Assign(AST_SymbolRef(_, _, Defined(symDef)) AST_Dot "prototype" AST_Dot funName, "=", value)) =>
        Some(ClassId(symDef), funName, value)
      case _ => None
    }
  }

  object ClassMemberDef {
    def unapply(arg: AST_Node) = arg match {
      case ClassPropertyDef(name, funName, AST_Function(args, body)) =>
        Some(name, funName, args, body)
      case _ => None
    }
  }

  object DefineProperties {

    object DefinePropertiesObject {
      def unapply(arg: AST_Node) = arg match {
        case AST_SimpleStatement(AST_Call(AST_SymbolRefName("Object") AST_Dot "defineProperties",
        AST_SymbolRef(_, _, Defined(symDef)) AST_Dot "prototype", properties@_*)) =>
          Some(ClassId(symDef), properties)

        case AST_SimpleStatement(AST_Call(AST_SymbolRefName("Object") AST_Dot "defineProperties",
        AST_SymbolRef(_, _, Defined(symDef)), properties@_*)) =>
          Some(ClassId(symDef), properties)

        case _ => None
      }
    }

    def unapply(arg: AST_Node) = arg match {
      case DefinePropertiesObject(name, Seq(AST_Object(properties))) => Some(name, properties)
      case _ => None

    }
  }

  object DefineProperty {
    def unapply(arg: AST_Node) = arg match {
      // Object.defineProperty(XXXX.prototype, "name", {...} //
      case AST_SimpleStatement(AST_Call(
      AST_SymbolRefName("Object") AST_Dot "defineProperty",
      AST_SymbolRefDef(sym) AST_Dot "prototype",
      prop: AST_String,
      AST_Object(properties))) =>
        Some(ClassId(sym), prop.value, properties)
      case _ => None

    }
  }

  object DefineStaticMember {
    def unapply(arg: AST_Node) = arg match {
      // Cls.defX = 0;
      // Cls.defY = function() {return 0;};
      case AST_SimpleStatement(AST_Assign(AST_SymbolRef(_, Defined(scope), Defined(clsSym)) AST_Dot member, "=", value)) =>
        Some(ClassId(clsSym), clsSym, scope, member, value)
      case _ => None
    }
  }

  object ClassParentAndPrototypeDef {
    def unapply(arg: AST_Node) = arg match {
      // name.prototype = Object.assign( Object.create( sym.prototype ), {... prototype object ... } )
      case AST_SimpleStatement(
      AST_Assign(AST_SymbolRefDef(name) AST_Dot "prototype", "=",
      AST_Call(
      AST_SymbolRefName("Object") AST_Dot "assign",
      AST_Call(AST_SymbolRefName("Object") AST_Dot "create", AST_SymbolRefDef(sym) AST_Dot "prototype"), prototypeDef: AST_Object)
      )) =>
        //println(s"ClassParentAndPrototypeDef $name extends ${sym.name}")
        Some(ClassId(name), ClassId(sym), prototypeDef)

      // Object.assign( name.prototype, sym.prototype, {prototype object} )
      case AST_SimpleStatement(AST_Call(
      AST_SymbolRefName("Object") AST_Dot "assign",
      AST_SymbolRefDef(name) AST_Dot "prototype",
      AST_SymbolRefDef(sym) AST_Dot "prototype",
      prototypeDef: AST_Object
      )) =>
        //println(s"ClassParentAndPrototypeDef2 $name extends $sym")
        Some(ClassId(name), ClassId(sym), prototypeDef)
      case _ =>
        None
    }

  }

  object ClassParentDef {
    def unapply(arg: AST_Node) = arg match {
      // name.prototype = new sym.prototype
      case AST_SimpleStatement(AST_Assign(AST_SymbolRefDef(name) AST_Dot "prototype", "=", AST_New(AST_SymbolRefDef(sym), _*))) =>
        Some(ClassId(name), sym)

      // name.prototype = Object.create( sym.prototype );
      case AST_SimpleStatement(AST_Assign(
      AST_SymbolRefDef(name) AST_Dot "prototype", "=",
      AST_Call(AST_SymbolRefName("Object") AST_Dot "create", AST_SymbolRefDef(sym) AST_Dot "prototype")
      )) =>
        Some(ClassId(name), sym)

      // Object.assign( name.prototype, sym.prototype)
      case AST_SimpleStatement(AST_Call(
      AST_SymbolRefName("Object") AST_Dot "assign",
      AST_SymbolRefDef(name) AST_Dot "prototype",
      AST_SymbolRefDef(sym) AST_Dot "prototype"
      )) =>
        Some(ClassId(name), sym)

      case _ => None
    }
  }

  object ClassPrototypeDef {
    def unapply(arg: AST_Node) = arg match {

      //Object.assign( XXX.prototype, { ... })
      case AST_SimpleStatement(AST_Call(
      AST_SymbolRefName("Object") AST_Dot "assign",
      AST_SymbolRefDef(name) AST_Dot "prototype", prototypeDef: AST_Object
      )) =>
        //println(s"Match prototype def $name")
        Some(ClassId(name),prototypeDef)

      /// XXX.prototype = new { ... }
      case AST_SimpleStatement(AST_Assign(AST_SymbolRefDef(name) AST_Dot "prototype", "=", prototypeDef: AST_Object)) =>
        //println(s"Match prototype def $name")
        Some(ClassId(name),prototypeDef)

      case _ => None
    }
  }


  def convertProtoClassesRecursive(n: AST_Node): AST_Node = {
    n.transformAfter { (node, _) =>
      node match {
        //case _: AST_Toplevel =>
        //  node
        case _: AST_Scope =>
          //println(s"scope $node")
          convertProtoClasses(node)
        case _ =>
          node
      }
    }
  }

  def convertProtoClasses(n: AST_Node): AST_Node = {
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
        case t: AST_Block =>
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
      def emptyNode = AST_EmptyStatement(node)

      class Helper(tokensFrom: AST_Node) {

        object AsFunction {
          def onlyVariables(ss: Seq[AST_Statement]) = ss.forall(s => s.isInstanceOf[AST_Var])

          object ReturnValue {
            def unapply(arg: AST_Statement) = arg match {
              case AST_Return(Defined(body)) =>
                Some(body)
              case AST_SimpleStatement(body) =>
                Some(body)
              case _ =>
                None
            }
          }

          def unapply(arg: (ClassMember, Boolean)) = arg match {
            case (ClassFunMember(args, body), _) =>
              Some(args, body)

            // non-static functions should always be represented as functions if possible
            case (ClassVarMember(AST_BlockStatement(ss :+ ReturnValue(AST_Function(args, body)))), false) /*if onlyVariables(ss)*/ =>
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

        def newValue(k: String, v: AST_Node, isStatic: Boolean) = {
          //println(s"newValue $k $v $isStatic")
          new AST_ObjectKeyVal {
            fillTokens(this, v)
            key = k
            value = v
            // hack - use quote to mark static values
            quote = if (isStatic) "'" else "\""

          }
        }

        def newGetterOrSetter(node: AST_ObjectSetterOrGetter, k: String, args: Seq[AST_SymbolFunarg], body: Seq[AST_Statement], isStatic: Boolean) = {
          fillTokens(node, tokensFrom)
          node.key = keyNode(tokensFrom, k)
          node.`static` = isStatic
          node.value = new AST_Function {
            fillTokens(this, tokensFrom)
            argnames = args.toJSArray
            this.body = body.toJSArray
          }
          node
        }

        def newMember(k: String, v: ClassMember, isStatic: Boolean = false) = {
          (v, isStatic) match {
            case AsFunction(args, body) =>
              newMethod(k, args, body, tokensFrom, isStatic): AST_ObjectProperty

            case (m: ClassVarMember, false) =>
              newGetter(k, Seq(), Seq(AST_SimpleStatement(tokensFrom)(m.value)), isStatic)
            case (m: ClassVarMember, true) =>
              newValue(k, m.value, isStatic)

          }
        }

        def newGetter(k: String, args: Seq[AST_SymbolFunarg], body: Seq[AST_Statement], isStatic: Boolean = false): AST_ObjectProperty = {
          newGetterOrSetter(new AST_ObjectGetter, k, args, body, isStatic)
        }


        def newSetter(k: String, args: Seq[AST_SymbolFunarg], body: Seq[AST_Statement], isStatic: Boolean = false): AST_ObjectProperty = {
          newGetterOrSetter(new AST_ObjectSetter, k, args, body, isStatic)
        }

        def newClass(sym: AST_Symbol, base: js.UndefOr[AST_Node], props: Iterable[AST_ObjectProperty]): AST_DefClass = {
          new AST_DefClass {
            fillTokens(this, tokensFrom)
            name = new AST_SymbolDefClass {
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

      def classDefine(sym: AST_Symbol) = {
        val clsId = ClassId(sym)
        //println(s"  ${walker.stack.map(nodeClassName).mkString("|")}")
        // check if there exists a type with this name
        val clazz = classes(clsId)

        object helper extends Helper(node)
        import helper._

        //val baseDef = clazz.base.flatMap(classes.get)

        val base = clazz.base.fold(js.undefined: js.UndefOr[AST_SymbolRef])(b => AST_SymbolRef(node)(b.name))

        val properties = classProperties(clazz)

        //println(s"classDefine ${sym.name} ${sym.thedef.map(SymbolTypes.id)} ${clazz.base} $base ${base.flatMap(_.thedef.map(SymbolTypes.id))}")
        newClass(sym, base, properties)
      }

      if (false) node match {
        case ex: AST_Export =>
          println(s"walk AST_Export $node ${ex.exported_value.nonNull} ${ex.exported_definition.nonNull}")
        case _: AST_Block =>
          println(s"walk AST_Block $node")
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
        case classNode@AST_DefClass(Defined(sym), _, _) if classes contains ClassId(sym) =>
          // add any prototype member definitions as needed

          val mergeProperties = classProperties(classes(ClassId(sym)))

          classNode.properties ++= mergeProperties
          //println(s"AST_DefClass $classNode - merge members (${mergeProperties.size})")

          classNode
        //case DefineStaticMember(name, member, _) if verifyStaticMemberOnce(name, member) =>
        //  emptyNode
        case classNode@AST_DefClass(_, _, _)  =>
          //println(s"AST_DefClass $classNode - not in ${classes.keys}")

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

      def removeHeadThis(args: Seq[AST_Node]) = {
        if (args.headOption.exists(_.isInstanceOf[AST_This])) args.tail else args
      }

      def getClassArguments: Seq[AST_Node] = {
        val args = findThisClassInWalker(walker).toSeq.flatMap { defClass =>
          findInlineBody(defClass).orElse(findConstructor(defClass)).toSeq.flatMap(_.value.argnames)
        }
        args.map(a => AST_SymbolRef.sym(a)(a))
      }

      node match {
        // Animal.apply(this, Array.prototype.slice.call(arguments))
        case call@AST_Call(
        AST_SymbolRefDef(IsSuperClass()) AST_Dot "apply",
        _: AST_This,
        AST_Call(AST_SymbolRefName("Array") AST_Dot "prototype" AST_Dot "slice" AST_Dot "call", AST_SymbolRefName("arguments"))
        ) =>
          //println(s"Super constructor call in ${thisClass.map(_.name.get.name)}")
          call.expression = AST_Super().withTokens(node)
          call.args = getClassArguments.toJSArray
          call
        // Super.apply(this, arguments)
        case call@AST_Call(AST_SymbolRefDef(IsSuperClass()) AST_Dot "apply", _: AST_This, AST_SymbolRefName("arguments")) =>
          // TODO: check class constructor arguments as pass them
          call.expression = AST_Super().withTokens(node)
          call.args = getClassArguments.toJSArray
          call

        // Light.call( this, skyColor, intensity )
        case call@AST_Call(AST_SymbolRefDef(IsSuperClass()) AST_Dot "call", args@_*) =>
          //println(s"Super constructor call in ${thisClass.map(_.name.get.name)}")
          call.expression = AST_Super().withTokens(node)
          call.args = removeHeadThis(args).toJSArray
          call


        // Light.prototype.call( this, source );
        case call@AST_Call(
        AST_SymbolRefDef(IsSuperClass()) AST_Dot "prototype" AST_Dot func AST_Dot "call",
        _: AST_This, args@_*
        ) =>
          //println(s"Super call of $func in ${thisClass.map(_.name.get.name)}")
          call.expression = new AST_Dot {
            fillTokens(this, node)
            expression = AST_Super().withTokens(node)
            property = func
          }
          call.args = removeHeadThis(args).toJSArray
          call


        // this.constructor, typically as new this.constructor( ... )
        case (_: AST_This) AST_Dot "constructor" =>
          //println("this.constructor")
          thisClass.flatMap(_.name.nonNull.map(_.name)).fold(node)(cls => AST_SymbolRef(node)(cls))

        case _ =>
          //println(nodeClassName(node))
          node
      }
    }

    cleanupClasses
  }

  def classTokenSource(cls: AST_DefClass): AST_Node = {
    // prefer class name symbol as the token source
    // ES6 classes parse class name as it is, we do not want to change this
    // instead we change the class token root so that all symbols in the class have the same offset as the class symbol
    val name = cls.name.nonNull
    // symbol declaration may sometimes be someplace else than the class location ifself. In such case prefer the declaration
    // this happens with import directives
    val declName = name.flatMap(_.thedef.nonNull.flatMap(_.orig.headOption))

    declName.orElse(name).getOrElse(cls)
  }

  // convert class members represented as AST_ObjectKeyVal into inline class body variables
  def convertClassMembers(n: AST_Node): AST_Node = {
    val ret = n.transformAfter { (node, _) =>
      node match {
        case cls: AST_DefClass  =>
          //println(s"convertClassMembers AST_DefClass ${ClassId(cls.name.get)}")

          val newMembers = mutable.ArrayBuffer.empty[AST_Var]
          cls.properties.foreach {
            //case AST_ConciseMethod(AST_SymbolName(p), _) =>
            case kv@AST_ObjectKeyVal(p, v) if !propertyIsStatic(kv) =>
              //println(s"newMembers append $cls $p $v")
              newMembers append AST_Var(cls)(AST_VarDef.initialized(cls)(p, v))
            //case s: AST_ObjectSetter =>
            //case s: AST_ObjectGetter =>
            case _ =>
          }
          if (newMembers.nonEmpty) {
            val inlineBody = classInlineBody(cls, classTokenSource(cls))
            //println(s"inlineBody ${inlineBody.body}")
            //println(s"convertClassMembers newMembers ${newMembers.mkString(",")}")

            inlineBody.body ++= (newMembers: Iterable[AST_Statement]).toJSArray

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


  def processAllClasses(n: AST_Extended, c: Option[RegExp] = None)(p: AST_DefClass => AST_DefClass): AST_Extended = {
    val ret = n.top.transformAfter { (node, _) =>
      node match {
        case cls@AST_DefClass(Defined(AST_SymbolName(cName)), _, _) if c.forall(_ test cName)=>
          p(cls)
        case _ =>
          node
      }
    }
    n.copy (top = ret)
  }


  def applyRules(n: AST_Extended): AST_Extended = {
    n.config.rules.foldLeft(n){ (n, rule) =>
      rule(n)
      n.top.figure_out_scope()
      n
    }

  }


  /**
    * motivation: handle KeyframeTrack.prototype = KeyframeTrackPrototype in Three.js
    * */
  def inlinePrototypeVariables(n: AST_Node): AST_Node = {
    // convert:
    // XXX.prototype = YYYY
    // YYYY = ....
    // to
    // XXX.prototype = ....
    // and replace YYYY with XXX.prototype
    // is order a problem?

    var prototypeVariableSymbols = Map.empty[SymbolDef, AST_SymbolRef]

    object PrototypeVariable {
      def unapply(arg: AST_Node) = arg match  {
        case AST_SimpleStatement(assign@AST_Assign((clsSym: AST_SymbolRef) AST_Dot "prototype", "=", AST_SymbolRefDef(protoFunSym))) =>
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

    var prototypeVariableDefs = Map.empty[SymbolDef, AST_Node]

    object PrototypeVariableDef {
      def unapply(arg: AST_Node) = arg match {
        case AST_Definitions(AST_VarDef(AST_Symbol(_, _, Defined(symDef)), Defined(init))) if prototypeVariableSymbols contains symDef =>
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
          AST_EmptyStatement(node)
        case _ =>
          node
      }
    }.transformAfter { (node, _) =>
      node match {
        case symRef@AST_SymbolRef(_,_,Defined(symDef)) =>
          prototypeVariableSymbols.get(symDef).fold[AST_Node](symRef) { clsSymRef =>
            new AST_Dot {
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

  def inlinePrototypeConstants(n: AST_Node): AST_Node = {
    // convert:
    // const pp = XXX.prototype
    // pp.f = ....
    // to
    // XXX.prototype.f = ....

    var prototypeSymbols = Map.empty[SymbolDef, SymbolDef]

    object PrototypeConstant {
      def unapply(arg: AST_Node) = arg match  {
        case AST_Definitions(AST_VarDef(AST_Symbol(_, _, Defined(protoSym)), AST_SymbolRefDef(clsSym) AST_Dot "prototype")) =>
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
        case symRef@AST_SymbolRef(_,_,Defined(symDef)) =>
          prototypeSymbols.get(symDef).fold[AST_Node](symRef) { clsSymDef =>
            new AST_Dot {
              fillTokens(this, symRef)
              expression = AST_SymbolRef.symDef(symRef)(clsSymDef)
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
  def inlineConstructorFunction(n: AST_Node): AST_Node = {
    // list constructor functions
    // XXXXX.prototype.constructor = XXXXX
    var constructorSymbols = Map.empty[SymbolDef, AST_Statement]

    object PrototypeConstructor {
      def unapply(arg: AST_Node) = arg match {
        case s@AST_SimpleStatement(AST_Assign(
        AST_SymbolRefDef(clsSym) AST_Dot "prototype" AST_Dot "constructor", "=", AST_SymbolRefDef(protoFunSym)
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
      case AST_Defun(
      Defined(AST_SymbolDef(fun)), args, Seq(AST_SimpleStatement(AST_Call(
      AST_SymbolRefDef(implementFun) AST_Dot "apply", _: AST_This, AST_SymbolRefName("arguments")
      )))) if constructorSymbols contains fun =>
        //println(s"Defined function ${fun.name} using ${implementFun.name}")
        implToConstructor += implementFun -> fun
        false
      case _ =>
        false
    }

    var constructorFunctionDefs = Map.empty[SymbolDef, AST_Defun]
    n.walk {
      case defun@AST_Defun(Defined(AST_SymbolDef(fun)), _, _) if implToConstructor contains fun =>
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
        case n@AST_SymbolRef(_, _, Defined(symDef)) =>
          implToConstructor.get(symDef).fold(node) { impl =>
            n.thedef = impl
            n.name = impl.name
            n
          }
        // inline XXXXX.apply(this, arguments) - was already rewritten from YYYYY (transformAfter transforms children first)
        case defun@AST_Defun(_, _, Seq(AST_SimpleStatement(AST_Call(
        AST_SymbolRefDef(symDef) AST_Dot "apply", _: AST_This, AST_SymbolRefName("arguments")
        )))) =>
          //println(s"Detect ${symDef.name}.apply")
          constructorToImpl.get(symDef).flatMap(constructorFunctionDefs.get).fold(node) { funDef =>
            //println(s"Found body of ${funDef.name.map(_.name)} in ${defun.name.map(_.name)}")
            defun.body = funDef.body.toJSArray
            defun
          }
        // remove the original implementation
        case defun@AST_Defun(Defined(AST_SymbolDef(sym)), _, _) if implToConstructor contains sym =>
          AST_EmptyStatement(defun)
        case _ => node
      }
    }
  }


  val transforms = Seq[AST_Extended => AST_Extended](
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
