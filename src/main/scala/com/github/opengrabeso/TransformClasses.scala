package com.github.opengrabeso
import Transform._
import Classes._
import Uglify._
import UglifyExt._
import JsUtils._
import UglifyExt.Import._

import scala.scalajs.js
import js.JSConverters._
import scala.collection.immutable.ListMap
import scala.language.implicitConversions
import scala.scalajs.js.RegExp

object TransformClasses {
  import Symbols._

  case class ClassId(name: String, pos: Int)

  object ClassId {
    // TODO: avoid get, use something safe instead
    def apply(sym: SymbolDef): ClassId = {
      ClassId(sym.name, SymbolTypes.id(sym).get.sourcePos)
    }
    def apply(sym: AST_Symbol): ClassId = ClassId(sym.thedef.get)
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
  )

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

  object DefineStaticMembers {
    //def unapply(arg: AST_Node): Option[(AST_SymbolVarOrConst, AST_Object)] = None

    def unapply(arg: AST_Node) = arg match {
      case AST_Definitions(AST_VarDef(sym: AST_Symbol, Defined(objDef: AST_Object))) =>
        //println(s"Detect static class definition ${sym.name}")
        Some(sym, objDef)
      case _ =>
        None
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
        // TODO: name extracted, but unused - verify it
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
        Some(ClassId(sym), ClassId(sym), prototypeDef)
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


  class ClassList {

    var classes = Map.empty[ClassId, ClassDef]

    def += (kv: (ClassId, ClassDef)): Unit = classes += kv

    def defClass(name: ClassId): ClassDef = classes.getOrElse(name, new ClassDef)

    def defineSingleProperty(name: ClassId, key: String, p: AST_ObjectKeyVal) = {
      (p.value, p.key) match {
        case (fun: AST_Function, "get") =>
          //println(s"Add getter ${pp.key}")
          // new lookup needed for classes(name), multiple changes can be chained
          val c = defClass(name)
          classes += name -> c.copy(getters = c.getters + (key -> ClassFunMember(fun.argnames, fun.body)))
        case (fun: AST_Function, "set") =>
          //println(s"Add setter ${pp.key}")
          val c = defClass(name)
          classes += name -> c.copy(setters = c.setters + (key -> ClassFunMember(fun.argnames, fun.body)))
        //println(classes)
        case (value, "value") =>
          //println(s"Add value $key")
          val c = defClass(name)
          classes += name -> c.copy(values = c.values + (key -> ClassVarMember(value)))

        case _ =>
          //println("Other property " + nodeClassName(p))

      }
    }

    def defineStaticMember(name: ClassId, key: String, value: AST_Node) = {
      //println(s"defineStaticMember $name.$key ${ScalaOut.outputNode(value)}")
      val c = defClass(name)
      if (!(c.membersStatic contains key)) {
        val member = value match {
          case AST_Function(args, body) =>
            //println(s"Define static fun $key")
            ClassFunMember(args, body)
          case _ =>
            //println(s"Define static var $key")
            ClassVarMember(value)
        }
        classes += name -> c.copy(membersStatic = c.membersStatic + (key -> member))
      }

    }

  }

  implicit def classesFromClassList(cl: ClassList):  Map[ClassId, ClassDef] = cl.classes

  private def classList(n: AST_Extended) = {
    var classes = new ClassList

    var classNames = Set.empty[ClassId]

    n.top.walkWithDescend { (node, _, walker) =>
      node match {
        // new XXX()
        case AST_New(AST_SymbolRefDef(call), _*) =>
          classNames += ClassId(call)
          false

        // any use of XXX.prototype probably marks a class
        case AST_SymbolRefDef(name) AST_Dot "prototype" =>
          classNames += ClassId(name)
          false

        /* the rule did more harm than good - functions are sometimes defined externally
        // use of this in a function most likely means the function is a constructor
        case (_: AST_This) AST_Dot _ =>
          for {
            fun <- walker.stack.reverse.collectFirst { case c: AST_Lambda => c }
            sym <- fun.name.nonNull
            Some(sym: AST_SymbolDefun) <- sym.thedef.nonNull.map(_.orig.headOption)
          } {
            //println(s"Detected class ${sym.name}")
            classNames += sym.name
          }
          false
        */

        /*
      // XXXX.prototype = ...;
      case AST_SimpleStatement(AST_Assign(AST_Dot(AST_SymbolRef(name, _, _), "prototype"), "=", _)) =>
        classNames += name
        false

      // Object.create(XXXX.prototype)
      case AST_Call(AST_Dot(AST_SymbolRef("Object", _, _), "create"), AST_Dot(AST_SymbolRef(name, _, _), "prototype")) =>
        classNames += name
        false
      */

        case x =>
          //println(nodeClassName(x))
          false
      }
    }


    def processPrototype(name: ClassId, prototypeDef: AST_Object, isStatic: Boolean = false) = {
      val clazz = classes.getOrElse(name, ClassDef(staticOnly = isStatic))

      classes += name -> prototypeDef.properties.foldLeft(clazz) { (clazz, m) =>
        //println(s"Property ${m.key}")
        val key = propertyName(m)
        // constructor key most often references the constructor function - this is handled separately
        if (key == "constructor") {
          clazz
        } else {
          // TODO: static getters / setters?
          def addMember(member: ClassMember) = {
            if (!isStatic) clazz.copy(members = clazz.members + (key -> member))
            else clazz.copy(membersStatic = clazz.membersStatic + (key -> member))
          }
          def addGetter(member: ClassFunMember) = clazz.copy(getters = clazz.getters + (key -> member))
          def addSetter(member: ClassFunMember) = clazz.copy(setters = clazz.setters + (key -> member))

          m match {
            case kv: AST_ObjectKeyVal =>
              //println(s"Add AST_ObjectKeyVal $key")
              if (isStatic) {
                addMember(ClassVarMember(kv.value))
              } else {
                kv.value match {
                  case AST_Function(args, body) =>
                    //println(s"Add fun member ${kv.key}")
                    addMember(ClassFunMember(args, body))
                  case v =>
                    //println(s"Add var member ${kv.key} ${nodeClassName(v)}")
                    addMember(ClassVarMember(v))
                }
              }
            case m: AST_ConciseMethod =>
              //println(s"Add AST_ConciseMethod $key")
              addMember(ClassFunMember(m.value.argnames, m.value.body))
            case p: AST_ObjectGetter =>
              assert(!isStatic)
              addGetter(ClassFunMember(p.value.argnames, p.value.body))
            case p: AST_ObjectSetter =>
              assert(!isStatic)
              addSetter(ClassFunMember(p.value.argnames, p.value.body))

            case _ =>
              // prototype contains something other than a key: val pair - what to do with it?
              val member = unsupported(s"Unsupported property type ${nodeClassName(m)}", m.value, Some(m.value))
              addMember(ClassVarMember(member))
          }
        }
      }
    }

    def removeStaticMember(name: ClassId, member: String) = {
      for (cls <- classes.get(name)) {
        classes += name -> cls.copy(membersStatic = cls.membersStatic - member)
      }
    }

    n.top.walk {

      case _: AST_Toplevel =>
        false
      case ClassDefine(sym, args, body) =>
        val clsId = ClassId(sym)
        // check if there exists a type with this name
        if (classNames contains clsId) {
          // looks like a constructor
          //println("Constructor " + sym.name)

          // constructor may contain property definitions
          // note: we do not currently handle property definitions in any inner scopes
          val bodyFiltered = body.filter {
            //Object.defineProperty( this, 'id', { value: textureId ++ } );
            case AST_SimpleStatement(AST_Call(
            AST_SymbolRefName("Object") AST_Dot "defineProperty", _: AST_This, prop: AST_String,
            AST_Object(properties)
            )) =>
              //println(s"Detect defineProperty ${sym.name}.${prop.value}")
              properties.foreach {
                case p: AST_ObjectKeyVal => classes.defineSingleProperty(ClassId(sym), prop.value, p)
                case _ =>
              }
              false
            case _ =>
              true
          }

          val constructor = ClassFunMember(args, bodyFiltered)

          val c = classes.defClass(ClassId(sym))
          classes += ClassId(sym) -> c.copy(members = c.members + ("constructor" -> constructor))
        }

        true
      case ClassMemberDef(name, funName, args, body) =>
        //println(s"Assign $name.$funName")
        for (clazz <- classes.get(name)) {
          val member = ClassFunMember(args, body)
          classes += name -> clazz.copy(members = clazz.members + (funName -> member))
        }
        true

      case DefineProperty(name, prop, Seq(property: AST_ObjectKeyVal)) if classes.contains(name) =>
        //println(s"Detected DefineProperty $name.$prop  ${nodeClassName(property)}")
        classes.defineSingleProperty(name, prop, property)
        true

      case DefineProperties(name, properties) if classes.contains(name) =>
        //println(s"Detected DefineProperties $name")
        properties.foreach {
          case AST_ObjectKeyVal(key, AST_Object(props)) =>
            //println(s"  property $key")
            props.foreach {
              case p: AST_ObjectKeyVal =>
                //println(s"  sub property ${p.key} ${nodeClassName(p.value)}")
                classes.defineSingleProperty(name, key, p)

              case _ =>
            }
          case _ =>
        }

        true
      case ClassPropertyDef(name, propName, value) =>
        //println(s"Assign $name.$funName")
        if (propName != "constructor") { // constructor is most often assigned a constructor function
          for (clazz <- classes.get(name)) {
            val member = ClassVarMember(value)
            classes += name -> clazz.copy(members = clazz.members + (propName -> member))
          }
        }
        true
      case ClassParentAndPrototypeDef(name, sym, prototypeDef) =>
        for (clazz <- classes.get(name)) {
          //println(s"base $sym")
          classes += name -> clazz.copy(base = Some(sym))
        }
        processPrototype(name, prototypeDef)
        true
      case DefineStaticMembers(clsName, objDef) =>
        processPrototype(ClassId(clsName), objDef, true)
        true
      case ClassParentDef(name, sym) =>
        for (clazz <- classes.get(name)) {
          classes += name -> clazz.copy(base = Some(ClassId(sym)))
        }
        true
      case ClassPrototypeDef(name, prototypeDef) =>
        processPrototype(name, prototypeDef)
        true

      // note: after ClassPrototypeDef, as  prototype definition would match static member definition as well
      case DefineStaticMember(clsName, clsSym, scope, member, value) =>
        // is it the same scope as the class definition? If not, do not consider it as a static init
        //println(s"Nesting ${scope.nesting} .. ${clsSym.scope.nesting}")
        if (scope == clsSym.scope) {
          //println(s"Define static member $clsName.$member as ${nodeClassName(value)}")
          classes.defineStaticMember(clsName, member, value)
        }
        true

      case _ =>
        false
    }
    //println(classNames)
    //println(classes)

    classes
  }

  def convertProtoClasses(n: AST_Extended): AST_Extended = {
    // for any class types try to find constructors and prototypes and try to transform them
    // start with global classes (are local classes even used in JS?)

    val classes = classList(n)


    //println(classes.classes)

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

    val deleteProtos = n.top.transformAfter { (node, transformer) =>
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
            case DefineStaticMember(name, _, _, member, statement)  =>
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
              fillTokens(this, sym)
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

      node match {
        case ClassDefine(sym, _, _) if classes contains ClassId(sym) =>
          val clsId = ClassId(sym)
          //println(s"  ${walker.stack.map(nodeClassName).mkString("|")}")
          // check if there exists a type with this name
          val clazz = classes(clsId)

          object helper extends Helper(node)
          import helper._

          val base = clazz.base.fold(js.undefined: js.UndefOr[AST_Node])(b => AST_SymbolRef(node)(b.name))

          val mappedMembers = clazz.members.map { case (k, v) => newMember(k, v) }
          val mappedGetters = clazz.getters.map { case (k, v) => newGetter(k, v.args, v.body) }
          val mappedSetters = clazz.setters.map { case (k, v) => newSetter(k, v.args, v.body) }
          val mappedValues = clazz.values.map { case (k, v) => newValue(k, v.value, false) }
          val mappedStatic = clazz.membersStatic.map { case (k, v) => newMember(k, v, true) }

          val properties = mappedMembers ++ mappedGetters ++ mappedSetters ++ mappedValues ++ mappedStatic
          newClass(sym, base, properties)

        case DefineStaticMembers(sym, _) if classes.contains(ClassId(sym)) =>
          val clsId = ClassId(sym)
          val clazz = classes(clsId)

          object helper extends Helper(node)
          import helper._

          val mappedStatic = clazz.membersStatic.map { case (k, v) => newMember(k, v, true) }

          val markerBase = AST_SymbolRef(node)("static_^")

          newClass(sym, markerBase, mappedStatic)

        case DefineProperties(name, _) if classes.contains(name) =>
          emptyNode
        case DefineProperty(name, _, _) if classes.contains(name) =>
          emptyNode
        //case DefineStaticMember(name, member, _) if verifyStaticMemberOnce(name, member) =>
        //  emptyNode
        case _ =>
          node
      }
    }

    val cleanupClasses = createClasses.transformAfter { (node, walker) =>
      // find enclosing class (if any)
      def thisClass = findThisClassInWalker(walker)

      object IsSuperClass {
        def unapply(name: String): Boolean = {
          //println(s"${thisClass.flatMap(superClass)} $name")
          thisClass.flatMap(superClass).contains(name)
        }
      }

      def newAST_Super = new AST_Super {
        fillTokens(this, node)
        name = "super"
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
        AST_SymbolRefName(IsSuperClass()) AST_Dot "apply",
        _: AST_This,
        AST_Call(AST_SymbolRefName("Array") AST_Dot "prototype" AST_Dot "slice" AST_Dot "call", AST_SymbolRefName("arguments"))
        ) =>
          //println(s"Super constructor call in ${thisClass.map(_.name.get.name)}")
          call.expression = newAST_Super
          call.args = getClassArguments.toJSArray
          call
        // Super.apply(this, arguments)
        case call@AST_Call(AST_SymbolRefName(IsSuperClass()) AST_Dot "apply", _: AST_This, AST_SymbolRefName("arguments")) =>
          // TODO: check class constructor arguments as pass them
          call.expression = newAST_Super
          call.args = getClassArguments.toJSArray
          call

        // Light.call( this, skyColor, intensity )
        case call@AST_Call(AST_SymbolRefName(IsSuperClass()) AST_Dot "call", args@_*) =>
          //println(s"Super constructor call in ${thisClass.map(_.name.get.name)}")
          call.expression = newAST_Super
          call.args = removeHeadThis(args).toJSArray
          call


        // Light.prototype.call( this, source );
        case call@AST_Call(
        AST_SymbolRefName(IsSuperClass()) AST_Dot "prototype" AST_Dot func AST_Dot "call",
        _: AST_This, args@_*
        ) =>
          //println(s"Super call of $func in ${thisClass.map(_.name.get.name)}")
          call.expression = new AST_Dot {
            fillTokens(this, node)
            expression = newAST_Super
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

    n.copy(top = cleanupClasses)
  }

  def fillVarMembers(n: AST_Extended): AST_Extended = {
    object IsThis {
      def unapply(arg: AST_Node) = arg match {
        case _: AST_This => true
        case AST_SymbolRef("this", _, _) => true // not used in practice, AST_This seems to catch all
        case _ => false
      }
    }

    // TODO: detect access other than this (see AST_This in expressionType to check general this handling)
    val ret = n.top.transformAfter { (node, _) =>
      node match {
        case cls: AST_DefClass =>
          var newMembers = Seq.empty[String]
          // scan known prototype members (both function and var) first
          var existingMembers = listPrototypeMemberNames(cls)

          cls.walk {
            case IsThis() AST_Dot mem =>
              //println(s"Detect this.$mem")
              if (!existingMembers.contains(mem)) {
                newMembers = newMembers :+ mem
                existingMembers = existingMembers :+ mem
              }
              false
            case _ =>
              false
          }

          val vars = newMembers.map { m =>
            new AST_Var {
              fillTokens(this, cls)
              definitions = js.Array(AST_VarDef.uninitialized(node) (m))
            }
          }

          val accessor = classInlineBody(cls)
          accessor.body = (vars: Seq[AST_Statement]).toJSArray

          cls
        case _ =>
          node
      }
    }

    val classInfo = listClassMembers(ret)
    //println(classInfo)

    // remove members already present in a parent from a derived class
    val cleanup = ret.transformAfter { (node, _) =>
      node match {
        case cls@AST_DefClass(Defined(AST_SymbolName(clsName)), _,_) =>
          for (AST_SymbolName(base) <- cls.`extends`) {
            //println(s"Detected base $base")
            val accessor = classInlineBody(cls)
            accessor.body = accessor.body.filter {
              case VarName(member) => classInfo.classContains(base, member).isEmpty
              case _ => true
            }
          }
          cls
        case _ =>
          node
      }
    }

    n.copy(top = cleanup)
  }

  def inlineConstructors(n: AST_Node): AST_Node = {
    n.transformAfter { (node, _) =>
      node match {
        case cls: AST_DefClass =>

          for (constructorProperty@AST_ConciseMethod(_, constructor: AST_Lambda) <- findConstructor(cls)) {
            // anything before a first variable declaration can be inlined, variables need to stay private
            val (inlined, rest) = constructor.body.span {
              case _: AST_Definitions => false
              case _ => true
            }
            //println(s"inlined ${inlined.map(nodeClassName)}")
            //println(s"rest ${rest.map(nodeClassName)}")
            // transform parameter names while inlining (we need to use parSuffix names)
            val parNames = constructor.argnames.map(_.name)
            val parNamesSet = parNames.toSet
            object IsParameter {
              def unapply(arg: String): Boolean = parNamesSet contains arg
            }
            val parNamesAdjusted = inlined.map { s =>
              s.transformAfter { (node, transformer) =>
                node match {
                  case sym@AST_SymbolName(IsParameter()) =>
                    sym.name = sym.name + parSuffix
                    sym
                  // do not inline call, we need this.call form for the inference
                  // on the other hand form without this is better for variable initialization
                  case (_: AST_This) AST_Dot member if !transformer.parent().isInstanceOf[AST_Call] =>
                    AST_SymbolRef(node)(member)
                  case _ =>
                    node
                }
              }
            }
            // add adjusted constructor argument names so that parser correctly resolves them inside of the function
            val accessor = classInlineBody(cls)
            accessor.argnames = constructor.argnames.map { p =>
              val a = p.clone()
              a.name = p.name + parSuffix
              a
            }
            accessor.body = accessor.body ++ parNamesAdjusted
            if (rest.nonEmpty) {
              constructor.body = rest
            } else {
              cls.properties = cls.properties -- Seq(constructorProperty)
            }
          }

          cls
        case _ =>
          node
      }
    }
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
    n.config.rules.foldLeft(n)((n, rule) => rule(n))

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

  def deleteMembers(n: AST_Extended, member: ConvertProject.MemberDesc) = {
    processAllClasses(n, Some(member.cls)) { c =>
      val ret = c.clone()
      // filter member functions and properties
      ret.properties = c.properties.filterNot(p => member.name.test(propertyName(p)))

      deleteVarMember(ret, member.name)
    }

  }

  def makeProperties(n: AST_Extended, member: ConvertProject.MemberDesc) = {
    TransformClasses.processAllClasses(n, Some(member.cls)) { c =>

      // search constructor for a property definition
      val applied = for (constructor <- Classes.findConstructor(c)) yield {
        val cc = c.clone()

        deleteVarMember(cc, member.name)

        object CheckPropertyInit {
          def unapply(arg: AST_Node): Option[AST_Node] = arg match {
            case c: AST_Constant =>
              Some(c)
            case o: AST_Object =>
              // TODO: check if values are acceptable (no dependencies on anything else then parameters)
              Some(o)
            case _ =>
              None

          }
        }
        object MatchName {
          def unapply(arg: String): Option[String] = {
            if (member.name.test(arg)) Some(arg)
            else None
          }
        }

        val newC = constructor.transformAfter { (node, transformer) =>
          node match {
            case AST_SimpleStatement(AST_Assign(AST_This() AST_Dot MatchName(prop), "=", CheckPropertyInit(init))) =>
              //println(s"Found property definition ${nodeClassName(init)}")
              val ss = AST_SimpleStatement(init) {
                Classes.transformClassParameters(c, init.clone())
              }
              cc.properties += newMethod(prop, Seq(), Seq(ss), init)
              AST_EmptyStatement(node)
            case _ =>
              node
          }
        }

        Classes.replaceProperty(cc, constructor, newC)
      }

      applied.getOrElse(c)
    }

  }

  def replaceIsClass(n: AST_Extended, member: ConvertProject.MemberDesc): AST_Extended = {
    // first scan for all symbols matching the rule
    var isClassMembers = Map.empty[String, AST_DefClass]

    n.top.walk {
      case cls@AST_DefClass(Defined(AST_SymbolName(cName)), _, _) if member.cls test cName =>
        val matching = cls.properties
          .collect{
            // we expect getter, no parameters, containing a single true statement
            case AST_ObjectGetter(AST_SymbolName(name), AST_Function(Seq(), Seq(AST_SimpleStatement(_: AST_True)))) =>
              name
          }.filter { n =>
            val matched = member.name.exec(n)
            if (matched == null || matched.length < 2) false
            else {
              // only when class name matches the first match group
              matched.lift(1).exists(_.contains(cName))
            }
          }.toSet

        isClassMembers ++= matching.map(_ -> cls)
        true
      case _ =>
        false
    }

    //println(s"Detected isClass members $isClassMembers")

    object GetClass {
      def unapply(arg: String): Option[AST_DefClass] = isClassMembers.get(arg)
    }

    val ret = n.top.transformAfter { (node, _) =>
      node match {
        case callOn AST_Dot GetClass(AST_DefClass(Defined(AST_SymbolName(prop)), _, _)) =>
          //println(s"Detect call $prop")
          AST_Binary(node) (callOn, instanceof, AST_SymbolRef(node)(prop))
      case _ =>
          node
      }
    }

    n.copy(top = ret)
  }


  val transforms = Seq[AST_Extended => AST_Extended](
    onTopNode(inlinePrototypeVariables),
    onTopNode(inlineConstructorFunction),
    convertProtoClasses,
    fillVarMembers,
    // applyRules after fillVarMembers - we cannot delete members before they are created
    // applyRules before inlineConstructors, so that constructor is a single function
    applyRules,
    onTopNode(inlineConstructors)
  )
}
