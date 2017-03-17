package com.github.opengrabeso
import Transform._
import Classes._
import Uglify._
import UglifyExt._
import UglifyExt.Import._

import scala.scalajs.js
import js.JSConverters._
import scala.collection.immutable.ListMap

object TransformClasses {

  object ClassDefine {
    def unapply(arg: AST_Node) = arg match {
      // function ClassName() {}
      case AST_Defun(Defined(sym), args, body) =>
        Some(sym, args, body)

      // ClassName = function() {}
      case AST_Assign(sym: AST_SymbolRef, "=", AST_Lambda(args, body)) =>
        Some(sym, args, body)

      // var ClassName = function() {}
      case AST_Var(AST_VarDef(sym: AST_Symbol, Defined(AST_Lambda(args, body)))) =>
        Some(sym, args, body)

      case _ =>
        //println(nodeClassName(arg))
        None
    }
  }

  sealed trait ClassMember

  case class ClassFunMember(args: js.Array[AST_SymbolFunarg], body: Seq[AST_Statement]) extends ClassMember

  case class ClassVarMember(value: AST_Node) extends ClassMember

  case class ClassDef(
    base: Option[String] = None,
    members: ListMap[String, ClassMember] = ListMap.empty,
    getters: ListMap[String, ClassFunMember] = ListMap.empty,
    setters: ListMap[String, ClassFunMember] = ListMap.empty
  )

  object ClassMemberDef {
    def unapply(arg: AST_Node) = arg match {
      case AST_SimpleStatement(AST_Assign(AST_Dot(AST_Dot(AST_SymbolRef(name, _, _), "prototype"), funName), "=", AST_Function(args, body))) =>
        Some(name, funName, args, body)
      case _ => None
    }
  }

  object ClassPropertyDef {
    def unapply(arg: AST_Node) = arg match {
      case AST_SimpleStatement(AST_Assign(AST_Dot(AST_Dot(AST_SymbolRef(name, _, _), "prototype"), funName), "=", value)) =>
        Some(name, funName, value)
      case _ => None
    }
  }

  object DefineProperties {

    object DefinePropertiesObject {
      def unapply(arg: AST_Node) = arg match {
        case AST_SimpleStatement(AST_Call(AST_SymbolRefName("Object") AST_Dot "defineProperties",
        AST_SymbolRef(name, _, _) AST_Dot "prototype", properties@_*)) =>
          Some(name, properties)

        case AST_SimpleStatement(AST_Call(AST_SymbolRefName("Object") AST_Dot "defineProperties",
        AST_SymbolRef(name, _, _), properties@_*)) =>
          Some(name, properties)

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
        Some(sym.name, prop.value, properties)
      case _ => None

    }
  }

  object ClassParentAndPrototypeDef {
    def unapply(arg: AST_Node) = arg match {
      // name.prototype = Object.assign( Object.create( sym.prototype ), {... prototype object ... } )
      case AST_SimpleStatement(
      AST_Assign(AST_Dot(AST_SymbolRef(name, _, _), "prototype"), "=",
      AST_Call(
      AST_Dot(AST_SymbolRefName("Object"), "assign"),
      AST_Call(AST_Dot(AST_SymbolRef("Object", _, _), "create"), AST_Dot(AST_SymbolRefDef(sym), "prototype")), prototypeDef: AST_Object)
      )) =>
        //println(s"ClassParentAndPrototypeDef $name extends ${sym.name}")
        Some(name, sym.name, prototypeDef)

      // Object.assign( name.prototype, sym.prototype, {prototype object} )
      case AST_SimpleStatement(AST_Call(
      AST_Dot(AST_SymbolRefName("Object"), "assign"),
      AST_Dot(AST_SymbolRefName(name), "prototype"),
      AST_Dot(AST_SymbolRef(sym, _, _), "prototype"),
      prototypeDef: AST_Object
      )) =>
        //println(s"ClassParentAndPrototypeDef2 $name extends $sym")
        Some(name, sym, prototypeDef)
      case _ =>
        None
    }

  }

  object ClassParentDef {
    def unapply(arg: AST_Node) = arg match {
      case AST_SimpleStatement(AST_Assign(AST_Dot(AST_SymbolRef(name, _, _), "prototype"), "=", AST_New(AST_SymbolRefDef(sym), _*))) =>
        Some(name, sym)

      // name.prototype = Object.create( sym.prototype );
      case AST_SimpleStatement(AST_Assign(
      AST_Dot(AST_SymbolRef(name, _, _), "prototype"), "=",
      AST_Call(AST_Dot(AST_SymbolRef("Object", _, _), "create"), AST_Dot(AST_SymbolRefDef(sym), "prototype"))
      )) =>
        Some(name, sym)

      case _ => None
    }
  }

  object ClassPrototypeDef {
    def unapply(arg: AST_Node) = arg match {

      //Object.assign( XXX.prototype, { ... })
      case AST_SimpleStatement(AST_Call(
      AST_Dot(AST_SymbolRefName("Object"), "assign"),
      AST_Dot(AST_SymbolRefName(name), "prototype"), prototypeDef: AST_Object
      )) =>
        //println(s"Match prototype def $name")
        Some(name,prototypeDef)

      case _ => None
    }
  }

  def keyNode(orig: AST_Node, k: String) = new AST_SymbolRef {
    fillTokens(this, orig)
    name = k
  }

  private def classList(n: AST_Extended) = {
    var classes = Map.empty[String, ClassDef]

    var classNames = Set.empty[String]

    n.top.walk {
      // new XXX()
      case AST_New(AST_SymbolRefDef(call), _*) =>
        classNames += call.name
        false

      // any use of XXX.prototype probably marks a class
      case AST_Dot(AST_SymbolRef(name, _, _), "prototype") =>
        classNames += name
        false

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


    def processPrototype(name: String, prototypeDef: AST_Object) = {
      for (clazz <- classes.get(name)) {
        classes += name -> prototypeDef.properties.foldLeft(clazz) { (clazz, m) =>
          //println(s"Property ${m.key}")
          m match {
            case kv: AST_ObjectKeyVal if kv.key != "constructor" =>

              val member: ClassMember = kv.value match {
                case AST_Function(args, body) =>
                  //println(s"Add fun member ${kv.key}")
                  ClassFunMember(args, body)
                case v =>
                  //println(s"Add var member ${kv.key} ${nodeClassName(v)}")
                  ClassVarMember(v)
              }
              //println("  " + member)
              clazz.copy(members = clazz.members + (kv.key -> member))
            case _ =>
              // prototype contains something other than a key: val pair - what to do with it?
              clazz
          }

        }
      }
    }

    def defineSingleProperty(name: String, key: String, p: AST_ObjectKeyVal) = {
      (p.value, p.key) match {
        case (fun: AST_Function, "get") =>
          //println(s"Add getter ${pp.key}")
          // new lookup needed for classes(name), multiple changes can be chained
          val c = classes(name)
          classes += name -> c.copy(getters = c.getters + (key -> ClassFunMember(fun.argnames, fun.body)))
        case (fun: AST_Function, "set") =>
          //println(s"Add setter ${pp.key}")
          val c = classes(name)
          classes += name -> c.copy(setters = c.setters + (key -> ClassFunMember(fun.argnames, fun.body)))
        //println(classes)
        case _ =>
      }
    }

    n.top.walk {

      case _: AST_Toplevel =>
        false
      case ClassDefine(sym, args, body) =>
        // check if there exists a type with this name
        if (classNames contains sym.name) {
          // looks like a constructor
          //println("Constructor " + sym.name)
          val constructor = ClassFunMember(args, body)
          classes += sym.name -> ClassDef(members = ListMap("constructor" -> constructor))
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
        defineSingleProperty(name, prop, property)
        true

      case DefineProperties(name, properties) if classes.contains(name) =>
        //println(s"Detected DefineProperties $name")
        properties.foreach {
          case AST_ObjectKeyVal(key, AST_Object(props)) =>
            //println(s"  property $key")
            props.foreach {
              case p: AST_ObjectKeyVal =>
                //println(s"  sub property ${p.key} ${nodeClassName(p.value)}")
                defineSingleProperty(name, key, p)

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
      case ClassParentDef(name, sym) =>
        for (clazz <- classes.get(name)) {
          classes += name -> clazz.copy(base = Some(sym.name))
        }
        true
      case ClassPrototypeDef(name, prototypeDef) =>
        processPrototype(name, prototypeDef)
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

    //println(classes)

    val deleteProtos = n.top.transformAfter { (node, _) =>
      node match {
        case t: AST_Block =>
          val newBody = t.body.filter {
            case ClassMemberDef(name, _, _, _) if classes.get(name).isDefined =>
              false
            case ClassPropertyDef(name, _, _) if classes.get(name).isDefined =>
              false
            case ClassParentDef(name, _) if classes.get(name).isDefined =>
              false
            case ClassPrototypeDef(_, _) =>
              false
            case ClassParentAndPrototypeDef(_, _, _) =>
              false
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
      node match {
        case defun@ClassDefine(sym, _, _) if classes contains sym.name =>

          //println(s"  ${walker.stack.map(nodeClassName).mkString("|")}")
          // check if there exists a type with this name
          val clazz = classes(sym.name)
          new AST_DefClass {
            fillTokens(this, defun)
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
            `extends` = clazz.base.fold(js.undefined: js.UndefOr[AST_Node]) { b =>
              new AST_SymbolRef {
                /*_*/
                fillTokens(this, defun)
                /*_*/
                name = b
              }
            }

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
              def unapply(arg: ClassMember) = arg match {
                case ClassFunMember(args, body) =>
                  Some(args.toSeq, body)

                case ClassVarMember(AST_BlockStatement(ss :+ ReturnValue(AST_Function(args, body)))) /*if onlyVariables(ss)*/ =>
                  //println(nodeClassName(f))
                  val newBody = ss ++ body
                  Some(args.toSeq, newBody)

                // some var members should also be converted to fun members
                // expected structure:
                // - variable prefix + function body

                case _ =>
                  None
              }
            }

            def newGetterOrSetter(node: AST_ObjectSetterOrGetter, k: String, args: js.Array[AST_SymbolFunarg], body: Seq[AST_Statement]) = {
              fillTokens(this, defun)
              node.key = keyNode(defun, k)
              node.value = new AST_Function {
                fillTokens(this, defun)
                argnames = args
                this.body = body.toJSArray
              }
              node
            }

            def newGetter(k: String, args: js.Array[AST_SymbolFunarg], body: Seq[AST_Statement]): AST_ObjectProperty = {
              newGetterOrSetter(new AST_ObjectGetter, k, args, body)
            }


            def newSetter(k: String, args: js.Array[AST_SymbolFunarg], body: Seq[AST_Statement]): AST_ObjectProperty = {
              newGetterOrSetter(new AST_ObjectSetter, k, args, body)
            }

            val mappedMembers = clazz.members.map { case (k, v) =>
              v match {
                case AsFunction(args, body) =>
                  new AST_ConciseMethod {
                    key = keyNode(defun, k)

                    value = new AST_Accessor {
                      fillTokens(this, defun)
                      argnames = args.toJSArray
                      this.body = body.toJSArray

                    }
                  }: AST_ObjectProperty

                case m: ClassVarMember =>
                  newGetter(k, js.Array(), js.Array(new AST_SimpleStatement {
                    fillTokens(this, defun)
                    body = m.value
                  }))

              }

            }

            val mappedGetters = clazz.getters.map {
              case (k, v) =>
                //println(s"newGetter $k")
                newGetter(k, v.args, v.body)
            }
            val mappedSetters = clazz.setters.map {
              case (k, v) =>
                //println(s"newSetter $k")
                newSetter(k, v.args, v.body)
            }

            properties = (mappedMembers ++ mappedGetters ++ mappedSetters).toJSArray
          }
        case DefineProperties(name, _) if classes.contains(name) =>
          new AST_EmptyStatement {
            fillTokens(this, node)
          }
        case DefineProperty(name, _, _) if classes.contains(name) =>
          new AST_EmptyStatement {
            fillTokens(this, node)
          }
        case _ =>
          node
      }
    }


    val cleanupClasses = createClasses.transformAfter { (node, walker) =>
      // find enclosing class (if any)
      def thisClass = walker.stack.collectFirst {
        case c: AST_DefClass =>
          //println(s"${c.name.get.name}")
          c
      }

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

      node match {
        // Animal.apply(this, Array.prototype.slice.call(arguments))
        case call@AST_Call(
        AST_SymbolRefName(IsSuperClass()) AST_Dot "apply",
        _: AST_This,
        AST_Call(AST_SymbolRefName("Array") AST_Dot "prototype" AST_Dot "slice" AST_Dot "call",args@_*)
        ) =>
          //println(s"Super constructor call in ${thisClass.map(_.name.get.name)}")
          call.expression = newAST_Super
          call.args = args.toJSArray
          call
        // Light.call( this, skyColor, intensity )
        case call@AST_Call(AST_SymbolRefName(IsSuperClass()) AST_Dot "call", args@_*) =>
          //println(s"Super constructor call in ${thisClass.map(_.name.get.name)}")
          call.expression = newAST_Super
          call.args = args.toJSArray
          call
        // Light.prototype.copy.call( this, source );
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
          call.args = args.toJSArray
          call
        case _ =>
          node
      }
    }


    AST_Extended(cleanupClasses, n.types)
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
            case AST_Dot(IsThis(), mem) =>
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
              fillTokens(this, node)
              definitions = js.Array(new AST_VarDef {
                name = new AST_SymbolVar {
                  fillTokens(this, node)
                  name = m
                }
              })
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

    AST_Extended(cleanup, n.types)
  }

  def inlineConstructors(n: AST_Extended): AST_Extended = {
    val ret = n.top.transformAfter { (node, _) =>
      node match {
        case cls: AST_DefClass =>

          for (constructorProperty@AST_ConciseMethod(_, constructor: AST_Lambda) <- findConstructor(cls)) {
            // anything before a first variable declaration can be inlined, variables need to stay private
            val (inlined, rest) = constructor.body.span {
              case _: AST_Var => false
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
                    sym.name = sym.name + SymbolTypes.parSuffix
                    sym
                  // do not inline call, we need this.call form for the inference
                  // on the other hand form without this is better for variable initialization
                  case AST_Dot(_: AST_This, member) if !transformer.parent().isInstanceOf[AST_Call] =>
                    new AST_SymbolRef {
                      fillTokens(this, node)
                      name = member
                    }
                  case _ =>
                    node
                }
              }
            }
            // add adjusted constructor argument names so that parser correctly resolves them inside of the function
            val accessor = classInlineBody(cls)
            accessor.argnames = constructor.argnames.map { p =>
              val a = p.clone()
              a.name = p.name + SymbolTypes.parSuffix
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

    AST_Extended(ret, n.types)
  }

  val transforms = Seq(
    convertProtoClasses _,
    fillVarMembers _,
    inlineConstructors _
  )
}
