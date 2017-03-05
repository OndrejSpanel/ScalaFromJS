package com.github.opengrabeso
import Transform._

import com.github.opengrabeso.Uglify._
import com.github.opengrabeso.UglifyExt._
import com.github.opengrabeso.UglifyExt.Import._

import scala.scalajs.js
import js.JSConverters._

object TransformClasses {
  import Transform.TypeDesc

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

  case class ClassFunMember(args: js.Array[AST_SymbolFunarg], body: js.Array[AST_Statement]) extends ClassMember

  case class ClassVarMember(value: AST_Node) extends ClassMember

  case class ClassDef(base: Option[String] = None, members: Map[String, ClassMember] = Map.empty)

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


  private def classList(n: AST_Extended) = {
    var classes = Map.empty[String, ClassDef]

    var classNames = Set.empty[TypeDesc]

    n.top.walk {
      // new XXX()
      case AST_New(AST_SymbolRefDef(call), _*) =>
        classNames += call.name
        false

      // any use of XXX.prototype probably marks a class
      case AST_Dot(AST_SymbolRef(name, _, _), "prototype") =>
        // TODO: heuristic detection of non-classes?
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
                  //println(s"Add var member ${kv.key}")
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

    n.top.walk {
      case _: AST_Toplevel =>
        false
      case ClassDefine(sym, args, body) =>
        // check if there exists a type with this name
        if (classNames contains sym.name) {
          // looks like a constructor
          //println("Constructor " + sym.name)
          val constructor = ClassFunMember(args, body)
          classes += sym.name -> ClassDef(members = Map("constructor" -> constructor))
        }
        true
      // TODO: detect Object.assign call as well
      case ClassMemberDef(name, funName, args, body) =>
        //println(s"Assign $name.$funName")
        for (clazz <- classes.get(name)) {
          val member = ClassFunMember(args, body)
          classes += name -> clazz.copy(members = clazz.members + (funName -> member))
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

  def apply(n: AST_Extended): AST_Extended = {
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

    val ret = deleteProtos.transformAfter { (node, walker) =>
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
            // TODO: resolve a symbol
            //println(s"${sym.name} extends ${clazz.base}")
            `extends` = clazz.base.fold(js.undefined: js.UndefOr[AST_Node]) { b =>
              new AST_SymbolRef {
                /*_*/
                fillTokens(this, defun) // TODO: tokens from a property instead
                /*_*/
                name = b
              }
            }
            properties = clazz.members.map { case (k, v) =>
              v match {
                case m: ClassFunMember =>
                  new AST_ConciseMethod {
                    // symbol lookup will be needed
                    key = new AST_SymbolRef {
                      fillTokens(this, defun) // TODO: tokens from a property instead
                      name = k
                    }
                    value = new AST_Accessor {
                      fillTokens(this, defun) // TODO: tokens from a property instead
                      argnames = m.args
                      this.body = m.body

                    }
                  }: AST_ObjectProperty
                case m: ClassVarMember =>
                  new AST_ConciseMethod {
                    // symbol lookup will be needed
                    key = new AST_SymbolRef {
                      fillTokens(this, defun) // TODO: tokens from a property instead
                      name = k
                    }
                    value = new AST_Accessor {
                      fillTokens(this, defun) // TODO: tokens from a property instead
                      argnames = js.Array()
                      this.body = js.Array(new AST_SimpleStatement {
                        fillTokens(this, defun)
                        body = m.value
                      })

                    }
                  }: AST_ObjectProperty
              }
            }.toJSArray
          }
        case _ =>
          node
      }
    }

    AST_Extended(ret, n.types)
  }


}