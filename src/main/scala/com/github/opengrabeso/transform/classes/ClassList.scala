package com.github.opengrabeso
package transform
package classes

import Classes._
import net.gamatron.esprima._
import esprima._

import SymbolTypes.SymbolMapId

import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

object ClassList {

  implicit def classesFromClassList(cl: ClassList):  Map[ClassId, ClassDef] = cl.classes

  private [classes] def apply(n: Node.Node): ClassList = {
    var classes = new ClassList
    var classNames = Set.empty[ClassId]

    def createClassAsNeeded(clsId: ClassId, isStatic: Boolean = false): Option[ClassDef] = {
      val clazz = classes.get(clsId)
      clazz.orElse {
        // safety check: check if the class really should exists
        if (classNames contains clsId) {
          val c = ClassDef(staticOnly = isStatic)
          classes += clsId -> c
          Some(c)
        } else {
          None
        }
      }
    }

    n.walk {
      // new XXX()
      case `n` => // the scope itself should never be considered a symbol (can be Node.DefClass)
        false
      case Node.New(Node.SymbolRefDef(call), _*) =>
        //println(s"Node.New ${call.name}")
        classNames += ClassId(call)
        false

      // any use of XXX.prototype probably marks a class
      case Node.SymbolRefDef(name) Node.StaticMemberExpression "prototype" =>
        //println(s"Node.SymbolRefDef ${name.name}")
        classNames += ClassId(name)
        false
      case Node.DefClass(Defined(name), _, _) =>
        //println(s"Node.DefClass ${name.name}")
        classNames += ClassId(name)
        true

      /* the rule did more harm than good - functions are sometimes defined externally
      // use of this in a function most likely means the function is a constructor
      case (_: Node.This) Node.StaticMemberExpression _ =>
        for {
          fun <- walker.stack.reverse.collectFirst { case c: Node.Lambda => c }
          sym <- fun.name.nonNull
          Some(sym: Node.SymbolDefun) <- sym.thedef.nonNull.map(_.orig.headOption)
        } {
          //println(s"Detected class ${sym.name}")
          classNames += sym.name
        }
        false
      */

      case x =>
        //println(nodeClassName(x))
        false
    }

    if (false) if (classNames.nonEmpty) {
      println(s"class names $classNames in $n")
    }


    def processPrototype(name: ClassId, prototypeDef: Node.Object, isStatic: Boolean = false) = {
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
            case kv: Node.ObjectKeyVal =>
              //println(s"Add Node.ObjectKeyVal $key")
              if (isStatic) {
                addMember(ClassVarMember(kv.value))
              } else {
                kv.value match {
                  case Node.Function(args, body) =>
                    //println(s"Add fun member ${kv.key}")
                    addMember(ClassFunMember(args, body))
                  case v =>
                    //println(s"Add var member ${kv.key} ${nodeClassName(v)}")
                    addMember(ClassVarMember(v))
                }
              }
            case m: Node.ConciseMethod =>
              //println(s"Add Node.ConciseMethod $key")
              addMember(ClassFunMember(m.value.argnames, m.value.body))
            case p: Node.ObjectGetter =>
              assert(!isStatic)
              addGetter(ClassFunMember(p.value.argnames, p.value.body))
            case p: Node.ObjectSetter =>
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

    def processBody(clsId: SymbolMapId, body: Seq[Node.Statement]) = {
      // constructor may contain property definitions
      // note: we do not currently handle property definitions in any inner scopes
      val bodyFiltered = body.filter {
        //Object.defineProperty( this, 'id', { value: textureId ++ } );
        case Node.SimpleStatement(Node.Call(
        Node.SymbolRefName("Object") Node.StaticMemberExpression "defineProperty", _: Node.This, prop: Node.String,
        Node.Object(properties)
        )) =>
          //println(s"Detect defineProperty ${sym.name}.${prop.value}")
          properties.foreach {
            case p: Node.ObjectKeyVal => classes.defineSingleProperty(clsId, prop.value, p)
            case _ =>
          }
          false
        case _ =>
          true
      }
      bodyFiltered
    }

    n.walk {

      case `n` => // always enter into the scope we are processing
        false
      case defun@ClassDefineValue(Node.SymbolDef(symDef), args, body, proto) =>
        for {
          clsId <- SymbolTypes.id(symDef) if classNames contains clsId
        } {

          val funScope = defun
          //println(s"Constructor ${symDef.name} returning value, funScope $funScope")

          def transformReferences[T <: Node.Node](n: T): T = {
            n.transformAfter { (node, _) =>
              node match {
                case Node.SymbolRef(name, _, Defined(symbolDef)) =>
                  //println(s"Check reference $name, ${symbolDef.scope}")
                  if (funScope == symbolDef.scope) {
                    //println(s"Transform reference $name")

                    new Node.StaticMemberExpression {
                      fillTokens(this, node)
                      expression = Node.This().withTokens(node)
                      property = name
                    }
                  } else node
                case _ =>
                  node
              }
            }
          }
          def transformReferencesInBody(body: Seq[Node.Statement]): Seq[Node.Statement] = {
            body.map(transformReferences)
          }

          // scan for value and member definitions

          //val c = classes.defClass(clsId)
          object res {
            var clazz = new ClassDef
            val body = ArrayBuffer.empty[Node.Statement]
          }

          body.foreach {
            case Node.Defun(Defined(Node.SymbolDef(sym)), fArgs, fBody) =>
              val member = ClassFunMember(fArgs, transformReferencesInBody(fBody))
              res.clazz = res.clazz.addMember(sym.name, member)
            case Node.Definitions(vars@_*) =>
              //println("Vardef")
              vars.foreach {
                case Node.VarDef(Node.SymbolName(vName), Defined(vValue)) =>
                  val member = ClassVarMember(vValue)
                  res.clazz = res.clazz.addValue(vName, member)
                case vd@Node.VarDef(Node.SymbolName(vName), _) =>
                  //println(s"value member $vName as undefined")
                  val member = ClassVarMember(Node.EmptyStatement(vd))
                  res.clazz = res.clazz.addValue(vName, member)
              }
            case s =>
              //println(nodeClassName(s))
              res.body.append(s)
          }


          proto.foreach {
            case Node.ObjectKeyVal(name, value) =>
              //println(s"$name, $value")
              value match {
                case Node.SymbolName(`name`) => // same name, no need for any action
                case Node.SymbolName(other) =>
                  res.clazz = res.clazz.renameMember(other, name)
                case Node.Lambda(fArgs, fBody) =>
                  val member = ClassFunMember(fArgs, transformReferencesInBody(fBody))
                  res.clazz = res.clazz.addMember(name, member)
                case _ =>
                // TODO: we should include only the ones used by the return value - this may include some renaming
                // TODO: the unused ones should be marked private
              }
            case _ =>
          }

          // transform args to be in the class namespace (constructor symbol)
          val argsTransformed = args.map { arg =>
            val t = arg.clone()
            fillTokens(t, defun)
            t
          }
          val constructor = ClassFunMember(argsTransformed, res.body)
          classes += clsId -> res.clazz.addMember(inlineBodyName, constructor)
        }
        true

      case ClassDefine(Node.SymbolDef(symDef), args, body) =>
        for (clsId <- SymbolTypes.id(symDef) if classNames contains clsId ) {
          // looks like a constructor
          //println("Constructor " + symDef.name)

          val bodyFiltered = processBody(clsId, body)

          val constructor = ClassFunMember(args, bodyFiltered)

          val c = classes.defClass(clsId)
          classes += clsId -> c.copy(members = c.members + ("constructor" -> constructor))
        }

        true
      case ClassMemberDef(name, funName, args, body) =>
        //println(s"Assign member $name.$funName")
        for (clazz  <- createClassAsNeeded(name)) {
          val member = ClassFunMember(args, body)
          classes += name -> clazz.copy(members = clazz.members + (funName -> member))
        }
        true

      case DefineProperty(name, prop, Seq(property: Node.ObjectKeyVal)) if classes.contains(name) =>
        //println(s"Detected DefineProperty $name.$prop  ${nodeClassName(property)}")
        classes.defineSingleProperty(name, prop, property)
        true

      case DefineProperties(name, properties) if classes.contains(name) =>
        //println(s"Detected DefineProperties $name")
        properties.foreach {
          case Node.ObjectKeyVal(key, Node.Object(props)) =>
            //println(s"  property $key")
            props.foreach {
              case p: Node.ObjectKeyVal =>
                //println(s"  sub property ${p.key} ${nodeClassName(p.value)}")
                classes.defineSingleProperty(name, key, p)

              case _ =>
            }
          case _ =>
        }

        true
      case ClassPropertyDef(name, propName, value) =>
        //println(s"Assign property $name.$propName")
        if (propName != "constructor") { // constructor is most often assigned a constructor function
          for (clazz <- createClassAsNeeded(name)) {
            val member = ClassVarMember(value)
            classes += name -> clazz.copy(members = clazz.members + (propName -> member))
          }
        }
        true
      case ClassParentAndPrototypeDef(name, sym, prototypeDef) =>
        for (clazz <- createClassAsNeeded(name)) {
          //println(s"ClassParentAndPrototypeDef $name base $sym")
          classes += name -> clazz.copy(base = Some(sym))
        }
        processPrototype(name, prototypeDef)
        true
      case ClassParentDef(name, sym) =>
        for (clazz <- createClassAsNeeded(name)) {
          //println(s"ClassParentDef $name base $sym")
          classes += name -> clazz.copy(base = Some(ClassId(sym)))
        }
        true
      case ClassPrototypeDef(name, prototypeDef) =>
        processPrototype(name, prototypeDef)
        true

      // note: after ClassPrototypeDef, as  prototype definition would match static member definition as well
      case DefineStaticMember(clsName, clsSym, scope, member, value) if classNames contains clsName =>
        // is it the same scope as the class definition? If not, do not consider it as a static init
        //println(s"Nesting ${scope.nesting} .. ${clsSym.scope.nesting}")
        if (scope == clsSym.scope) {
          //println(s"Define static member $clsName.$member as ${nodeClassName(value)}")
          classes.defineStaticMember(clsName, member, value)
        }
        true
      case Node.DefClass(Defined(clsSym), _, _) =>
        createClassAsNeeded(ClassId(clsSym))
        true
      case _: Node.Scope =>
        // do not enter any other scopes, they will be handled by recursion
        true

      case _ =>
        false
    }

    //println(s"classNames $classNames")
    //println(s"classes ${classes.classes}")

    classes
  }


}

class ClassList {

  var classes = Map.empty[ClassId, ClassDef]

  def += (kv: (ClassId, ClassDef)): Unit = classes += kv

  def defClass(name: ClassId): ClassDef = classes.getOrElse(name, new ClassDef)

  def defineSingleProperty(name: ClassId, key: String, p: Node.ObjectKeyVal) = {
    (p.value, p.key) match {
      case (fun: Node.Function, "get") =>
        //println(s"Add getter ${pp.key}")
        // new lookup needed for classes(name), multiple changes can be chained
        val c = defClass(name)
        classes += name -> c.copy(getters = c.getters + (key -> ClassFunMember(fun.argnames, fun.body)))
      case (fun: Node.Function, "set") =>
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

  def defineStaticMember(name: ClassId, key: String, value: Node.Node) = {
    //println(s"defineStaticMember $name.$key ${ScalaOut.outputNode(value)}")
    val c = defClass(name)
    if (!(c.membersStatic contains key)) {
      val member = value match {
        case Node.Function(args, body) =>
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
