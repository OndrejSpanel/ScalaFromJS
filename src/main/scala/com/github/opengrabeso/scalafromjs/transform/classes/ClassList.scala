package com.github.opengrabeso.scalafromjs
package transform
package classes

import Classes._
import com.github.opengrabeso.scalafromjs.esprima._
import com.github.opengrabeso.esprima._
import SymbolTypes.SymbolMapId
import com.github.opengrabeso.scalafromjs.esprima.symbols.{Id, ScopeContext}

import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

object ClassList {

  implicit def classesFromClassList(cl: ClassList):  Map[ClassId, ClassDef] = cl.classes

  private [classes] def apply(n: Node.Node)(ctx: ScopeContext): ClassList = {
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

    n.walkWithScope(ctx) { (node, context) =>
      implicit val ctx = context
      node match {
        // new XXX()
        case `n` => // the scope itself should never be considered a symbol (can be Node.ClassDeclaration)
          false
        case Node.NewExpression(Node.Identifier(call), _) =>
          //println(s"Node.New ${call.name}")
          classNames += ClassId(call)
          false

        // any use of XXX.prototype probably marks a class
        case Node.Identifier(name) Dot "prototype" =>
          //println(s"Node.Identifier ${name.name}")
          classNames += ClassId(name)
          false
        case Node.ClassDeclaration(Defined(name), _, _) =>
          //println(s"Node.ClassDeclaration ${name.name}")
          classNames += ClassId(name)
          true

        /* the rule did more harm than good - functions are sometimes defined externally
      // use of this in a function most likely means the function is a constructor
      case (_: Node.This) Dot _ =>
        for {
          fun <- walker.stack.reverse.collectFirst { case c: Node.FunctionExpression => c }
          sym <- fun.name
          Some(sym: Node.SymbolDefun) <- sym.thedef.map(_.orig.headOption)
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
    }

    if (false) if (classNames.nonEmpty) {
      println(s"class names $classNames in $n")
    }


    def processPrototype(name: ClassId, prototypeDef: OObject, isStatic: Boolean = false) = {
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
            case kv: Node.MethodDefinition =>
              //println(s"Add ObjectKeyVal $key")
              if (isStatic) {
                kv.value match {
                  case v: Node.Expression =>
                    addMember(ClassVarMember(v, m))
                }
              } else {
                kv.value match {
                  case AnyFun(args, body) =>
                    //println(s"Add fun member ${kv.key}")
                    addMember(ClassFunMember(args, Block.statements(body), m))
                  case v: Node.Expression =>
                    //println(s"Add var member ${kv.key} ${nodeClassName(v)}")
                    addMember(ClassVarMember(v, m))
                }
              }

            case Node.Property(kind, _, _, value, method, shorthand) =>
              value match {
                case AnyFun(args, body) =>
                  val funMember = ClassFunMember(args, Block.statements(body), m)
                  kind match {
                    case "get" =>
                      addGetter(funMember)
                    case "set" =>
                      addSetter(funMember)
                    case _ =>
                      addMember(funMember)

                  }
                case value: Node.Expression =>
                  addMember(ClassVarMember(value, m))
                case _ =>
                  val member = unsupported(s"Unsupported property type ${nodeClassName(m)}", m, Some(m))
                  addMember(ClassVarMember(ScalaNode.StatementExpression(member), m))
              }

            case _ =>
              // prototype contains something other than a key: val pair - what to do with it?
              val member = unsupported(s"Unsupported property type ${nodeClassName(m)}", m, Some(m))
              addMember(ClassVarMember(ScalaNode.StatementExpression(member), m))
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
        case Node.ExpressionStatement(Node.CallExpression(
        Node.Identifier("Object") Dot "defineProperty",
        Seq( _: Node.ThisExpression, StringLiteral(prop), OObject(properties)))
        ) =>
          //println(s"Detect defineProperty ${sym.name}.${prop.value}")
          properties.foreach {
            case p: Node.Property => classes.defineSingleProperty(clsId, prop, p)
            case _ =>
          }
          false
        case _ =>
          true
      }
      bodyFiltered
    }

    n.walkWithScope(ctx) { (node, context) =>
      implicit val ctx = context
      node match {
        case `n` => // always enter into the scope we are processing
          false
        case defun@ClassDefineValue(Id(symDef), args, body, proto) =>
          for {
            clsId <- SymbolTypes.id(symDef) if classNames contains clsId
          } {
            val funScope = ScopeContext.getNodeId(defun)
            //println(s"Constructor ${symDef.name} returning value, funScope $funScope")

            ctx.withScope(defun.body) {
              // transform references to local variables to this.xxx references
              def transformReferences[T <: Node.Node](n: T): T = {
                n.transformBefore(ctx) { (node, descend, transformer) =>
                  implicit val ctx = transformer.context
                  node match {
                    case nameExpr@Node.Identifier(Id(name)) =>
                      assert(AnyFun.unapply(ctx.parents.last).isEmpty)
                      //println(s"Check reference $name, ${symbolDef.scope}")
                      if (funScope == name.sourcePos) {
                        //println(s"Transform reference $name")
                        Dot(Node.ThisExpression(), nameExpr).withTokens(nameExpr)
                      } else node
                    case p: Node.Property =>
                      // do not replace the variable in a key name
                      descend(p.value, transformer)
                      p
                    case AnyFun(_, b) =>
                      // do not replace the variable in a parameter list
                      ctx.withScope(b)(descend(b, transformer))
                      node
                    case _ =>
                      descend(node, transformer)
                      node
                  }
                }
              }

              def transformReferencesInBody(body: Seq[Node.StatementListItem]): Seq[Node.StatementListItem] = {
                body.map(transformReferences)
              }

              // scan for value and member definitions

              //val c = classes.defClass(clsId)
              object res {
                var clazz = new ClassDef
                val body = ArrayBuffer.empty[Node.StatementListItem]
              }

              body.foreach {
                case f@DefFun(Defined(Node.Identifier(sym)), fArgs, fBody, _) =>
                  ctx.withScope(f, fBody) {
                    val member = ClassFunMember(fArgs, transformReferencesInBody(fBody.body), f)
                    res.clazz = res.clazz.addMember(sym, member)
                  }
                case Node.VariableDeclaration(vars, kind) =>
                  //println("Vardef")
                  vars.foreach {
                    case v@Node.VariableDeclarator(Node.Identifier(vName), Defined(vValue)) =>
                      val member = ClassVarMember(vValue, v)
                      res.clazz = res.clazz.addValue(vName, member)
                    case vd@Node.VariableDeclarator(Node.Identifier(vName), _) =>
                      //println(s"value member $vName as undefined")
                      val member = ClassVarMember(ScalaNode.StatementExpression(Node.EmptyStatement()), vd)
                      res.clazz = res.clazz.addValue(vName, member)
                  }
                case s =>
                  //println(nodeClassName(s))
                  res.body.append(s)
              }


              proto.foreach {
                case ObjectKeyVal(name, value) =>
                  //println(s"$name, $value")
                  value match {
                    case Node.Identifier(`name`) => // same name, no need for any action
                    case Node.Identifier(other) =>
                      res.clazz = res.clazz.renameMember(other, name)
                    case AnyFun(fArgs, fBody) =>
                      ctx.withScope(value, fBody) {
                        val member = ClassFunMember(fArgs, transformReferencesInBody(Block.statements(fBody)), value)
                        res.clazz = res.clazz.addMember(name, member)
                      }
                    case _ =>
                    // TODO: we should include only the ones used by the return value - this may include some renaming
                    // TODO: the unused ones should be marked private
                  }
                case _ =>
              }


              // transform args to be in the class namespace (constructor symbol)
              val argsTransformed = args.map { arg =>
                val t = arg.cloneNode()
                t
              }
              val constructor = ClassFunMember(argsTransformed, res.body, node)
              classes += clsId -> res.clazz.addMember(inlineBodyName, constructor)
            }
          }
          true

        case ClassDefine(Id(symDef), args, body) =>
          for (clsId <- SymbolTypes.id(symDef) if classNames contains clsId) {
            // looks like a constructor
            //println("Constructor " + symDef.name)

            val bodyFiltered = processBody(clsId, body)

            val constructor = ClassFunMember(args, bodyFiltered, node)

            val c = classes.defClass(clsId)
            classes += clsId -> c.copy(members = c.members + ("constructor" -> constructor))
          }

          true
        case ClassMemberDef(name, funName, args, body) =>
          //println(s"Assign member $name.$funName")
          for (clazz <- createClassAsNeeded(name)) {
            val member = ClassFunMember(args, Block.statements(body), node)
            classes += name -> clazz.copy(members = clazz.members + (funName -> member))
          }
          true

        case DefineProperty(name, prop, Seq(property)) if classes.contains(name) =>
          //println(s"Detected DefineProperty $name.$prop  ${nodeClassName(property)}")
          classes.defineSingleProperty(name, prop, property)
          true

        case DefineProperties(name, properties) if classes.contains(name) =>
          //println(s"Detected DefineProperties $name")
          properties.foreach {
            case ObjectKeyVal(key, OObject(props)) =>
              //println(s"  property $key")
              props.foreach {
                case p: Node.Property =>
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
              val member = ClassVarMember(value, node)
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
        case DefineStaticMember(clsSym, member, value) if classNames contains clsSym =>
          // is it the same scope as the class definition? If not, do not consider it as a static init
          //println(s"Nesting ${scope.nesting} .. ${clsSym.scope.nesting}")
          if (ctx.scopeId == clsSym.sourcePos) {
            //println(s"Define static member $clsName.$member as ${nodeClassName(value)}")
            classes.defineStaticMember(clsSym, member, value)
          }
          true
        case Node.ClassDeclaration(Defined(clsSym), _, _) =>
          createClassAsNeeded(ClassId(clsSym))
          true
        case IsDeclScope() =>
          // do not enter any other scopes, they will be handled by recursion
          true

        case _ =>
          false
      }
    }

    //println(s"classNames $classNames")
    //println(s"classes ${classes.classes}")

    classes
  }


}

class ClassList {

  var classes = Map.empty[ClassId, ClassDef]

  def += (kv: (ClassId, ClassDef)): Unit = classes += kv

  //def isEmpty: Boolean = classes.isEmpty

  def defClass(name: ClassId): ClassDef = classes.getOrElse(name, new ClassDef)

  def defineSingleProperty(name: ClassId, key: String, p: Node.ObjectExpressionProperty) = {
    p match {
      case p: Node.Property =>
        (p.value, propertyKeyName(p.key)) match {
          case (fun: Node.FunctionExpression, "get") =>
            //println(s"Add getter ${pp.key}")
            // new lookup needed for classes(name), multiple changes can be chained
            val c = defClass(name)
            classes += name -> c.copy(getters = c.getters + (key -> ClassFunMember(fun.params, fun.body.body, p)))
          case (fun: Node.FunctionExpression, "set") =>
            //println(s"Add setter ${pp.key}")
            val c = defClass(name)
            classes += name -> c.copy(setters = c.setters + (key -> ClassFunMember(fun.params, fun.body.body, p)))
          //println(classes)
          case (value: Node.Expression, "value") =>
            //println(s"Add value $key")
            val c = defClass(name)
            classes += name -> c.copy(values = c.values + (key -> ClassVarMember(value, p)))

          case _ =>
          //println("Other property " + nodeClassName(p))

        }
      case _ =>

    }
  }

  def defineStaticMember(name: ClassId, key: String, value: Node.Node) = {
    //println(s"defineStaticMember $name.$key ${ScalaOut.outputNode(value)}")
    val c = defClass(name)
    if (!(c.membersStatic contains key)) {
      val member = value match {
        case AnyFun(args, body) =>
          //println(s"Define static fun $key")
          ClassFunMember(args, Block.statements(body), value)
        case v: Node.Expression =>
          //println(s"Define static var $key")
          ClassVarMember(v, value)
      }
      classes += name -> c.copy(membersStatic = c.membersStatic + (key -> member))
    }

  }

}
