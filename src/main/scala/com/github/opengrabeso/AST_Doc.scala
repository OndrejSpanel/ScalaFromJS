package com.github.opengrabeso

object AST_Doc {
  object AST_Node {
    object AST_Statement {
      object AST_Debugger
      object AST_Directive
      object AST_SimpleStatement
      object AST_Block {
        object AST_BlockStatement
        object AST_Scope {
          object AST_Toplevel
          object AST_Lambda {
            object AST_Accessor
            object AST_Function
            object AST_Defun
          }
        }
        object AST_Switch
        object AST_SwitchBranch {
          object AST_Default
          object AST_Case
        }
        object AST_Try
        object AST_Catch
        object AST_Finally
      }
      object AST_EmptyStatement
      object AST_StatementWithBody {
        object AST_LabeledStatement
        object AST_IterationStatement {
          object AST_DWLoop {
            object AST_Do
            object AST_While
          }
          object AST_For
          object AST_ForIn
        }
        object AST_With
        object AST_If
      }
      object AST_Jump {
        object AST_Exit {
          object AST_Return
          object AST_Throw
        }
        object AST_LoopControl {
          object AST_Break
          object AST_Continue
        }
      }
      object AST_Definitions {
        object AST_Var
        object AST_Const
      }
    }
    object AST_VarDef
    object AST_Call {
      object AST_New
    }
    object AST_Seq
    object AST_PropAccess {
      object AST_Dot
      object AST_Sub
    }
    object AST_Unary {
      object AST_UnaryPrefix
      object AST_UnaryPostfix
    }
    object AST_Binary {
      object AST_Assign
    }
    object AST_Conditional
    object AST_Array
    object AST_Object
    object AST_ObjectProperty {
      object AST_ObjectKeyVal
      object AST_ObjectSetter
      object AST_ObjectGetter
    }
    object AST_Symbol {
      object AST_SymbolAccessor
      object AST_SymbolDeclaration {
        object AST_SymbolVar {
          object AST_SymbolFunarg
        }
        object AST_SymbolConst
        object AST_SymbolDefun
        object AST_SymbolLambda
        object AST_SymbolCatch
      }
      object AST_Label
      object AST_SymbolRef
      object AST_LabelRef
      object AST_This
    }
    object AST_Constant {
      object AST_String
      object AST_Number
      object AST_RegExp
      object AST_Atom {
        object AST_Null
        object AST_NaN
        object AST_Undefined
        object AST_Hole
        object AST_Infinity
        object AST_Boolean {
          object AST_False
          object AST_True
        }
      }
    }
  }
}
