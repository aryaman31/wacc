package compiler.semantics

import compiler.parser.ast.expr.Expr
import compiler.parser.ast.expr.Expr._
import compiler.parser.ast.types.Type
import compiler.parser.ast.types.Type.BaseType.{BoolType, CharType, IntType, StringType}
import compiler.parser.ast.types.Type.NullPair


object ExprTypesUtil {
  implicit class ExprTypeInference(expr: Expr) {
    def inputType: Option[Type] = expr match {
      case _: Bang => Some(BoolType)
      case _: Negate => Some(IntType)
      case _: Len => None // Can't get here
      case _: Chr => Some(IntType)
      case _: Ord => Some(CharType)

      case _: Multiply => Some(IntType)
      case _: Divide => Some(IntType)
      case _: Mod => Some(IntType)
      case _: Add => Some(IntType)
      case _: Subtract => Some(IntType)
      case _: GT | _: GTE | _: LT | _: LTE => Some(IntType)
      case _: Equals | _: NotEquals | _: And | _: Or => Some(BoolType)
    }

    def outputType: Option[Type] = expr match {
      case _: Bang => Some(BoolType)
      case _: Negate => Some(IntType)
      case _: Len => Some(IntType)
      case _: Chr => Some(CharType)
      case _: Ord => Some(IntType)

      case _: Multiply => Some(IntType)
      case _: Divide => Some(IntType)
      case _: Mod => Some(IntType)
      case _: Add => Some(IntType)
      case _: Subtract => Some(IntType)
      case _: GT | _: GTE | _: LT | _: LTE |
           _: Equals | _: NotEquals | _: And | _: Or => Some(BoolType)

      case _: IntLiter => Some(IntType)
      case _: BoolLiter => Some(BoolType)
      case _: ChrLiter => Some(CharType)
      case _: StrLiter => Some(StringType)
      case _: PairLiter => Some(NullPair)
    }
  }
}
