package compiler.semantics

import compiler.generator.utils.GeneratorUtils.{BOOL_BYTES, CHAR_BYTES, INT_BYTES, WORD_BYTES}
import compiler.parser.ast.types.Type
import compiler.parser.ast.types.Type.ArrayType
import compiler.parser.ast.types.Type.BaseType.{BoolType, CharType, IntType}
import compiler.semantics.SymbolTable.IdentSymbol

trait SymbolTable[InputType, Symbol <: IdentSymbol] {
  def getIdentifierSymbol(name: String): Option[Symbol]
  def addIdentifierSymbol(in: InputType): Boolean
}

object SymbolTable {
  sealed trait IdentSymbol
  case class FuncSymbol(returnType: Type, paramTypes: List[Type]) extends IdentSymbol
  case class VarSymbol(varType: Type, var offset: Int = 0, var isParam: Boolean = false) extends IdentSymbol {

    def setParamOffset(currentOffset: Int): Unit = {
      offset = currentOffset
    }

    def setOffset(currentOffset: Int): Int = {

      val value = varType match {
        case IntType => INT_BYTES
        case CharType => CHAR_BYTES
        case BoolType => BOOL_BYTES
        case _ => WORD_BYTES
      }
      offset = currentOffset + value
      value
    }

    def getDimension: Int = {
      varType match {
        case ArrayType(t, dim) => dim
        case _ => 0
      }
    }
  }

}
