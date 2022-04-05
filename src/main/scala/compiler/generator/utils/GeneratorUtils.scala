package compiler.generator.utils

import compiler.generator.utils.InstrUtils._
import compiler.generator.utils.OperandUtils.Register.sp
import compiler.generator.utils.OperandUtils._
import compiler.parser.ast.expr.Expr
import compiler.parser.ast.expr.Expr._
import compiler.parser.ast.expr.PairElem.{Fst, Snd}
import compiler.parser.ast.types.Type
import compiler.parser.ast.types.Type.BaseType.{BoolType, CharType, IntType}
import compiler.parser.ast.types.Type.{ArrayType, InnerUntypedPair, PairType}
import compiler.semantics.ExprTypesUtil.ExprTypeInference
import compiler.semantics.VarSymbolTable

import scala.collection.mutable
import scala.math.abs

object GeneratorUtils {

  val INDENT = "    "
  val NEWLINE = "\n"

  val INT_BYTES: Int = 4
  val CHAR_BYTES: Int = 1
  val BOOL_BYTES: Int = 1
  val WORD_BYTES: Int = 4
  val BYTE_SIZE: Int = 1

  val INT_NUM_BITS: Int = 32

  val TRUE: Int = 1
  val FALSE: Int = 0

  val MAX_STACK_CHANGE = 1024

  // Get the type of an expression (either directly or by accessing symbol table)
  def getExprType(expr: Expr, symbolTable: VarSymbolTable): Type = {
    expr match {
      case Ident(name) =>
        // Lookup type from symbol table
        symbolTable.getIdentifierSymbol(name).get.varType
      case ArrayElemExpr(Ident(name), exprList) =>
        symbolTable.getIdentifierSymbol(name).get.varType match {
          // Either decrements the dimension or if new dimension
          // becomes 0 then we return the base type of array
          case ArrayType(t, dim) =>
            dim - exprList.length match {
              case 0 => t
              case newDim => ArrayType(t, newDim)
            }
        }

      // Get the types of pair elem
      case Fst(expr) => getExprType(expr, symbolTable) match {
        case PairType(t1, _) => t1
      }
      case Snd(expr) => getExprType(expr, symbolTable) match {
        case PairType(_, t2) => t2
      }
      case _: PairLiter =>
        // Null so we don't know the types inside the pair
        PairType(InnerUntypedPair, InnerUntypedPair)

      // Otherwise we can just use the function from front-end
      case _ => expr.outputType.get
    }
  }

  // Get the number of bytes required to store a given type on the stack
  def getBytes(typ: Type): Int = {
    typ match {
      case CharType => CHAR_BYTES
      case BoolType => BOOL_BYTES
      case IntType => INT_BYTES
      case _ => WORD_BYTES
    }
  }

  // Get the number of bytes required to store an expr on the stack - using the type
  def getNumBytes(expr: Expr, symbolTable: VarSymbolTable): Int = {
   val exprType: Type = getExprType(expr, symbolTable)
    getBytes(exprType)
  }

  // Increment/ decrement the stack pointer by a certain amount
  // (as we can only increment decrement the sp by a maximum of 1024 in one instruction)
  // Negative value will decrement
  def moveStackPointer(totalBytesUsed:Int): List[Instr] = {
    // Check if we need ADD or SUB
    val neg = totalBytesUsed < 0
    var bytesLeft = abs(totalBytesUsed)
    val addInstructions: mutable.ListBuffer[Instr] = mutable.ListBuffer()
    while (bytesLeft > MAX_STACK_CHANGE) {
      bytesLeft = bytesLeft - MAX_STACK_CHANGE

      addInstructions.addOne(
        if (neg) SUB(sp, sp, ImmediateAddress(MAX_STACK_CHANGE))
        else ADD(sp, sp, ImmediateAddress(MAX_STACK_CHANGE))
      )
    }
    addInstructions.addOne(
      if (neg) SUB(sp, sp, ImmediateAddress(bytesLeft))
      else ADD(sp, sp, ImmediateAddress(bytesLeft))
    )
    addInstructions.toList
  }

}
