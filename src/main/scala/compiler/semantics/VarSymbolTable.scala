package compiler.semantics


import compiler.generator.utils.GeneratorUtils.{BOOL_BYTES, CHAR_BYTES, INT_BYTES, WORD_BYTES}
import compiler.parser.ast.func.Param
import compiler.parser.ast.stats.Stat.{Var, VarInit}
import compiler.parser.ast.types.Type
import compiler.parser.ast.types.Type.BaseType.{BoolType, CharType, IntType}
import compiler.semantics.SymbolTable.VarSymbol


case class VarSymbolTable(parent: Option[VarSymbolTable],
                          var table: Map[String, VarSymbol] = Map[String, VarSymbol](),
                          retType: Option[Type] = None,
                          var totalNumBytes: Int = 0,
                          var currentOffset: Int = 0,
                          var paramNumBytes: Int = 0,
                          var funcOffset: Int = 0) extends SymbolTable[Var, VarSymbol] {

  private var isNumBytesCalculated = false

  override def getIdentifierSymbol(name: String): Option[VarSymbol] = {
    table.get(name) match {
      case Some(t) => Some(t)
      case None => parent match {
        case Some(node) => node.getIdentifierSymbol(name)
        case None => None
      }
    }
  }

  def setOffset(name: String): Unit = {
    table.get(name) match {
      case Some(t) => currentOffset += t.setOffset(currentOffset)
      case None =>
    }
  }

  def getOffset(name: String): Int = {
    table.get(name) match {
      case Some(v: VarSymbol) =>
        if (v.isParam) totalNumBytes + WORD_BYTES + v.offset + funcOffset
        else totalNumBytes - v.offset + funcOffset
      case None => parent match {
        case Some(node) => totalNumBytes + node.getOffset(name) + funcOffset
        case None => 0
      }
    }
  }


  def getNumBytesUsed: Int = {
    if (isNumBytesCalculated) {
      return totalNumBytes
    }
    isNumBytesCalculated = true

    var numBytes = 0
    for ((_, VarSymbol(v, _, param)) <- table) {
      val bytes = v match {
        case IntType => INT_BYTES
        case BoolType => BOOL_BYTES
        case CharType => CHAR_BYTES
        case _ => WORD_BYTES
      }
      if (param) {
        paramNumBytes += bytes
      }
      else {
        numBytes += bytes
      }
    }
    totalNumBytes = numBytes
    totalNumBytes
  }

  override def addIdentifierSymbol(init: Var): Boolean = {
    init match {
      case param: Param =>
        val name = param.name.ident
        if (table.contains(name)) {
          return false
        }
        table += (name -> VarSymbol(param.paramType, isParam = true))
        true
      case varInit: VarInit =>
        val name = varInit.name.ident
        if (table.contains(name)) {
          return false
        }
        table += (name -> VarSymbol(varInit.varType))
        true
    }
  }
}
