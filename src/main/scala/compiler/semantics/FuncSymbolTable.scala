package compiler.semantics

import compiler.parser.ast.func.Func
import compiler.semantics.SymbolTable.FuncSymbol

case class FuncSymbolTable(var table: Map[String, FuncSymbol] = Map()) extends SymbolTable[Func, FuncSymbol] {

  override def getIdentifierSymbol(name: String): Option[FuncSymbol] = {
    table.get(name) match {
      case Some(t) => Some(t)
      case None => None
    }
  }

  override def addIdentifierSymbol(func: Func): Boolean = {
    val name = func.funcSig.name.ident
    if (table.contains(name)) {
      return false
    }
    table += (name -> FuncSymbol(func.funcSig.returnType, func.funcSig.params.map(_.paramType)))
    true
  }
}
