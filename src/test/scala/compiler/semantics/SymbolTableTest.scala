package compiler.semantics

import compiler.parser.ast.expr.Expr.{Ident, StrLiter}
import compiler.parser.ast.func.Func.FuncSig
import compiler.parser.ast.func.{Func, Param}
import compiler.parser.ast.stats.Stat.{Block, VarInit}
import compiler.parser.ast.types.Type.BaseType.{IntType, StringType}
import compiler.semantics.SymbolTable.{FuncSymbol, VarSymbol}
import org.scalatest.FunSuite
import org.scalatest.Matchers.{be, convertToAnyShouldWrapper}

class SymbolTableTest extends FunSuite {
  val table: VarSymbolTable = VarSymbolTable(None)
  val funcTable: FuncSymbolTable = FuncSymbolTable()
  test("TableCanAddAndGetParamIdent") {
    table.addIdentifierSymbol(Param(IntType, Ident("param1")(-1,-1)))
    table.getIdentifierSymbol("param1") should be (Some(VarSymbol(IntType, isParam=true)))
  }

  test("TableCanAddAndGetFuncIdent") {
    funcTable.addIdentifierSymbol(Func(FuncSig(IntType, Ident("func1")(-1,-1), List(Param(IntType, Ident("funcParam1")(-1,-1)))), Block(List())))
    funcTable.getIdentifierSymbol("func1") should be (Some(FuncSymbol(IntType, List(IntType))))
  }

  test("TableCanAddAndGetVarInit") {
    table.addIdentifierSymbol(VarInit(StringType, Ident("testVar")(-1,-1), StrLiter("Hi")(-1,-1)))
    table.getIdentifierSymbol("testVar") should be (Some(VarSymbol(StringType)))
  }

  test ("TableChecksParentForSymbolIfItDoesNotExist") {
    table.addIdentifierSymbol(Param(IntType, Ident("param1")(-1,-1)))
    val newTable: VarSymbolTable = VarSymbolTable(Some(table))
    newTable.getIdentifierSymbol("param1") should be (Some(VarSymbol(IntType, isParam=true)))
  }

  test ("TableChecksParentForSymbolIfItDoesNotExistReturnNone") {
    val newEmptyTable: VarSymbolTable = VarSymbolTable(None)
    val newTable: VarSymbolTable = VarSymbolTable(Some(newEmptyTable))
    newTable.getIdentifierSymbol("param1") should be (None)
  }
}
