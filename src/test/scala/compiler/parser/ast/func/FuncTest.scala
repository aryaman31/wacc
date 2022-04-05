package compiler.parser.ast.func

import compiler.parser.ast.expr.Expr.{Ident, IntLiter}
import compiler.parser.ast.func.Func.FuncSig
import compiler.parser.ast.stats.Stat.{Block, Println, Return}
import compiler.parser.ast.types.Type.BaseType.IntType
import org.scalatest.FunSuite
import org.scalatest.Matchers.{convertToAnyShouldWrapper, matchPattern}
import parsley.Success

class FuncTest extends FunSuite {

  test("CanParseFunctionDefinition") {
    val result = Func().parse("int foo(int x) is println x ; return 23 end")
    result should matchPattern { case Success(Func(FuncSig(IntType,
    Ident("foo"), List(Param(IntType, Ident("x")))),
    Block(List(Println(Ident("x")), Return(IntLiter(23))), None))) =>
    }
  }

}
