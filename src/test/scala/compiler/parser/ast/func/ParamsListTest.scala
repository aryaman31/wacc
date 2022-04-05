package compiler.parser.ast.func

import compiler.parser.ast.expr.Expr.Ident
import compiler.parser.ast.func.Param.ParamsList
import compiler.parser.ast.types.Type.BaseType.{BoolType, IntType}
import org.scalatest.FunSuite
import org.scalatest.Matchers.{convertToAnyShouldWrapper, matchPattern}
import parsley.Success

class ParamsListTest extends FunSuite {

  test("paramsListParserParsesCorrectly") {
    val result = ParamsList().parse("(int var1, bool   var2)")
    result should matchPattern {case Success(List(Param(IntType, Ident("var1")), Param(BoolType, Ident("var2"))))=>}
  }

}
