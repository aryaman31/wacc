package compiler.parser.ast.expr

import compiler.parser.ast.expr.Expr._
import org.scalatest.FunSuite
import org.scalatest.Matchers.{convertToAnyShouldWrapper, matchPattern}
import parsley.Success

class ExprTest extends FunSuite {

  test("IntLiterCanParseIntWithoutSign") {
    val result = Expr().parse("123")
    result should matchPattern {case Success(IntLiter(123))=>}
  }

  test("IntLiterCanParseIntWithPosSign") {
    val result = Expr().parse("+123")
    result should matchPattern {case Success(IntLiter(123))=>}
  }

  test("IntLiterCanParseIntWithNegSign") {
    val result = Expr().parse("-123")
    result should matchPattern {case Success(IntLiter(-123))=>}
  }

  test("CanParseTrueLiter") {
    val result = Expr().parse("true")
    result should matchPattern {case Success(BoolLiter(true))=>}
  }

  test("CanParseFalseLiter") {
    val result = Expr().parse("false")
    result should matchPattern {case Success(BoolLiter(false))=>}
  }

  test("CanParseCharLiter") {
    val result = Expr().parse("\'c\'")
    result should matchPattern {case Success(ChrLiter('c'))=>}
  }

  test("CanParseStringLiter") {
    val result = Expr().parse("\"hello\"")
    print(result)
    result should matchPattern {case Success(StrLiter("hello"))=>}
  }

  test("CanParseEscapeChars") {
    val result = Expr().parse("\"hell\no\"")
    print(result)
    result should matchPattern {case Success(StrLiter("hell\no"))=>}
  }

  test("CanParseArrayElemExpr") {
    val result = Expr().parse("xyz[5]")
    result should matchPattern {case Success(ArrayElemExpr(Ident("xyz"), List(IntLiter(5))))=>}
  }

  test("CanParseNestedArrayElem") {
    val result = Expr().parse("a[b[5]]")
    result should matchPattern {case Success(ArrayElemExpr(Ident("a"), List(ArrayElemExpr(Ident("b"), List(IntLiter(5))))))=>}
  }

  test("CanParseUnaryOperators") {
    val result = Expr().parse("! ord x")
    result should matchPattern {case Success(Bang(Ord(Ident("x"))))=>}
  }

  test("IdentParserParsesIdentifier") {
    val result = Expr().parse("xyz")
    result should matchPattern {case Success(Ident("xyz"))=>}
  }
  // Failure will happen after parsing ident
  test("IdentParserDoesn'tParseInvalidChar") {
    val result = Expr().parse("xy@z")
    result should matchPattern {case Success(Ident("xy"))=>}
  }

  test("IdentParserWorksWithUnderscores") {
    val result = Expr().parse("xy_z")
    result should matchPattern {case Success(Ident("xy_z"))=>}
  }  

//   test("IdentParserDoesNotStartWithNumbers") {
//     val result = Expr.parser.parse("1abcd")
//     result shouldBe a [Failure[_]]
//   }

  test("UnaryOperatorBangTest") {
    val result = Expr().parse("! xyz")
    result should matchPattern {case Success(Bang(Ident("xyz")))=>}
  }

  test("BinaryOperatorsPrecedenceIsCorrect") {
    Expr().parse("1*2+3") should matchPattern {case Success(
      Add(Multiply(IntLiter(1), IntLiter(2)), IntLiter(3)))=>}
    Expr().parse("1+2*3") should matchPattern {case Success(
      Add(IntLiter(1), Multiply(IntLiter(2), IntLiter(3))))=>}
    Expr().parse("p == (q || r)") should matchPattern {case Success(
      Equals(Ident("p"),Or(Ident("q"),Ident("r"))))=>}
    Expr().parse("p == q || r") should matchPattern {case Success(
      Or(Equals(Ident("p"),Ident("q")),Ident("r")))=>}
  }
}
