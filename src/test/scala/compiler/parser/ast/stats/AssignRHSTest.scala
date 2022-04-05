package compiler.parser.ast.stats

import compiler.parser.ast.expr.Expr._
import compiler.parser.ast.expr.PairElem.Snd
import compiler.parser.ast.stats.AssignRHS.{ArrayLiter, Call, NewPair}
import org.scalatest.FunSuite
import org.scalatest.Matchers.{convertToAnyShouldWrapper, matchPattern}
import parsley.Success

class AssignRHSTest extends FunSuite {

  test("canAssignArrayLiterOnRHS") {
    val result = AssignRHS.parser.parse("[3, 'a', \"star\"]")
    result should matchPattern {case Success(ArrayLiter(List(IntLiter(3), ChrLiter('a'), StrLiter("star"))))=>}
  }

  test("canAssignCallOnRHS") {
    val result = AssignRHS.parser.parse("call myfunc(arg1, arg2, arg3)")
    result should matchPattern {case Success(Call(Ident("myfunc"),List(Ident("arg1"), Ident("arg2"), Ident("arg3"))))=>}
  }

  test("canAssignNewPairOnRHS") {
    val result = AssignRHS.parser.parse("newpair(vex, 3)")
    result should matchPattern {case Success(NewPair(Ident("vex"),IntLiter(3)))=>}
  }

  test("canAssignPairElemOnRHS") {
    val result = AssignRHS.parser.parse("snd vex")
    result should matchPattern {case Success(Snd(Ident("vex")))=>}
  }

  test("canAssignExprOnRHS") {
    val result = AssignRHS.parser.parse("vex")
    result should matchPattern {case Success(Ident("vex"))=>}
  }

}
