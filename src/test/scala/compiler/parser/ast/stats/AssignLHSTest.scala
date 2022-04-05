package compiler.parser.ast.stats

import compiler.parser.ast.expr.Expr._
import compiler.parser.ast.expr.PairElem.Snd
import org.scalatest.FunSuite
import org.scalatest.Matchers.{convertToAnyShouldWrapper, matchPattern}
import parsley.Success

class AssignLHSTest extends FunSuite {
  test("canAssignIdentOnLHS") {
    val result = Stat.assignLHSParser.parse("vex")
    result should matchPattern {case Success(Ident("vex"))=>}
  }

  test("canAssignArrayElemOnLHS") {
    val result = Stat.assignLHSParser.parse("ahyes[2]")
    result should matchPattern {case Success(ArrayElemExpr(Ident("ahyes"), List(IntLiter(2))))=>}
  }

  test("canAssignPairElemOnLHS") {
    val result = AssignRHS.parser.parse("snd vex")
    result should matchPattern {case Success(Snd(Ident("vex")))=>}
  }
}
