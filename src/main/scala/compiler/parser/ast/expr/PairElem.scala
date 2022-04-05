package compiler.parser.ast.expr


import compiler.parser.ast.expr.Expr.AssignLHS
import compiler.parser.ast.stats.AssignRHS
import compiler.parser.utils.LexerUtils.implicits.tokenLift
import parsley.Parsley
import parsley.Parsley.{LazyParsley, attempt, pos}
import parsley.errors.combinator.ErrorMethods
import parsley.lift.lift1

sealed trait PairElem extends AssignLHS with AssignRHS {
  val expr: Expr
}

object PairElem {
  def apply(): Parsley[PairElem] = (attempt(Fst()) <|> Snd()).label("pair element")

  case class Fst(expr: Expr)(val pos: (Int, Int)) extends PairElem
  object Fst {
    def apply(): Parsley[Fst] = pos <**> lift1(Fst.apply, "fst" ~> Expr())
  }

  case class Snd(expr: Expr)(val pos: (Int, Int)) extends PairElem
  object Snd {
    def apply(): Parsley[Snd] = pos <**> lift1(Snd.apply, "snd" ~> Expr())
  }
}


