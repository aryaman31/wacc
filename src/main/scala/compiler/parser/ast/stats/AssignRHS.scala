package compiler.parser.ast.stats

import compiler.parser.ast.expr.Expr.Ident
import compiler.parser.ast.expr.{Expr, PairElem}
import compiler.parser.utils.LexerUtils.implicits.tokenLift
import parsley.Parsley
import parsley.Parsley.{LazyParsley, pos}
import parsley.combinator.{attemptChoice, sepBy}
import parsley.errors.combinator.ErrorMethods
import parsley.lift.{lift1, lift2}

trait AssignRHS {
  val pos: (Int, Int)
}

object AssignRHS {
  lazy val parser: Parsley[AssignRHS] = attemptChoice(
    Call(),
    Expr(),
    NewPair(),
    PairElem(),
    ArrayLiter()
  )

  // '[' + (expr +  many(',' + expr))? + ']'
  case class ArrayLiter(exprList: List[Expr])(val pos: (Int, Int)) extends AssignRHS
  object ArrayLiter {
    def apply(): Parsley[AssignRHS] =
      pos <**> lift1(ArrayLiter.apply,
        "[" ~> sepBy(Expr(), ",") <~ "]")
        .label("array")
  }

  // 'newpair' + '(' + expr + ',' + expr + ')'
  case class NewPair(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends AssignRHS
  object NewPair {
    def apply(): Parsley[AssignRHS] =
      pos <**> lift2(NewPair.apply,
        "newpair(" ~> Expr(),
        "," ~> Expr() <~ ")"
      ).label("newpair")
  }

  // 'call' + ident + '(' + (expr + many(',' + expr))? + ')'
  case class Call(func: Ident, args: List[Expr])(val pos: (Int, Int)) extends AssignRHS
  object Call {
    def apply(): Parsley[AssignRHS] =
      pos <**> lift2(Call.apply,
        "call" ~> Ident(),
        "(" ~> sepBy(Expr(), ",") <~ ")"
      ).label("function call")
  }
}