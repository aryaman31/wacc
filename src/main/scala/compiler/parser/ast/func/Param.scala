package compiler.parser.ast.func

import compiler.parser.ast.expr.Expr.Ident
import compiler.parser.ast.stats.Stat.Var
import compiler.parser.ast.types.Type
import compiler.parser.utils.LexerUtils.implicits.tokenLift
import parsley.Parsley
import parsley.Parsley.LazyParsley
import parsley.combinator.sepBy
import parsley.lift.lift2

// "<type> <Ident>" or "<type> <Ident> ,"
case class Param(paramType: Type, name: Ident) extends Var

object Param {
  type ParamsList = List[Param]

  object ParamsList {
    def apply(): Parsley[ParamsList] =
    "(" ~>
      sepBy(Param() , ","
      ) <~ ")"}

  private def apply(): Parsley[Param] =
    lift2(new Param(_,_),
      Type.parser,
      Ident()
    )}