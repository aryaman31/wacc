package compiler.parser.ast.func

import compiler.parser.ast.expr.Expr.Ident
import compiler.parser.ast.func.Func.FuncSig
import compiler.parser.ast.func.Param.ParamsList
import compiler.parser.ast.stats.Stat
import compiler.parser.ast.stats.Stat.Block
import compiler.parser.ast.types.Type
import compiler.parser.utils.LexerUtils.implicits.tokenLift
import parsley.Parsley
import parsley.Parsley.{LazyParsley, attempt}
import parsley.errors.combinator.ErrorMethods
import parsley.lift.{lift2, lift3}

// def + <Type> + <Ident> + <ParamsList(with '()')> + "is" + <Stat> + "end"
case class Func(funcSig: FuncSig, block: Block)

object Func {
  def apply(): Parsley[Func] =
    lift2(Func.apply, attempt(FuncSig()),
      "is" ~> Stat(isFunc = true) <~ "end")

  case class FuncSig(returnType: Type, name: Ident, params: ParamsList)
  object FuncSig {
    def apply(): Parsley[FuncSig] =
      lift3(FuncSig.apply, Type.parser, Ident(), ParamsList()).label("function definition")
  }
}


