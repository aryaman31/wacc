package compiler.parser.ast

import compiler.parser.ast.func.Func
import compiler.parser.ast.stats.Stat
import compiler.parser.ast.stats.Stat.Block
import compiler.parser.utils.LexerUtils.fully
import compiler.parser.utils.LexerUtils.implicits.tokenLift
import compiler.semantics.FuncSymbolTable
import parsley.Parsley
import parsley.Parsley.LazyParsley
import parsley.combinator.many
import parsley.lift.lift2

// begin + [func] + stat + end
case class Program(funcDefs: List[Func], block: Block, var funcSymbolTable: Option[FuncSymbolTable] = None)

object Program {
  def apply(): Parsley[Program] =
    fully(
      lift2(new Program(_: List[Func], _: Block),
        "begin" ~> many(Func()),
        Stat(isFunc = false) <~ "end"
      )
    )
}