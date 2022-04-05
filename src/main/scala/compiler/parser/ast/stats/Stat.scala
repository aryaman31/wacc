package compiler.parser.ast.stats

import compiler.parser.ast.expr.Expr.{ArrayElemExpr, AssignLHS, Ident}
import compiler.parser.ast.expr.{Expr, PairElem}
import compiler.parser.ast.types.Type
import compiler.parser.utils.LexerUtils.implicits.tokenLift
import compiler.semantics.VarSymbolTable
import parsley.Parsley
import parsley.Parsley.{LazyParsley, attempt}
import parsley.combinator.{attemptChoice, choice, many, sepBy1}
import parsley.errors.combinator.{ErrorMethods, fail}
import parsley.lift.{lift1, lift2, lift3}

sealed trait Stat

object Stat {
  case class Block(statements: List[Stat], var symbolTable: Option[VarSymbolTable] = None)

  def apply(isFunc: Boolean): Parsley[Block] = {
    (if (isFunc) lift2((a: List[Stat], b: Stat) => a :+ b,
      many(attempt(singleStatParser <~ ";")), returnStatParser)

    else sepBy1(singleStatParser, ";")).map(Block(_))
      .label("statement/s")
      .explain("Every scope must have at least one statement.")
  }

  lazy val singleStatParser: Parsley[Stat] =
    attemptChoice(
      Skip(),
      VarInit(),
      VarAssign(),
      Read(),
      Free(),
      Return(),
      Exit(),
      Println(),
      Print(),
      IfElse(isFunc = false),
      While(),
      BeginEnd(isFunc = false)
    )

  lazy val returnStatParser: Parsley[Stat] =
    choice(
      Return(),
      Exit(),
      IfElse(isFunc = true),
      BeginEnd(isFunc = true),
      fail("Function branch does not end with return statement")
    )

  val assignLHSParser: Parsley[AssignLHS] =
    attemptChoice(
      ArrayElemExpr(),
      Ident(),
      PairElem()
    )

  // "skip"
  case object Skip extends Stat {
    def apply(): Parsley[Stat] = "skip" #> Skip
  }

  trait Var
  // "<t> <name> = <rhs>" -> initialisation of variable
  case class VarInit(varType: Type, name: Ident, rhs: AssignRHS) extends Stat with Var
  case object VarInit {
    def apply(): Parsley[Stat] =
      lift3(VarInit.apply, Type.parser, Ident(), "=" ~> AssignRHS.parser)
  }

  // "<lhs> = <rhs>" -> variable assignment
  case class VarAssign(lhs: AssignLHS, rhs: AssignRHS) extends Stat
  case object VarAssign {
    def apply(): Parsley[Stat] =
      lift2(VarAssign.apply, assignLHSParser, "=" ~> AssignRHS.parser)
  }

  // "read <AssignLHS>"
  case class Read(readVal: AssignLHS) extends Stat
  case object Read {
    def apply(): Parsley[Stat] = lift1(Read.apply, "read" ~> assignLHSParser)
  }

  // "free" + <expr>
  case class Free(expr: Expr) extends Stat
  case object Free {
    def apply(): Parsley[Stat] = lift1(Free.apply, "free" ~> Expr())
  }

  // "return" + <expr>
  case class Return(expr: Expr) extends Stat
  case object Return {
    def apply(): Parsley[Stat] = lift1(Return.apply, "return" ~> Expr())
  }

  // "exit" + <expr>
  case class Exit(expr: Expr) extends Stat
  case object Exit {
    def apply(): Parsley[Stat] = lift1(Exit.apply, "exit" ~> Expr())
  }

  // "print" + <expr>
  case class Print(expr: Expr) extends Stat
  case object Print {
    def apply(): Parsley[Stat] = lift1(Print.apply, "print" ~> Expr())
  }


  // "println" + <expr>
  case class Println(expr: Expr) extends Stat
  case object Println {
    def apply(): Parsley[Stat] = lift1(Println.apply, "println" ~> Expr())
  }

  // "if <condition> then <ifStat> else <elseStat>"
  case class IfElse(condition: Expr, ifBlock: Block, elseBlock: Block) extends Stat
  case object IfElse {
    def apply(isFunc: Boolean): Parsley[Stat] =
      lift3(IfElse.apply,
        "if" ~> Expr(), "then" ~> Stat(isFunc),
        "else" ~> Stat(isFunc) <~ "fi")

  }

  // "while <condition> do <stat>>"
  case class While(condition: Expr, block: Block) extends Stat
  object While {
    def apply(): Parsley[Stat] =
      lift2(While.apply, "while" ~> Expr(),
        "do" ~> Stat(isFunc = false) <~ "done")
  }

  // "begin <Stat> end"
  case class BeginEnd(block: Block) extends Stat
  object BeginEnd {
    def apply(isFunc: Boolean): Parsley[BeginEnd] =
      lift1(BeginEnd.apply, "begin" ~> Stat(isFunc) <~ "end")

  }
}

