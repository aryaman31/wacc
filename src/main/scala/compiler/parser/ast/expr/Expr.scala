package compiler.parser.ast.expr

import compiler.parser.ast.stats.AssignRHS
import compiler.parser.utils.LexerUtils._
import compiler.parser.utils.LexerUtils.implicits.tokenLift
import compiler.parser.utils.ParserBuilder.{ParserBuilderPos1, ParserBuilderPos2}
import parsley.Parsley
import parsley.Parsley.{LazyParsley, attempt, notFollowedBy, pos}
import parsley.character.{digit, whitespace}
import parsley.combinator.{attemptChoice, many, some}
import parsley.errors.combinator.ErrorMethods
import parsley.expr.{InfixL, Ops, Prefix, precedence}
import parsley.lift.{lift1, lift2}

import scala.language.{implicitConversions, postfixOps}

sealed trait Expr extends AssignRHS {
  val pos: (Int, Int)
}

object Expr {
  def apply(): Parsley[Expr] = parser

  def binOp(p: Parsley[Any]): Parsley[Any] = p.label("binary operator")
  def unOp(p: Parsley[Any]): Parsley[Any] = p.label("unary operator")

  private val negateToken: Parsley[Unit] = lexer.lexeme("-" <~ notFollowedBy(digit))


  // precedence table for all unary and binary operations, attempts to parse operator expressions first, then brackets
  // and literals
  private lazy val parser: Parsley[Expr] =
    (precedence[Expr](('(' ~> Expr.parser <~ ')'.label("\")\"")).label("expression"), literals)(
    Ops(Prefix)(Bang <# unOp("!"),
                Negate <# attempt(unOp(negateToken)),
                Len <# unOp("len"),
                Ord <# unOp("ord"),
                Chr <# unOp("chr")),
    Ops(InfixL)(Multiply <# binOp("*"),
                Divide <# binOp("/"),
                Mod <# binOp("%")),
    Ops(InfixL)(Add <# binOp("+"),
                Subtract <# binOp("-")),
    Ops(InfixL)(attempt(GTE <# binOp(">=")),
                GT <# binOp(">"),
                attempt(LTE <# binOp("<=")),
                LT <# binOp("<")),
    Ops(InfixL)(attempt(Equals <# binOp("==")),
                NotEquals <# binOp("!=")),
    Ops(InfixL)(And <# binOp("&&")),
    Ops(InfixL)(Or <# binOp("||"))
  ) <~ many(whitespace).hide).label("expression")

  // !, -, len, ord, chr + <expr>
  sealed trait UnaryOp extends Expr {val expr: Expr}
  case class Bang(override val expr: Expr)(val pos: (Int, Int)) extends UnaryOp
  case class Negate(override val expr: Expr)(val pos: (Int, Int)) extends UnaryOp
  case class Len(override val expr: Expr)(val pos: (Int, Int)) extends UnaryOp
  case class Ord(override val expr: Expr)(val pos: (Int, Int)) extends UnaryOp
  case class Chr(override val expr: Expr)(val pos: (Int, Int)) extends UnaryOp

  case object Bang extends ParserBuilderPos1[Expr, UnaryOp]
  case object Negate extends ParserBuilderPos1[Expr, UnaryOp]
  case object Len extends ParserBuilderPos1[Expr, UnaryOp]
  case object Ord extends ParserBuilderPos1[Expr, UnaryOp]
  case object Chr extends ParserBuilderPos1[Expr, UnaryOp]

  // lots <expr> + binop + <expr>
  sealed trait BinOp extends Expr {val expr1: Expr; val expr2: Expr}
  case class Multiply(override val expr1: Expr, override val expr2: Expr)(val pos: (Int, Int))  extends BinOp
  case class Divide(override val expr1: Expr, override val expr2: Expr)(val pos: (Int, Int))    extends BinOp
  case class Mod(override val expr1: Expr, override val expr2: Expr)(val pos: (Int, Int))       extends BinOp
  case class Add(override val expr1: Expr, override val expr2: Expr)(val pos: (Int, Int))       extends BinOp
  case class Subtract(override val expr1: Expr, override val expr2: Expr)(val pos: (Int, Int))  extends BinOp
  case class GT(override val expr1: Expr, override val expr2: Expr)(val pos: (Int, Int))        extends BinOp
  case class GTE(override val expr1: Expr, override val expr2: Expr)(val pos: (Int, Int))       extends BinOp
  case class LT(override val expr1: Expr, override val expr2: Expr)(val pos: (Int, Int))        extends BinOp
  case class LTE(override val expr1: Expr, override val expr2: Expr)(val pos: (Int, Int))       extends BinOp
  case class Equals(override val expr1: Expr, override val expr2: Expr)(val pos: (Int, Int))    extends BinOp
  case class NotEquals(override val expr1: Expr, override val expr2: Expr)(val pos: (Int, Int)) extends BinOp
  case class And(override val expr1: Expr, override val expr2: Expr)(val pos: (Int, Int))       extends BinOp
  case class Or(override val expr1: Expr, override val expr2: Expr)(val pos: (Int, Int))        extends BinOp

  case object Multiply  extends ParserBuilderPos2[Expr, Expr, BinOp]
  case object Divide    extends ParserBuilderPos2[Expr, Expr, BinOp]
  case object Mod       extends ParserBuilderPos2[Expr, Expr, BinOp]
  case object Add       extends ParserBuilderPos2[Expr, Expr, BinOp]
  case object Subtract  extends ParserBuilderPos2[Expr, Expr, BinOp]
  case object GT        extends ParserBuilderPos2[Expr, Expr, BinOp]
  case object GTE       extends ParserBuilderPos2[Expr, Expr, BinOp]
  case object LT        extends ParserBuilderPos2[Expr, Expr, BinOp]
  case object LTE       extends ParserBuilderPos2[Expr, Expr, BinOp]
  case object Equals    extends ParserBuilderPos2[Expr, Expr, BinOp]
  case object NotEquals extends ParserBuilderPos2[Expr, Expr, BinOp]
  case object And       extends ParserBuilderPos2[Expr, Expr, BinOp]
  case object Or        extends ParserBuilderPos2[Expr, Expr, BinOp]

  trait AssignLHS extends Expr {
    val pos: (Int, Int)
  }

  // some(IdentChar)
  case class Ident(ident: String)(val pos: (Int, Int)) extends Term with AssignLHS
  object Ident {
    def apply(): Parsley[Ident] = pos <**> lift1(Ident.apply, lexer.identifier)
  }

  sealed trait Term extends Expr

  lazy val literals: Parsley[Term] = attemptChoice(
    IntLiter(),
    BoolLiter(),
    ChrLiter(),
    StrLiter(),
    PairLiter(),
    ArrayElemExpr(),
    Ident()).label("expression")


  // IntSign + some(Digit)
  case class IntLiter(value: Int)(val pos: (Int, Int)) extends Term
  object IntLiter {
    def apply(): Parsley[IntLiter] =
      pos <**> lift1(IntLiter.apply,
        signedInt).label("Int")
  }

  // "true" or "false"
  case class BoolLiter(value: Boolean)(val pos: (Int, Int)) extends Term
  object BoolLiter {
    def apply(): Parsley[BoolLiter] =
      pos <**> lift1(BoolLiter.apply,
        "true" #> true <|> "false" #> false).label("Bool")
  }

  // ' + character + '
  case class ChrLiter(value: Char)(val pos: (Int, Int)) extends Term
  object ChrLiter {
    def apply(): Parsley[ChrLiter] =
      pos <**> lift1(ChrLiter.apply,
        charLiteral).label("Char")
  }

  // " + many(character) + "
  case class StrLiter(value: String)(val pos: (Int, Int)) extends Term
  object StrLiter {
    def apply(): Parsley[StrLiter] = pos <**> lift1(StrLiter.apply,
      stringLiteral).label("String")
  }

  case class PairLiter(pos: (Int, Int)) extends Term
  object PairLiter {
    def apply(): Parsley[Term] = (pos <~ "null").map(PairLiter.apply)
  }

  // <name> + '[' + <expr> + ']'
  case class ArrayElemExpr(arrName: Ident, exprList: List[Expr])(val pos: (Int, Int)) extends Term with AssignLHS
  object ArrayElemExpr {
    def apply(): Parsley[ArrayElemExpr] =
      pos <**> lift2(ArrayElemExpr.apply,
        Ident(), some("[" ~> Expr.parser <~ "]")).label("Array element")
  }
}