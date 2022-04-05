package compiler.parser.ast.types

import compiler.parser.utils.LexerUtils.implicits.tokenLift
import compiler.parser.utils.LexerUtils.token
import parsley.Parsley
import parsley.Parsley.LazyParsley
import parsley.combinator.attemptChoice
import parsley.errors.combinator.ErrorMethods
import parsley.expr.chain.postfix1
import parsley.lift.lift2
sealed trait Type


/*
 * <type> ::= <base-type> | <array-type> | <pair-type>
 *
 * <base-type> ::= "int" | "bool" | "char" | "string"
 *
 * <array-type> ::= <type> "[" "]"
 *
 * <pair-type> ::= "pair" "(" <pair-elem-type> "," <pair-elem-type> ")"
 * <pair-elem-type> ::= <base-type> | <array-type> | "pair"
 */

object Type {
  val parser: Parsley[Type] = {
    attemptChoice(
      ArrayType(),
      PairType(),
      BaseType()
    )
  }

  sealed trait BaseType extends Type with PairElemType

  object BaseType {
    def apply(): Parsley[BaseType] = {
      attemptChoice(
        IntType(),
        BoolType(),
        CharType(),
        StringType()
      ).label("base type")
    }

    // int
    case object IntType extends BaseType {
      def apply(): Parsley[BaseType] = "int" #> IntType
      override def toString: String = "Int"
    }

    // bool
    case object BoolType extends BaseType {
      def apply(): Parsley[BaseType] = "bool" #> BoolType
      override def toString: String = "Bool"
    }

    // char
    case object CharType extends BaseType {
      def apply(): Parsley[BaseType] = "char" #> CharType
      override def toString: String = "Char"
    }

    // string
    case object StringType extends BaseType {
      def apply(): Parsley[BaseType] = "string" #> StringType
      override def toString: String = "String"
    }
  }

  // <t> + []
  case class ArrayType(t: Type, dim: Int) extends Type with PairElemType {
    override def toString: String = "Array"
  }
  case object ArrayType {
    def apply(): Parsley[ArrayType] = parser
    private lazy val parser: Parsley[ArrayType] = {
      postfix1((PairType() <|> BaseType()).map(ArrayType(_, 0)),
        "[]" #> { a: ArrayType => ArrayType(a.t, a.dim + 1)})
    }
  }

  case class PairType(t1: PairElemType, t2: PairElemType) extends Type with PairElemType
  object PairType {
    def apply(): Parsley[PairType] =
      lift2(PairType.apply,
        "pair(" ~> token(PairElemType.apply()) <~ ",",
        token(PairElemType()) <~ ")")
  }

  sealed trait PairElemType extends Type {
    override def toString: String = "Pair"
  }
  object PairElemType {
    def apply(): Parsley[PairElemType] =
      attemptChoice(ArrayType(), BaseType(), "pair" #> InnerUntypedPair).label("type")
        .explain("A type inside a pair can either be an array, base type, or untyped pair (\'pair\')")
  }

  sealed trait UntypedPair extends PairElemType
  case object InnerUntypedPair extends UntypedPair
  case object NullPair extends UntypedPair
}
