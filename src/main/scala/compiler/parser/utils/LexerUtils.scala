
package compiler.parser.utils

import parsley.Parsley
import parsley.Parsley.{LazyParsley, attempt, void}
import parsley.character._
import parsley.combinator.{decide, eof, optional}
import parsley.errors.combinator.fail
import parsley.token.{LanguageDef, Lexer, Predicate}

import scala.language.implicitConversions

object LexerUtils {
  val identCharSet: Set[Char] = ('_' +: (('a' to 'z') ++ ('A' to 'Z'))).toSet
  val identLetterSet: Set[Char] = ('_' +: (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9'))).toSet
  val lang: LanguageDef = LanguageDef.plain.copy(
    commentLine = "#",
    commentStart = "",
    commentEnd = "",
    identStart = Predicate(identCharSet),
    identLetter = Predicate(identLetterSet),
    space = Predicate(isWhitespace),
    keywords =
      Set("begin", "end", "skip", "read", "free", "return",
        "exit", "print", "println", "if", "then", "else", "fi",
        "while", "do", "done", "newpair", "call", "fst", "snd",
        "int", "bool", "char", "string", "pair", "len", "ord",
        "chr", "true", "false", "null")
  )

  val lexer = new Lexer(lang)

  def checkIntOverflow(bigInt: BigInt): Option[Int] = {
    if (bigInt.compareTo(BigInt(Int.MaxValue)) <= 0
      && bigInt.compareTo(BigInt(Int.MinValue)) >= 0)
      Some(bigInt.toInt)
    else None
  }

  lazy val signedInt: Parsley[Int] =
    token(decide((char('-') ~> num.map(-BigInt(_)) <|> (optional(char('+'))) ~> num.map(BigInt(_)))
      .map(checkIntOverflow), fail("Error: Integer overflow")))

  lazy val num: Parsley[String] = digit.foldLeft1("")((x, d) => x + d)

  val escapedCharSet = Set('0', 'b', 't', 'n', 'f', 'r', '\"', '\'', '\\')
  val notCharsSet = Set('\\', '\'', '\"')

  def escapeChar(c: Char): Char = {
    c match {
      case '0' => '\u0000'
      case 'b' => '\b'
      case 't' => '\t'
      case 'n' => '\n'
      case 'f' => '\f'
      case 'r' => '\r'
      case '\"' => '\"'
      case '\'' => '\''
      case '\\' => '\\'
    }
  }

  lazy val charParser: Parsley[Char] =
    (char('\\') ~> oneOf(escapedCharSet)).map(escapeChar) <|>
      noneOf(notCharsSet)

  val charLiteral: Parsley[Char] = char('\'') ~> charParser <~ implicits.tokenLift('\'')
  val stringLiteral: Parsley[String] = char('\"') ~> charParser.foldLeft("")(_ :+ _) <~ implicits.tokenLift('\"')

  def fully[A](p: Parsley[A]): Parsley[A] = lexer.whiteSpace ~> p <~ eof

  def token[A](p: Parsley[A]): Parsley[A] = lexer.lexeme(attempt(p))

  object implicits {
    implicit def tokenLift(c: Char): Parsley[Unit] = void(lexer.symbol(c))

    implicit def tokenLift(s: String): Parsley[Unit] = {
      if (lang.keywords(s)) lexer.keyword(s)
      else void(lexer.symbol_(s))
    }
  }
}
