package compiler

import compiler.parser.ast.types.Type

object ErrorUtils {

  import parsley.errors.ErrorBuilder

  def getErrorString(lineNo: Int, line: SemanticErrorLine): String = {
    SemanticError(lineNo, line).toString
  }

  case class SemanticError(lineNo: Int, line: SemanticErrorLine) {
    override def toString: String = "\nSemantic Error at line " + lineNo + ": " +
      line.toString + "\n"
  }

  sealed trait SemanticErrorLine

  case class TypeErrorLine(expected: Type, actual: Type, line: String, errType: Int) extends SemanticErrorLine {
    override def toString: String = {
      var ex = "\nExpected: "
      var ac = "\nActual: "
      val sb = new StringBuilder()
      errType match {
        case 1 => sb.addAll("Incompatible types at \n")
        case 2 => sb.addAll("Invalid assignment due to incompatible types at \n")
          ex = "\nLHS: "
          ac = "\nRHS: "
        case 3 => sb.addAll("Return type does not match expected function return type at \n")
        case 4 => sb.addAll("Exit code must be of type Int at \n")
        case 5 => sb.addAll("Condition must be of type Bool at \n")
        case 6 => sb.addAll("Array index must be of type Int at \n")
        case 7 => sb.addAll("Unary operator called on unexpected type at \n")
        case 8 => sb.addAll("Equality check called on expressions of different types at \n")
          ex = "\nLHS: "
          ac = "\nRHS: "
        case 9 => sb.addAll("Comparison called on expressions of different types at \n")
          ex = "\nLHS: "
          ac = "\nRHS: "
        case 10 => sb.addAll("Comparison called on expression not of type int or char at \n")
          ex = "\nLHS: "
          ac = "\nRHS: "
        case 11 => sb.addAll("Binary operator called on unexpected type on LHS at \n")
        case 12 => sb.addAll("Binary operator called on unexpected type on RHS at \n")
        case 13 => sb.addAll("Array with mismatched types at \n")
      }
      sb.addAll(line + ex + expected.toString + ac + actual.toString)
      sb.toString
    }
  }

  case class IdentErrorLine(ident: String, line: String, errType: Int) extends SemanticErrorLine {
    override def toString: String = {
      val sb = new StringBuilder("Invalid identifier at " + line + "\n" +
        "Identifier \"" + ident + "\" ")
      errType match {
        case 1 => sb.addAll("is undefined.")
        case 2 => sb.addAll("is already in scope, cannot be redefined")
        case 3 => sb.addAll("is not a function identifier")
        case 4 => sb.addAll("is not an array identifier")
        case 5 => sb.addAll("is not a pair identifier")
      }
      sb.toString()
    }
  }

  case class FuncErrorLine(argTypes: List[Type], paramTypes: List[Type], line: String) extends SemanticErrorLine {
    override def toString: String = "Incorrect function arguments at \n" + line +
      "\nExpected: (" + flatten(paramTypes, ", ") +
      ")\nActual: (" + flatten(argTypes, ", ") + ")"
  }

  case class MessageErrorLine(msg: String, line: String) extends SemanticErrorLine {
    override def toString: String = msg + " at \n" + line
  }

  case class ParseError(pos: (Int, Int), lines: ParseErrorLines) {
    override def toString: String = "Syntax Error at line: " +
                                    pos._1 + " position: " + pos._2 + "\n" +
                                    lines.toString
  }

  private def generateArrow(errorPointsAt: Int): String = {
    val sb = new StringBuilder()
    for (_ <- 1 to errorPointsAt) {
      sb.addOne(' ')
    }
    sb.addOne('^')
    sb.toString()
  }

  case class ParseLineInfo(line: String, linesBefore: Seq[String], errorPointsAt: Int) {
    override def toString: String = {
      val sb = new StringBuilder()
      if (linesBefore.nonEmpty) {
        sb.addAll(linesBefore.reduce((a, b) => a + "\n" + b) + "\n")
      }
      sb.addAll(line + "\n" + generateArrow(errorPointsAt) + "\n")
      sb.toString()
    }
  }

  private def flatten[T](iterable: Iterable[T], delim: String): String = {
    if (iterable.isEmpty) {
      return ""
    }
    iterable.map(a => a.toString).reduce((x, y) => x + delim + y)
  }

  sealed trait ParseErrorLines

  case class VanillaError(unexpected: Option[ParseErrorItem],
                          expected: Set[ParseErrorItem],
                          reasons: Set[String],
                          lineInfo: ParseLineInfo)
    extends ParseErrorLines {
    override def toString: String = lineInfo.toString +
      "Unexpected: " + unexpected.getOrElse("None").toString + "\n" +
      "Expected: " + flatten(expected, " or ") +
      "\n" + flatten(reasons, "\n")
  }

  case class SpecialisedError(msgs: Set[String], lineInfo: ParseLineInfo)
    extends ParseErrorLines {
    override def toString: String = lineInfo.toString + flatten(msgs, "\n")
  }

  sealed trait ParseErrorItem

  case class ParseRaw(item: String) extends ParseErrorItem {
    override def toString: String = "\"" + item + "\""
  }

  case class ParseNamed(item: String) extends ParseErrorItem {
    override def toString: String = item
  }

  case object ParseEndOfInput extends ParseErrorItem {
    override def toString: String = "end of input"
  }

  class ParseErrorBuilder extends ErrorBuilder[ParseError] {
    override def format(pos: Position, source: Source, lines: ParseErrorLines): ParseError = {
      ParseError(pos, lines)
    }

    override type Position = (Int, Int)
    override type Source = Unit

    override def pos(line: Int, col: Int): Position = (line, col)

    override def source(sourceName: Option[String]): Source = ()

    override type ErrorInfoLines = ParseErrorLines

    override def vanillaError(unexpected: UnexpectedLine,
                              expected: ExpectedLine,
                              reasons: Messages,
                              line: LineInfo): ErrorInfoLines =
      VanillaError(unexpected, expected, reasons, line)

    override def specialisedError(msgs: Messages, line: LineInfo): ErrorInfoLines =
      SpecialisedError(msgs, line)

    override type ExpectedItems = Set[ParseErrorItem]
    override type Messages = Set[Message]

    override def combineExpectedItems(alts: Set[Item]): ExpectedItems = alts

    override def combineMessages(alts: Seq[Message]): Messages =
      alts.toSet

    override type UnexpectedLine = Option[ParseErrorItem]
    override type ExpectedLine = Set[ParseErrorItem]
    override type Message = String
    override type LineInfo = ParseLineInfo

    override def unexpected(item: Option[Item]): UnexpectedLine = item

    override def expected(alts: ExpectedItems): ExpectedLine = alts

    override def reason(reason: String): Message = reason

    override def message(msg: String): Message = msg

    override def lineInfo(line: String, linesBefore: Seq[String],
                          linesAfter: Seq[String], errorPointsAt: Int): LineInfo =
      ParseLineInfo(line, linesBefore, errorPointsAt)


    override val numLinesBefore: Int = 1
    override val numLinesAfter: Int = 0
    override type Item = ParseErrorItem
    override type Raw = ParseRaw
    override type Named = ParseNamed
    override type EndOfInput = ParseEndOfInput.type

    override def raw(item: String): Raw = ParseRaw(item)

    override def named(item: String): Named = ParseNamed(item)

    override val endOfInput: EndOfInput = ParseEndOfInput
  }

}
