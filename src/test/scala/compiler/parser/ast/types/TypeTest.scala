package compiler.parser.ast.types

import compiler.parser.ast.types.Type.BaseType._
import compiler.parser.ast.types.Type.{ArrayType, BaseType, PairType}
import org.scalatest.FunSuite
import org.scalatest.Matchers.{be, convertToAnyShouldWrapper}
import parsley.Success

class TypeTest extends FunSuite {
  test("BaseTypeParserParsesInt") {
    val result = BaseType().parse("int x ")
    result should be (Success(IntType))
  }

  test("BaseTypeParserParsesChar") {
    val result = BaseType().parse("char x")
    result should be (Success(CharType))
  }

  test("BaseTypeParserParsesBool") {
    val result = BaseType().parse("bool x")
    result should be (Success(BoolType))
  }

  test("BaseTypeParserParsesString") {
    val result = BaseType().parse("string x")
    result should be (Success(StringType))
  }

  test("BaseTypeParserParsesWithTrailingWhitespace") {
    val result = BaseType().parse("string    ")
    result should be (Success(StringType))
  }

  test("ArrayTypeParserParsesCorrectly") {
    val result = Type.parser.parse("int[] ")
    result should be (Success(ArrayType(IntType, 1)))
  }

  test("PairTypeParserParsesCorrectly") {
    val result = Type.parser.parse("pair(int,int) ")
    result should be (Success(PairType(IntType, IntType)))
  }

  test("PairTypeParserParsesCorrectlyWithWhitespace") {
    val result = Type.parser.parse("pair(  int  ,  int  )  ")
    print(result)
    result should be (Success(PairType(IntType, IntType)))
  }
}
