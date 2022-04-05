package compiler

import compiler.ErrorUtils.{ParseError, ParseErrorBuilder}
import compiler.generator.ASMGenerator
import compiler.parser.ast.Program
import compiler.semantics.Analyser
import parsley.{Failure, Success}

import scala.io.BufferedSource

object Compiler {
  val NO_ERROR: Int = 0
  val SYNTAX_ERROR: Int = 100
  val SEMANTIC_ERROR: Int = 200

  def compileAndExit(path: String): Unit = {
    sys.exit(compile(path, generateCode = true))
  }

  def compile(path: String, generateCode: Boolean): Int = {
    import scala.io.Source.fromFile
    val source: BufferedSource = fromFile(path)
    println("Reading...")
    val lines: String = try source.mkString finally source.close()
    println("Parsing...")
    implicit val eb: ParseErrorBuilder = new ParseErrorBuilder
    Program().parse[ParseError](lines) match {
      case Failure(err) =>
        println(err)
        SYNTAX_ERROR

      case Success(ast) =>
        println("Analysing...")
        val analyser = new Analyser()
        val errors = analyser.analyseProgram(ast, lines)
        errors.foreach(e => println(e))

        if (errors.isEmpty) {
          if (!generateCode) return NO_ERROR

          println("Generating...")
          val generator = new ASMGenerator(ast)
          generator.generateASM(getFileBaseName(path))
          println("Done")
          NO_ERROR
        } else {
          SEMANTIC_ERROR
        }
    }
  }


  private def getFileBaseName(path: String): String = {
    val arr = path.split("[/.]")
    arr(arr.length - 2)
  }
}
