package integration

import compiler.Compiler.{NO_ERROR, SEMANTIC_ERROR, SYNTAX_ERROR, compile}
import org.scalatest.FunSuite
import org.scalatest.Matchers.{be, convertToAnyShouldWrapper}

import java.io.File

class FrontEndIntegrationTest extends FunSuite {

  test("Front end valid tests") {
    runTests("wacc_examples/valid", NO_ERROR)
  }

  test("Front end syntax error tests") {
    runTests("wacc_examples/invalid/syntaxErr", SYNTAX_ERROR)
  }

  test("Front end semantic error tests") {
    runTests("wacc_examples/invalid/semanticErr", SEMANTIC_ERROR)
  }

  def runTests(path: String, exitCode: Int): Unit = {
    val dir = new File(path)
    checkFiles(dir.listFiles, exitCode)
  }

  def checkFiles(files: Array[File], expExitCode: Int): Unit = {
    for (file <- files) {
      if (file.isDirectory) {
        checkFiles(file.listFiles, expExitCode)
      }
      else {
        if (file.getAbsolutePath.split("\\.").last != "wacc") {
          return
        }
        val accExitCode = compile(file.getAbsolutePath, generateCode = false)
        accExitCode should be (expExitCode)
      }
    }
  }
}
