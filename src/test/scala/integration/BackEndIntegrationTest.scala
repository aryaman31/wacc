package integration

import compiler.Compiler.compile
import org.scalatest.FunSuite

import java.io.File
import scala.io.Source
import scala.language.postfixOps
import scala.sys.process._

class BackEndIntegrationTest extends FunSuite {

  test("Advanced Tests") {
    runTests("wacc_examples/valid/advanced", "advanced")
  }

  test("Array Tests") {
    runTests("wacc_examples/valid/array", "array")
  }

  test("Exit Tests") {
    runTests("wacc_examples/valid/basic/exit", "exit")
  }

  test("Skip Tests") {
    runTests("wacc_examples/valid/basic/skip", "skip")
  }

  test("Expression Tests") {
    runTests("wacc_examples/valid/expressions", "expressions")
  }

  test("Function Tests") {
    runTests("wacc_examples/valid/function", "function")
  }

  test("If Tests") {
    runTests("wacc_examples/valid/if", "if")
  }

  test("Print Tests") {
    runTests("wacc_examples/valid/IO/print", "print")
  }

  test("Read Tests") {
    runTests("wacc_examples/valid/IO/read", "read")
  }

  test("Advanced IO Tests") {
    runTests("wacc_examples/valid/IO/advancedIO", "advancedIO")
  }

  test("Pair Tests") {
    runTests("wacc_examples/valid/pairs", "pairs")
  }

  test("Runtime Error Tests") {
    runTests("wacc_examples/valid/runtimeErr", "runtimeErr")
  }

  test("Scope Tests") {
    runTests("wacc_examples/valid/scope", "scope")
  }

  test("Sequence Tests") {
    runTests("wacc_examples/valid/sequence", "sequence")
  }

  test("Variable Tests") {
    runTests("wacc_examples/valid/variables", "variables")
  }

  test("While Tests") {
    runTests("wacc_examples/valid/while", "while")
  }

  def runTests(path: String, testType: String): Unit = {
    val dir = new File(path)
    val expectedOutputPath = "wacc_examples/outputs/" + testType + ".out"
    val inputPath = "wacc_examples/inputs/" + testType + ".txt"
    val outputMap = constructExpectedOutputs(expectedOutputPath)
    val inputMap = constructInputs(inputPath)
    checkFiles(path: String, dir.listFiles, inputMap, outputMap)
  }

  def checkFiles(path: String, files: Array[File], inputs: Map[String, String], expectedOutputs: Map[String, ExpectedOutput]): Unit = {
    for (file <- files) {
      if (file.isDirectory) {
        checkFiles(path + "/" + file.getName, file.listFiles, inputs, expectedOutputs)
      }
      else {
        var fileName = file.getName
        if (fileName.contains(".wacc")) {
          fileName = fileName.replace(".wacc", "")
          val expectedOutput = expectedOutputs.getOrElse(fileName, ExpectedOutput(0, "\n"))
          val input = inputs.getOrElse(fileName, "")
          val fullPath = s"$path/$fileName"

          compile(fullPath + ".wacc", generateCode = true)

          (("echo \"" + input + "\"") #| s"arm-linux-gnueabi-gcc -o $fileName -mcpu=arm1176jzf-s -mtune=arm1176jzf-s $fileName.s").!
          val os = new java.io.ByteArrayOutputStream
          val exitCode: Int = ((("echo \"" + input + "\"") #|  s"qemu-arm -L /usr/arm-linux-gnueabi $fileName")  #> os).!
          os.close()
          withClue("test: " + fileName + " was unsuccessful. \nExpected: " +
            expectedOutput.expectedPrint + "\nActual: " + os.toString().replace("\r","\n") + "\n") {
            assert((os.toString().replace("\r", "\n")).equals(expectedOutput.expectedPrint))}
          withClue("test: " + fileName + " was unsuccessful, \nExpected exit code: " +
            expectedOutput.expectedExitCode + "\nActual: " + "exitCode" + "\n") {
            assert(exitCode == expectedOutput.expectedExitCode) }
        }
      }
    }
  }

  def constructInputs(inputPath: String): Map[String, String] = {
    val map = collection.mutable.Map[String, String]()
    val source = Source.fromFile(inputPath)
    val fileContents = source.getLines().mkString("\n")
    val inputsList = fileContents.split("===========================================================\n")
    for (inputSection <- inputsList) {
      if (inputSection != "") {
        val splitSection = inputSection.split(":")
        val fileName = splitSection(0)
        val input = splitSection(1)
        map.put(fileName, input)
      }
    }
    map.toMap
  }

  def constructExpectedOutputs(outputPath: String): Map[String, ExpectedOutput] = {
    val map = collection.mutable.Map[String, ExpectedOutput]()
    val source = Source.fromFile(outputPath)
    val fileContents = source.getLines().mkString("\n")
    val splitFileContents = fileContents.split("--Test:").drop(1)
    for (section <- splitFileContents) {
      val splitSection = section.split("===========================================================\n")
      val fileName = splitSection(0).split("\\.wacc")(0).replace(" ", "")
      val expectedPrint = splitSection(1)
      var expectedExitCode = 0
      if (!splitSection(2).contains("timed out")) {
        expectedExitCode = splitSection(2).replace("\n", "").
          replace("The exit code is ", "").
          replace("\n","").replace(".-- Finished","").toInt
      }
      val expectedOutput = ExpectedOutput(expectedExitCode, expectedPrint)
      map(fileName) = expectedOutput
    }
    source.close
    map.toMap
  }

  case class ExpectedOutput(expectedExitCode: Int, expectedPrint: String)
}
