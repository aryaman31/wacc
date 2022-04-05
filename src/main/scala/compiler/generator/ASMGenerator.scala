package compiler.generator

import compiler.generator.ProcedureGenerator._
import compiler.generator.utils.ASMFileWriter.writeToFile
import compiler.generator.utils.GeneratorUtils._
import compiler.generator.utils._
import compiler.parser.ast.Program
import compiler.semantics.SymbolTable.VarSymbol

import scala.language.postfixOps

class ASMGenerator(program: Program) {
  
  val assemblyState: AssemblyState = AssemblyState(program)

  // Top level function which generates the internal assembly representation
  // and then writes the assembly to the given fileName
  def generateASM(fileName: String): Unit = {
    generateCode()
    writeToFile(fileName, assemblyState)
  }

  // Generates the internal representation of the assembly file,
  // by mutating the AssemblyState val
  private def generateCode(): Unit = {
    for (func <- program.funcDefs) {
      // Initialise internal numBytes field for managing the stack usage
      func.block.symbolTable.get.getNumBytesUsed
      // Initialise the number of bytes for parameters
      var remParamBytes = func.block.symbolTable.get.paramNumBytes
      for (param <- func.funcSig.params.reverse) {
        val v = func.block.symbolTable.get.getIdentifierSymbol(param.name.ident).get
        v match {
          case v: VarSymbol =>
            if (v.isParam) {
              remParamBytes -= getBytes(param.paramType)
              v.setParamOffset(remParamBytes)
            }
          case _ =>
        }
      }
      // Add function as a procedure
      assemblyState.addProcedure(convertToProcedure("f_" + func.funcSig.name.ident, func.block, procType = FuncProcedure, isFunc = true, assemblyState=assemblyState))
    }
    // Add main block as procedure
    assemblyState.addProcedure(convertToProcedure("main", program.block, assemblyState=assemblyState))
  }
}


