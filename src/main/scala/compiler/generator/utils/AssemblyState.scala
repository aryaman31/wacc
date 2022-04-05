package compiler.generator.utils

import compiler.generator.procedure.Procedure
import compiler.parser.ast.Program
import compiler.semantics.FuncSymbolTable

import scala.collection.mutable

// other things should probably be added to this
case class AssemblyState(program: Program) {
  val funcSymbolTable: FuncSymbolTable = program.funcSymbolTable.get

  // rawString -> label
  val dataSection: mutable.Map[String, String] = mutable.LinkedHashMap()

  // list of procedures including main
  val codeSection: mutable.ListBuffer[Procedure] = mutable.ListBuffer()

  // log of added procedure flags, so we do not add duplicate assembly code
  private var logOfAddedProcedures: Set[String] = Set.empty

  private def msg(i: Int): String = s"msg$i"

  // check if a procedure (labelled by its branch name) has been added
  // used for built in procedures
  def procedureAdded(s: String) : Boolean = {
    logOfAddedProcedures.contains(s)
  }

  // Add a procedure to the procedure log (means it has been added to codeSection)
  // Used for built in procedures
  def addProcedureToLog(s: String) : Unit = {
    logOfAddedProcedures += s
  }

  // Returns the label for a given string
  // Returns the same label when queried on the same rawString (only adds one data)
  def getLabel(rawString: String): String = {
    dataSection.get(rawString) match {
      case None =>
        val label = msg(dataSection.size)
        dataSection.put(rawString, label)
        label
      case Some(label: String) =>
        label
    }
  }

  private var branchNum = 0
  // Returns a new branch name for a branch
  // Used in the cases of ifs/ elses/ whiles/ splitting a block
  def getBranchLabel: String = {
    val branchName = s"L$branchNum"
    branchNum += 1
    branchName
  }

  // Add a procedure to the codeSection (used for builtins and for other procedures)
  def addProcedure(procedure: Procedure) : Unit = {
    codeSection.addOne(procedure)
  }
}
