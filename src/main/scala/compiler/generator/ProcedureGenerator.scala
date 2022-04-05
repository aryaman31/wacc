package compiler.generator

import compiler.generator.BlockASMGenerator.convertBlockToAsm
import compiler.generator.procedure.Procedure
import compiler.generator.utils.AssemblyState
import compiler.generator.utils.GeneratorUtils.moveStackPointer
import compiler.generator.utils.InstrUtils._
import compiler.generator.utils.OperandUtils.ImmediateMemory
import compiler.generator.utils.OperandUtils.Register.{lr, pc, r0, r10, r11, r12, r4, r5, r6, r7, r8, r9}
import compiler.parser.ast.stats.Stat.Block
import compiler.semantics.VarSymbolTable

import scala.collection.mutable

object ProcedureGenerator {
  // These are for representing the status of a processed block
  // Branches and returns are mutually exclusive since for returns
  // we stop processing and for branches we recurse into a new convertToProcedure call
  sealed trait BlockExitStatus
  case object NORMAL extends BlockExitStatus
  case object BRANCHED extends BlockExitStatus
  case object RETURNED extends BlockExitStatus

  // Types of procedure - used to know when to add instructions such as
  // incrementing/decrementing the sp, or pushing/popping lr/pc
  sealed trait ProcedureType
  case object FullProcedure extends ProcedureType
  case object EndProcedure extends ProcedureType
  case object NestedBranch extends ProcedureType
  case object NestedReturnBranch extends ProcedureType
  case object StartProcedure extends ProcedureType
  case object FuncProcedure extends ProcedureType
  case object NewScopeBranch extends ProcedureType
  case object EndFuncProcedure extends ProcedureType

  // Function for generating a procedure from a block
  // Handles all the different cases such as functions/ returns/ if-else/ scoping...
  def convertToProcedure(name: String, block: Block, procType: ProcedureType = FullProcedure,
                         instructions: mutable.ListBuffer[Instr] = mutable.ListBuffer(),
                         inNest: Boolean = false, isFunc: Boolean = false, assemblyState: AssemblyState): Procedure = {
    var procedureType = procType
    val rootSymbolTable: VarSymbolTable = block.symbolTable.get
    val totalBytesUsed = rootSymbolTable.getNumBytesUsed
    val res = convertBlockToAsm(block, rootSymbolTable, instructions, List(r4, r5, r6, r7, r8, r9, r10, r11, r12),
      inNest, isFunc = isFunc, assemblyState)
    // If we branched from this block (using if-else or while)
    // then we need to make sure that we don't decrement the sp too early
    // The decrement should be handled by the procedure for the remaining block
    if (res == BRANCHED) {
      procedureType = procType match {
        case FullProcedure | FuncProcedure => StartProcedure
        case EndProcedure | EndFuncProcedure => NestedBranch
        case t => t
      }
    }
    // If there is a return in the block, then we need to pop
    if (inNest && res == RETURNED) procedureType = NestedReturnBranch

    var asm: List[Instr] = Nil
    totalBytesUsed match {
      case 0 => asm = instructions.toList
      case _ => asm =
        procedureType match {
          case EndProcedure | EndFuncProcedure => instructions.toList ::: moveStackPointer(totalBytesUsed)
          case StartProcedure | NestedReturnBranch => moveStackPointer(-totalBytesUsed) ::: instructions.toList
          case _ => moveStackPointer(-totalBytesUsed) ::: instructions.toList ::: moveStackPointer(totalBytesUsed)
        }
    }

    val instrList = procedureType match {
      case FullProcedure => PUSH(List(lr)) +: asm ::: endProcedure
      case FuncProcedure => PUSH(List(lr)) +: asm ::: endFuncProcedure
      case EndProcedure => asm ::: endProcedure
      case EndFuncProcedure => asm ::: endFuncProcedure
      case NestedBranch => instructions.toList
      case NestedReturnBranch => asm ::: endFuncProcedure
      case NewScopeBranch => asm
      case StartProcedure => PUSH(List(lr)) +: asm
    }
    Procedure(name, instrList)
  }

  val endProcedure = List(LDR(r0, ImmediateMemory(0)), POP(List(pc)), LTORG)
  val endFuncProcedure = List(POP(List(pc)), LTORG)

}
