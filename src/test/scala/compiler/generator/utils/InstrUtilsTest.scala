package compiler.generator.utils

import compiler.generator.utils.InstrUtils.{Cond, DoNotSetCond, Instr}
import compiler.generator.utils.OperandUtils.{ImmediateAddress, ImmediateMemory, Register}
import org.scalatest.FunSuite
import org.scalatest.Matchers.{be, convertToAnyShouldWrapper}

import scala.collection.mutable

class InstrUtilsTest extends FunSuite {
  test("canRepresentSimpleInstruction") {
    val instrAdd = InstrUtils.ADD(Register.r4, Register.r5, ImmediateAddress(3)).toString()
    instrAdd should be ("ADD r4, r5, #3")
  }

  test("canRepresentInstructions") {
    val program = mutable.ListBuffer[Instr]()
    program += InstrUtils.PUSH(List(Register.lr))
    program += InstrUtils.LDR(rd = Register.r0, address = ImmediateMemory(7))
    program += InstrUtils.BL("exit")
    program += InstrUtils.MOV(rd = Register.r0, op = ImmediateAddress(0))
    program += InstrUtils.POP(List(Register.pc))
    val programStr = program.fold("")((program, instr) => s"$program$instr\n")
    programStr should be ("PUSH {lr}\nLDR r0, =7\nBL exit\nMOV r0, #0\nPOP {pc}\n")
  }
}
