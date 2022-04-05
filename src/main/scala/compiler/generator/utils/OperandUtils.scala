package compiler.generator.utils

import compiler.generator.utils.OperandUtils.Register.Register


object OperandUtils {
  // Stores the case classes for representing operands and addresses

  sealed trait Operand
  sealed trait Address extends Operand

  sealed trait OffsetAddress extends Address

  case class OffsetByRegister(reg: Register, regOffset: Register) extends OffsetAddress {
    override def toString: String = s"[$reg, $regOffset]"
  }

  case class OffsetByInteger(reg: Register, offset: Int = 0, writeBack: Boolean = false) extends OffsetAddress {
    override def toString: String =
      if (offset != 0) {
        if (writeBack) {
          s"[$reg, #$offset]!"
        } else {
          s"[$reg, #$offset]"
        }
      }
      else {
        s"[$reg]"
      }
  }

  case class ImmediateChar(char: Char) extends Address {
    override def toString = s"#'$char'"
  }

  case class ImmediateAddress(address: Int) extends Address {
    override def toString = s"#$address"
  }

  case class ImmediateMemory(address: Int) extends Address {
    override def toString = s"=$address"
  }

  case class Label(label: String) extends Address {
    override def toString = s"=$label"
  }

  case object Register extends Enumeration {
    type Register = Value
    val r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, sp, lr, pc = Value
  }

  case class RegAsOperand(reg: Register) extends Operand {
    override def toString: String = reg.toString
  }


  case class LSL(rm: Register, op: Operand) extends Operand

  case class LSR(rm: Register, op: Operand) extends Operand

  case class ASR(rm: Register, op: Operand) extends Operand {
    override def toString: String =
      s"$rm, ASR $op"
  }

  case class ROR(rm: Register, op: Operand) extends Operand
}
