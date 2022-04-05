package compiler.generator.utils

import compiler.generator.utils.InstrUtils.Cond.{AL, Cond}
import compiler.generator.utils.OperandUtils.Register.Register
import compiler.generator.utils.OperandUtils._

object InstrUtils {
  // Stores the classes / objects for representing an assembly file
  // (along with the operands file and procedure file)

  case class Offset(register: Register, offset: Int)

  object Cond extends Enumeration {
    type Cond = Value
    val EQ, NE, VS, VC, GE, LE, GT, LT, CS = Value
    val AL: Value = Value("")

    // Inverts a condition
    def opposite(cond: Cond): Cond = {
      cond match {
        case EQ => NE
        case NE => EQ
        case GE => LT
        case LT => GE
        case GT => LE
        case LE => GT
        case VS => VC
        case VC => VS
      }
    }
  }

  sealed trait Setter

  case object SetCond extends Setter {
    override def toString: String = "S"
  }

  case object DoNotSetCond extends Setter {
    override def toString: String = ""
  }

  trait Instr

  case class ADD(rd: Register, rn: Register, op: Operand, cond: Cond = AL, S: Setter = DoNotSetCond) extends Instr {
    override def toString = s"ADD$cond$S $rd, $rn, $op"
  }

  case class ADC(rd: Register, rn: Register, op: Operand, cond: Cond = AL, S: Setter = DoNotSetCond) extends Instr

  case class SUB(rd: Register, rn: Register, op: Operand, cond: Cond = AL, S: Setter = DoNotSetCond) extends Instr {
    override def toString = s"SUB$cond$S $rd, $rn, $op"
  }

  case class SBC(rd: Register, rn: Register, op: Operand, cond: Cond = AL, S: Setter = DoNotSetCond  ) extends Instr

  case class RSB(rd: Register, rn: Register, op: Operand, cond: Cond = AL, S: Setter = DoNotSetCond  ) extends Instr {
    override def toString = s"RSB$cond$S $rd, $rn, $op"
  }

  case class RSC(rd: Register, rn: Register, op: Operand, cond: Cond = AL, S: Setter = DoNotSetCond  ) extends Instr

  case class MUL(rd: Register, rm: Register, rs: Register, cond: Cond = AL, S: Setter = DoNotSetCond  ) extends Instr {
    override def toString = s"MUL$cond$S $rd, $rm, $rs"
  }

  case class SMULL(rdlo: Register, rdhi: Register, rm: Register, rs: Register, cond: Cond = AL, S: Setter = DoNotSetCond  ) extends Instr {
    override def toString = s"SMULL$cond$S $rdlo, $rdhi, $rm, $rs"
  }

  case class MLA(rd: Register, rm: Register, rs: Register, rn: Register, cond: Cond = AL, S: Setter = DoNotSetCond  ) extends Instr {
    override def toString = s"MLA$cond$S $rd, $rm, $rs, $rn"
  }

  case class CMP(rn: Register, op: Operand, cond: Cond = AL) extends Instr {
    override def toString = s"CMP$cond $rn, $op"
  }

  // Might not need CMN
  case class CMN(rn: Register, op: Operand, cond: Cond = AL) extends Instr

  case class MOV(rd: Register, op: Operand, cond: Cond = AL, S: Setter = DoNotSetCond  ) extends Instr {
    override def toString = s"MOV$cond$S $rd, $op"
  }

  // Might not need MVN, TST and TEQ
  case class MVN(rd: Register, op: Operand, cond: Cond = AL, S: Setter = DoNotSetCond  ) extends Instr

  case class TST(rn: Register, op: Operand, cond: Cond = AL) extends Instr

  case class TEQ(rn: Register, op: Operand, cond: Cond = AL) extends Instr

  case class AND(rd: Register, rn: Register, op: Operand, cond: Cond = AL, S: Setter = DoNotSetCond  ) extends Instr {
    override def toString = s"AND$cond$S $rd, $rn, $op"
  }

  case class ORR(rd: Register, rn: Register, op: Operand, cond: Cond = AL, S: Setter = DoNotSetCond  ) extends Instr {
    override def toString = s"ORR$cond$S $rd, $rn, $op"
  }

  case class XOR(rd: Register, rn: Register, op: Operand, cond: Cond = AL, S: Setter = DoNotSetCond  ) extends Instr {
    override def toString = s"EOR$cond$S $rd, $rn, $op"
  }

  case class B(label: String, cond: Cond = AL) extends Instr {
    override def toString = s"B$cond $label"
  }

  case class BL(label: String, cond: Cond = AL) extends Instr {
    override def toString = s"BL$cond $label"
  }

  case class LDR(rd: Register, address: Address, cond: Cond = AL, S: Setter = DoNotSetCond, Signed: Boolean = false, ByteOp: Boolean = false) extends Instr {
    override def toString: String = {
      val str = new StringBuilder("")
      if (Signed) {
        str.append("S")
      }
      if (ByteOp) {
        str.append("B")
      }
      s"LDR$str$cond$S $rd, $address"
    }
  }

  case class STR(rd: Register, address: Address, cond: Cond = AL, S: Setter = DoNotSetCond, Signed: Boolean = false, ByteOp: Boolean = false) extends Instr {
    override def toString: String = {
      val str = new StringBuilder("")
      if (Signed) {
        str.append("S")
      }
      if (ByteOp) {
        str.append("B")
      }
      s"STR$str$cond$S $rd, $address"
    }
  }

  case class PUSH(regList: List[Register]) extends Instr {
    override def toString = s"PUSH {${regList.mkString(", ")}}"
  }

  case class POP(regList: List[Register]) extends Instr {
    override def toString = s"POP {${regList.mkString(", ")}}"
  }

  case object LTORG extends Instr {
    override def toString: String = ".ltorg"
  }

}
