package compiler.generator.procedure

import compiler.generator.utils.GeneratorUtils.{INDENT, NEWLINE}
import compiler.generator.utils.InstrUtils._
import compiler.generator.utils.OperandUtils.Register._

import scala.collection.mutable

// Represents an assembly procedure
// This class is used for representing any procedure such as builtins, functions, if-else ...
case class Procedure(name: String, instructions: List[Instr]) extends Instr {
  override def toString: String = {
    val sb: mutable.StringBuilder = new StringBuilder()
    if (name != "") sb.append(name + ":" + NEWLINE)
    for (instr <- instructions) {
      instr match {
        case proc: Procedure => sb.append(proc.toString)
        case _ => sb.append(INDENT + instr.toString + NEWLINE)
      }
    }
    sb.toString()
  }
}

object Procedure {
  // Apply method for constructing a Procedure that pops and pushes
  def apply(name: String, instructions: List[Instr], addPushPop: Boolean): Procedure = {
    if (addPushPop) return Procedure(name, PUSH(List(lr)) +: instructions :+ POP(List(pc)))
    Procedure(name, instructions)
  }
}
