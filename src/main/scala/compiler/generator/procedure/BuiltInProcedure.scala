package compiler.generator.procedure

import compiler.generator.utils.AssemblyState
import compiler.generator.utils.GeneratorUtils.getExprType
import compiler.generator.utils.InstrUtils.Cond.{AL, CS, Cond, EQ, LT, NE}
import compiler.generator.utils.InstrUtils._
import compiler.generator.utils.OperandUtils.Register.{Register, r0, r1, r2}
import compiler.generator.utils.OperandUtils.{ImmediateAddress, Label, OffsetByInteger, RegAsOperand}
import compiler.parser.ast.expr.Expr
import compiler.parser.ast.types.Type
import compiler.parser.ast.types.Type.ArrayType
import compiler.parser.ast.types.Type.BaseType.{BoolType, CharType, IntType, StringType}
import compiler.semantics.VarSymbolTable

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object BuiltInProcedure {
  // Store all the built in procedures that we may need when generating code

  sealed trait BuiltInProcedure {
    // Name of the procedure
    val branchName: String

    // Store the procedure dependencies - for example runtime error has print string as a dependency
    val procDependencies: List[BuiltInProcedure] = Nil

    // Store the data dependencies - strings which need to be added to the dataSection
    val dataDependencies: List[String] = Nil

    // Setup all the dependencies - add data to dataSection, add procedures to codeSection
    def setup(as: AssemblyState): Map[String, String] = {
      for (proc <- procDependencies) proc.addProcedure(None, as)
      val dataLabelMap = mutable.Map[String, String]()
      for (data <- dataDependencies) dataLabelMap.put(data, as.getLabel(data))
      dataLabelMap.toMap
    }

    // Sets up and adds the procedure to the assembly state
    def addProcedure(instruction: Option[ListBuffer[Instr]], as: AssemblyState, cond: Cond = AL): Unit = {
      if (!as.procedureAdded(branchName)) {
        val dataLabelMap = setup(as)
        // Mark procedure as added
        as.addProcedureToLog(branchName)
        // Add procedure to codeSection
        as.addProcedure(buildProc(dataLabelMap))
      }
      // Branch to the newly created procedure if the instructions are given
      if (instruction.isDefined) instruction.get += BL(cond = cond, label = branchName)
    }

    // Given a map of string dependencies to labels this function will generate a procedure
    // all the built in procedures will implement this function
    def buildProc(dataLabelMap: Map[String, String]): Procedure
  }

  // Add and branch to the appropriate print procedure
  def addPrintInstr(instructions: ListBuffer[Instr], expr: Expr, registers: List[Register],
                    symbolTable: VarSymbolTable, assemblyState: AssemblyState): Unit = {
    val exprType: Type = getExprType(expr, symbolTable)

    instructions += MOV(r0, RegAsOperand(registers.head))
    exprType match {
      // if char, handle with c 'putchar'
      case CharType =>
        instructions += BL(putCharLabel)
      // otherwise handle with our builtin procedures
      case _ =>
        val builtInPrint = exprType match {
          case IntType => PrintInt
          case StringType | ArrayType(CharType, 1) => PrintString
          case BoolType => PrintBool
          case _ => PrintReference
        }
        builtInPrint.addProcedure(Some(instructions), assemblyState)
    }
  }

  // Strings for procedures which are built into the operating system/ runtime environment
  val DIV: String = "__aeabi_idiv"
  val DIVMOD: String = "__aeabi_idivmod"

  /* --------------------------------- C function labels --------------------------------- */
  val flushLabel: String = "fflush"
  val printfLabel: String = "printf"
  val putStringLabel: String = "puts"
  val putCharLabel: String = "putchar"
  /* ------------------------------------------------------------------------------------- */

  // PRINT PROCEDURES

  case object PrintLn extends BuiltInProcedure {
    val nullTerminator: String = "\u0000"

    override val branchName: String = "p_print_ln"
    override val dataDependencies: List[String] = List(nullTerminator)

    override def buildProc(dataLabelMap: Map[String, String]): Procedure = Procedure(
      branchName,
      List(
        LDR(r0, Label(dataLabelMap(nullTerminator))),
        ADD(r0, r0, ImmediateAddress(4)),
        BL(putStringLabel),
        MOV(r0, ImmediateAddress(0)),
        BL(flushLabel)
      ), addPushPop = true
    )
  }



  case object PrintString extends BuiltInProcedure {
    val stringPrintFormatter: String = "%.*s\u0000"

    override val branchName: String = "p_print_string"
    override val dataDependencies: List[String] = List(stringPrintFormatter)

    override def buildProc(dataLabelMap: Map[String, String]): Procedure = Procedure(
      branchName,
      List(
        LDR(r1, OffsetByInteger(r0)),
        ADD(r2, r0, ImmediateAddress(4)),
        LDR(r0, Label(dataLabelMap(stringPrintFormatter))),
        ADD(r0, r0, ImmediateAddress(4)),
        BL(printfLabel),
        MOV(r0, ImmediateAddress(0)),
        BL(flushLabel)
      ), addPushPop = true
    )
  }


  case object PrintReference extends BuiltInProcedure {
    val referencePrintFormatter: String = "%p\u0000"

    override val branchName: String = "p_print_reference"
    override val dataDependencies: List[String] = List(referencePrintFormatter)

    override def buildProc(dataLabelMap: Map[String, String]): Procedure =
      Procedure(
        branchName,
        List(
          MOV(r1, RegAsOperand(r0)),
          LDR(r0, Label(dataLabelMap(referencePrintFormatter))),
          ADD(r0, r0, ImmediateAddress(4)),
          BL(printfLabel),
          MOV(r0, ImmediateAddress(0)),
          BL(flushLabel)
        ), addPushPop = true
      )
  }

  val intFormatter: String = "%d\u0000"

  case object PrintInt extends BuiltInProcedure {
    override val branchName: String = "p_print_int"
    override val dataDependencies: List[String] = List(intFormatter)

    override def buildProc(dataLabelMap: Map[String, String]): Procedure =
      Procedure(
        branchName,
        List(
          MOV(r1, RegAsOperand(r0)),
          LDR(r0, Label(dataLabelMap(intFormatter))),
          ADD(r0, r0, ImmediateAddress(4)),
          BL(printfLabel),
          MOV(r0, ImmediateAddress(0)),
          BL(flushLabel)
        ), addPushPop = true
      )
  }

  case object PrintBool extends BuiltInProcedure {
    lazy val trueString = "true\u0000"
    lazy val falseString = "false\u0000"

    override val branchName: String = "p_print_bool"
    override val dataDependencies: List[String] = List(trueString, falseString)

    override def buildProc(dataLabelMap: Map[String, String]): Procedure =
      Procedure(
        branchName,
        List(
          CMP(r0, ImmediateAddress(0)),
          LDR(r0, Label(dataLabelMap(trueString)), cond = NE),
          LDR(r0, Label(dataLabelMap(falseString)), cond = EQ),
          ADD(r0, r0, ImmediateAddress(4)),
          BL(printfLabel),
          MOV(r0, ImmediateAddress(0)),
          BL(flushLabel)
        ), addPushPop = true
      )
  }

  // READ PROCEDURES

  case object ReadInt extends BuiltInProcedure {
    override val branchName: String = "p_read_int"
    override val dataDependencies: List[String] = List(intFormatter)

    override def buildProc(dataLabelMap: Map[String, String]): Procedure =
      Procedure(
        branchName,
        List(
          MOV(r1, RegAsOperand(r0)),
          LDR(r0, Label(dataLabelMap(intFormatter))),
          ADD(r0, r0, ImmediateAddress(4)),
          BL("scanf")
        ), addPushPop = true
      )
  }

  case object ReadChar extends BuiltInProcedure {
    val charFormatter: String = " %c\u0000"

    override val branchName: String = "p_read_char"
    override val dataDependencies: List[String] = List(charFormatter)

    override def buildProc(dataLabelMap: Map[String, String]): Procedure =
      Procedure(
        branchName,
        List(
          MOV(r1, RegAsOperand(r0)),
          LDR(r0, Label(dataLabelMap(charFormatter))),
          ADD(r0, r0, ImmediateAddress(4)),
          BL("scanf")
        ), addPushPop = true
      )
  }

  // RUNTIME ERRORS

  case object RuntimeErr extends BuiltInProcedure {
    override val branchName: String = "p_throw_runtime_error"
    override val procDependencies: List[BuiltInProcedure] = List(PrintString)

    override def buildProc(dataLabelMap: Map[String, String]): Procedure =
      Procedure(
        branchName,
        List(
          BL(PrintString.branchName),
          MOV(r0, ImmediateAddress(-1)),
          BL("exit")
        )
      )
  }

  case object OverflowError extends BuiltInProcedure {
    lazy val overflowErrorMessage: String =
      "OverflowError: the result is too small/large to store in a 4-byte signed-integer.\n\u0000"

    override val branchName: String = "p_throw_overflow_error"
    override val procDependencies: List[BuiltInProcedure] = List(RuntimeErr)
    override val dataDependencies: List[String] = List(overflowErrorMessage)

    override def buildProc(dataLabelMap: Map[String, String]): Procedure =
      Procedure(
        branchName,
        List(
          LDR(r0, Label(dataLabelMap(overflowErrorMessage))),
          BL(RuntimeErr.branchName)
        )
      )
  }

  case object DivideByZeroError extends BuiltInProcedure {
    lazy val divideByZeroErrorMessage: String = "DivideByZeroError: divide or modulo by zero\n\u0000"

    override val branchName: String = "p_check_divide_by_zero"
    override val procDependencies: List[BuiltInProcedure] = List(RuntimeErr)
    override val dataDependencies: List[String] = List(divideByZeroErrorMessage)

    override def buildProc(dataLabelMap: Map[String, String]): Procedure =
      Procedure(
        branchName,
        List(
          CMP(r1, ImmediateAddress(0)),
          LDR(r0, Label(dataLabelMap(divideByZeroErrorMessage)), cond = EQ),
          BL(RuntimeErr.branchName, cond = EQ)
        ), addPushPop = true
      )
  }

  case object NullReferenceError extends BuiltInProcedure {
    lazy val nullReferenceErrorMessage: String = "NullReferenceError: dereference a null reference\n\u0000"

    override val branchName: String = "p_check_null_pointer"
    override val procDependencies: List[BuiltInProcedure] = List(RuntimeErr)
    override val dataDependencies: List[String] = List(nullReferenceErrorMessage)

    override def buildProc(dataLabelMap: Map[String, String]): Procedure =
      Procedure(
        branchName,
        List(
          CMP(r0, ImmediateAddress(0)),
          LDR(r0, Label(dataLabelMap(nullReferenceErrorMessage)), cond = EQ),
          BL(RuntimeErr.branchName, cond = EQ)
        ), addPushPop = true
      )
  }

  case object IndexOutOfBoundsCheck extends BuiltInProcedure {
    lazy val indexNegativeMessage: String = "ArrayIndexOutOfBoundsError: negative index\n\u0000"
    lazy val indexTooLargeMessage: String = "ArrayIndexOutOfBoundsError: index too large\n\u0000"

    override val branchName: String = "p_check_array_bounds"
    override val procDependencies: List[BuiltInProcedure] = List(RuntimeErr)
    override val dataDependencies: List[String] = List(indexTooLargeMessage, indexNegativeMessage)

    override def buildProc(dataLabelMap: Map[String, String]): Procedure =
      Procedure(
        branchName,
        List(
          CMP(r0, ImmediateAddress(0)),
          LDR(r0, address = Label(dataLabelMap(indexNegativeMessage)), cond = LT),
          BL(RuntimeErr.branchName, cond = LT),
          LDR(r1, address = OffsetByInteger(r1)),
          CMP(r0, RegAsOperand(r1)),
          LDR(r0, Label(dataLabelMap(indexTooLargeMessage)), cond = CS),
          BL(RuntimeErr.branchName, cond = CS),
        ), addPushPop = true
      )
  }
}
