package compiler.generator

import compiler.generator.RhsLhsASMGenerator.loadArrayElemExprAddress
import compiler.generator.procedure.BuiltInProcedure._
import compiler.generator.utils.AssemblyState
import compiler.generator.utils.GeneratorUtils._
import compiler.generator.utils.InstrUtils.Cond.{Cond, EQ, GE, GT, LE, LT, NE, VS, opposite}
import compiler.generator.utils.InstrUtils._
import compiler.generator.utils.OperandUtils.Register.{Register, r0, r1, sp}
import compiler.generator.utils.OperandUtils._
import compiler.parser.ast.expr.Expr._
import compiler.parser.ast.expr.PairElem.{Fst, Snd}
import compiler.parser.ast.expr.{Expr, PairElem}
import compiler.parser.ast.types.Type.BaseType.BoolType
import compiler.semantics.VarSymbolTable

import scala.collection.mutable.ListBuffer

object ExprASMGenerator {
  def convertExprToAsm(expr: Expr, symbolTable: VarSymbolTable, instructions: ListBuffer[Instr],
                       registers: List[Register], assemblyState: AssemblyState): Unit = {

    expr match {
      case unaryOP: UnaryOp =>
        convertUnaryOp(unaryOP, symbolTable, instructions, registers, assemblyState)
      case binOp: BinOp =>
        convertBinOp(binOp, symbolTable, instructions, registers, assemblyState)
      case int: IntLiter =>
        val value = int.value
        // if it is an exit code, then we must limit it to values between 0 and 255
        instructions += LDR(registers.head, ImmediateMemory(value))
      case str: StrLiter =>
        val label = assemblyState.getLabel(str.value)
        instructions += LDR(registers.head, Label(label))

      case chr: ChrLiter =>
        if (chr.value == '\u0000') instructions += MOV(rd = registers.head, op = ImmediateAddress(0))
        else instructions += MOV(registers.head, ImmediateChar(chr.value))

      case bool: BoolLiter =>
        val addr = if (bool.value) 1 else 0
        instructions += MOV(registers.head, ImmediateAddress(addr))

      case pairElem: PairElem =>
        convertExprToAsm(pairElem.expr, symbolTable, instructions, registers, assemblyState)
        instructions += MOV(r0, RegAsOperand(registers.head))
        NullReferenceError.addProcedure(Some(instructions), assemblyState)
        pairElem match {
          case Fst(_) =>
            instructions += LDR(registers.head, OffsetByInteger(registers.head))
          case Snd(_) =>
            instructions += LDR(registers.head, OffsetByInteger(registers.head, offset = WORD_BYTES))
        }
        instructions += LDR(registers.head, OffsetByInteger(registers.head))

      case _: PairLiter =>
        // not sure what to do here, just set it to null character
        instructions += LDR(registers.head, ImmediateMemory(0))

      case arrayElemExpr: ArrayElemExpr =>
        // sets registers.head to the address of the array pointer
        loadArrayElemExprAddress(arrayElemExpr, symbolTable, instructions, registers, assemblyState)
        val exprType = getExprType(arrayElemExpr, symbolTable)
        val numBytes = getBytes(exprType)
        instructions += LDR(registers.head, OffsetByInteger(registers.head),
          Signed = exprType == BoolType, ByteOp = (numBytes == BYTE_SIZE))

      case ident: Ident =>
        val idType = symbolTable.getIdentifierSymbol(ident.ident).get.varType
        // gets the exact position of the identifiers value with respect to the
        instructions += LDR(rd = registers.head, OffsetByInteger(sp, symbolTable.getOffset(ident.ident)),
          Signed = idType == BoolType, ByteOp = getBytes(idType) == BYTE_SIZE)
    }
  }

  def convertUnaryOp(unaryOp: UnaryOp, symbolTable: VarSymbolTable,
                     instructions: ListBuffer[Instr], registers: List[Register], assemblyState: AssemblyState): Unit = {
    unaryOp match {
      case Expr.Bang(expr) =>
        convertExprToAsm(expr, symbolTable, instructions, registers, assemblyState = assemblyState)
        instructions += XOR(registers.head, registers.head, ImmediateAddress(1))
      case Expr.Negate(expr) =>
        convertExprToAsm(expr, symbolTable, instructions, registers, assemblyState = assemblyState)
        instructions += RSB(registers.head, registers.head, ImmediateAddress(0), S = SetCond)
        OverflowError.addProcedure(Some(instructions), assemblyState, cond = VS)
      case Expr.Len(expr) =>
        expr match {
          //TODO enforce exitInt if at top level
          case Ident(ident) =>
            // retrieves length from memory
            instructions += LDR(registers.head, OffsetByInteger(sp, symbolTable.getOffset(ident)))
            instructions += LDR(registers.head, OffsetByInteger(registers.head))
          case arrayElemExpr: ArrayElemExpr =>
            // puts pointer to the sub array in registers.head
            loadArrayElemExprAddress(arrayElemExpr, symbolTable, instructions, registers, assemblyState)
            // dereferences pointer to get the length of the sub array
            instructions += LDR(registers.head, OffsetByInteger(registers.head))
          case _ =>
        }
      case Expr.Ord(expr) =>
        //TODO enforce exitInt if at top level
        convertExprToAsm(expr, symbolTable, instructions, registers, assemblyState = assemblyState)

      case Expr.Chr(expr) =>
        convertExprToAsm(expr, symbolTable, instructions, registers, assemblyState = assemblyState)
    }
  }

  def convertBinOp(binOp: BinOp, symbolTable: VarSymbolTable,
                   instructions: ListBuffer[Instr], registers: List[Register], assemblyState: AssemblyState): Unit = {
    if (registers.length == 2) {
      convertExprToAsm(binOp.expr1, symbolTable, instructions, registers, assemblyState = assemblyState)
      instructions += SUB(sp, sp, ImmediateAddress(WORD_BYTES))
      symbolTable.totalNumBytes += WORD_BYTES
      instructions += STR(registers.head, OffsetByInteger(sp))
      convertExprToAsm(binOp.expr2, symbolTable, instructions, registers, assemblyState = assemblyState)
      instructions += MOV(registers(1), RegAsOperand(registers.head))
      instructions += LDR(registers.head, OffsetByInteger(sp))
      instructions += ADD(sp, sp, ImmediateAddress(WORD_BYTES))
      symbolTable.totalNumBytes -= WORD_BYTES
    }
    else {
      convertExprToAsm(binOp.expr1, symbolTable, instructions, registers, assemblyState = assemblyState)
      convertExprToAsm(binOp.expr2, symbolTable, instructions, registers.tail, assemblyState = assemblyState)
    }
    val fst = registers.head
    val snd = registers.tail.head
    binOp match {
      case Expr.Multiply(_, _) =>
        instructions += SMULL(rdlo = fst, rdhi = snd, rm = fst, rs = snd)
        instructions += CMP(snd, ASR(fst, ImmediateAddress(INT_NUM_BITS - 1)))
        OverflowError.addProcedure(Some(instructions), assemblyState, cond = NE)

      case Expr.Divide(_, _) =>
        instructions += MOV(r0, RegAsOperand(fst))
        instructions += MOV(r1, RegAsOperand(snd))

        DivideByZeroError.addProcedure(Some(instructions), assemblyState)

        instructions += BL(DIV)
        instructions += MOV(fst, RegAsOperand(r0))

      case Expr.Mod(_, _) =>
        instructions += MOV(r0, RegAsOperand(fst))
        instructions += MOV(r1, RegAsOperand(snd))

        DivideByZeroError.addProcedure(Some(instructions), assemblyState)

        instructions += BL(DIVMOD)
        instructions += MOV(fst, RegAsOperand(r1))

      case Expr.Add(_, _) =>
        instructions += ADD(fst, fst, RegAsOperand(snd), S = SetCond)
        OverflowError.addProcedure(Some(instructions), assemblyState, cond = VS)

      case Expr.Subtract(_, _) =>
        instructions += SUB(fst, fst, RegAsOperand(snd), S = SetCond)
        OverflowError.addProcedure(Some(instructions), assemblyState, cond = VS)

      case Expr.GT(_, _) => generateComparisonExpr(GT)
      case Expr.GTE(_, _) => generateComparisonExpr(GE)
      case Expr.LT(_, _) => generateComparisonExpr(LT)
      case Expr.LTE(_, _) => generateComparisonExpr(LE)
      case Expr.Equals(_, _) => generateComparisonExpr(EQ)
      case Expr.NotEquals(_, _) => generateComparisonExpr(NE)
      case Expr.And(_, _) => instructions += AND(fst, fst, RegAsOperand(snd))
      case Expr.Or(_, _) => instructions += ORR(fst, fst, RegAsOperand(snd))
    }

    def generateComparisonExpr(cond: Cond): Unit = {
      instructions += CMP(fst, RegAsOperand(snd))
      instructions += MOV(fst, ImmediateAddress(1), cond = cond)
      instructions += MOV(fst, ImmediateAddress(0), cond = opposite(cond))
    }
  }

}
