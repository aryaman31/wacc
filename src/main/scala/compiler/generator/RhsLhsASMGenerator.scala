package compiler.generator

import compiler.generator.ExprASMGenerator.convertExprToAsm
import compiler.generator.procedure.BuiltInProcedure._
import compiler.generator.utils.AssemblyState
import compiler.generator.utils.GeneratorUtils.{BYTE_SIZE, INT_BYTES, WORD_BYTES, getNumBytes}
import compiler.generator.utils.InstrUtils._
import compiler.generator.utils.OperandUtils.Register.{Register, r0, r1, sp}
import compiler.generator.utils.OperandUtils._
import compiler.parser.ast.expr.Expr.{ArrayElemExpr, Ident}
import compiler.parser.ast.expr.PairElem.{Fst, Snd}
import compiler.parser.ast.expr.{Expr, PairElem}
import compiler.parser.ast.stats.AssignRHS
import compiler.parser.ast.stats.AssignRHS.{ArrayLiter, Call, NewPair}
import compiler.semantics.SymbolTable.VarSymbol
import compiler.semantics.VarSymbolTable

import scala.collection.mutable.ListBuffer

object RhsLhsASMGenerator {
  // Loads the address of a given AssignLHS into the head of registers
  def loadLHSAddress(lhs: Expr.AssignLHS, symbolTable: VarSymbolTable,
                     instructions: ListBuffer[Instr], registers: List[Register], assemblyState: AssemblyState): Unit = {
    lhs match {
      case Ident(ident) =>
        instructions += ADD(registers.head, sp, ImmediateAddress(symbolTable.getOffset(ident)))

      case pairElem: PairElem =>
        // Returns pair reference as registers.head
        convertExprToAsm(pairElem.expr, symbolTable, instructions, registers, assemblyState = assemblyState)

        instructions += MOV(rd = r0, op = RegAsOperand(registers.head))
        NullReferenceError.addProcedure(Some(instructions), assemblyState)

        // Dereferences the pair (fst or snd) to get the actual pointer
        pairElem match {
          case Fst(_) =>
            // Load first element address
            instructions += LDR(rd = registers.head, address = OffsetByInteger(registers.head))
          case Snd(_) =>
            // Load second element address
            instructions += LDR(rd = registers.head, address = OffsetByInteger(registers.head, WORD_BYTES))
        }

      case arrayElemExpr: ArrayElemExpr =>
        loadArrayElemExprAddress(arrayElemExpr, symbolTable, instructions, registers, assemblyState)
    }
  }

  // returns pointer from malloc in registers.head, or else just the value of the expression in registers.head
  def convertRHStoAsm(rhs: AssignRHS, symbolTable: VarSymbolTable, instructions: ListBuffer[Instr],
                      registers: List[Register], symbol: Option[VarSymbol] = None, assemblyState: AssemblyState): Unit = {
    rhs match {
      case expr: Expr =>
        convertExprToAsm(expr, symbolTable, instructions, registers, assemblyState = assemblyState)

      // Malloc array of appropriate size (i.e. arraylength * sizeofArrayElem + INT_BYTES)
      case arrayLiter: ArrayLiter =>
        // Updates array identifiers length if applicable
        val len = arrayLiter.exprList.length

        // Loads amount of space to be allocated for the array into r0 as a parameter to malloc
        // This is equal to arrayLiter.exprList.length * sizeofArrayElem + INT_BYTES
        var elemSize = 0
        if (len != 0) {
          // Gets and loads the memory required for the array elements into r0
          elemSize = getNumBytes(arrayLiter.exprList.head, symbolTable)
          instructions += LDR(r0, ImmediateMemory(elemSize * len + INT_BYTES))
        }
        else {
          // Increments r0 by INT_BYTES to allow space for the length parameter at the start of the array
          instructions += LDR(r0, ImmediateMemory(INT_BYTES))
        }
        instructions += BL("malloc")
        // Moves pointer into register used for returns in this function (registers.head)
        instructions += MOV(registers.head, RegAsOperand(r0))
        // Loads length into register and puts it into first position in array memory
        instructions += LDR(registers(1), ImmediateMemory(len))
        instructions += STR(registers(1), OffsetByInteger(registers.head))
        var i = 0

        // STR the array literal into memory at the malloced address
        for (expr <- arrayLiter.exprList) {
          // Loads result of evaluating expression into next available register
          convertExprToAsm(expr, symbolTable, instructions, registers.tail, assemblyState = assemblyState)
          instructions += STR(registers(1), OffsetByInteger(registers.head, i * elemSize + INT_BYTES))
          i += 1
        }

      // Malloc space for pointer to pair, and for the pointers to first and second
      case newPair: NewPair =>
        // Loads the amount of space to be allocated into r0 as a parameter to malloc
        // This is equal to WORD_BYTES * 2 as we need one pointer for each element in the pair

        instructions += LDR(r0, ImmediateMemory(2 * WORD_BYTES))
        instructions += BL("malloc")
        // Loads pointer to allocated memory into head of register list to prepare r0 for another malloc call
        instructions += MOV(registers.head, RegAsOperand(r0))

        // Allocates pair elements and loads the pointers to their positions into the allocated pointer positions
        val fstAddress = OffsetByInteger(registers.head)
        val sndAddress = OffsetByInteger(registers.head, WORD_BYTES)
        allocatePairElem(newPair.expr1, symbolTable, instructions, registers.tail, fstAddress, assemblyState)
        allocatePairElem(newPair.expr2, symbolTable, instructions, registers.tail, sndAddress, assemblyState)


      // Calls function specified by call, putting the return value into registers.head
      case funcCall: Call =>
        // Add parameters
        var totalSize = 0
        for (expr <- funcCall.args.reverse) {
          // Convert each parameter
          convertExprToAsm(expr, symbolTable, instructions, registers, assemblyState = assemblyState)
          // Shift pointer
          val size = getNumBytes(expr, symbolTable)
          totalSize += size
          instructions += STR(registers.head, OffsetByInteger(sp, -size, writeBack = true), ByteOp = size == BYTE_SIZE)
          symbolTable.funcOffset += size
        }

        // Call function
        instructions += BL(s"f_${funcCall.func.ident}")
        // Shift bytes used for params
        instructions += ADD(sp, sp, ImmediateAddress(totalSize))
        symbolTable.funcOffset -= totalSize
        // Move returned value into head of registers
        instructions += MOV(registers.head, RegAsOperand(r0))
    }
  }

  def allocatePairElem(expr: Expr, symbolTable: VarSymbolTable, instructions: ListBuffer[Instr],
                       registers: List[Register], targetAddress: Address, assemblyState: AssemblyState): Unit = {
    // Loads value of pair elem into next available register
    convertExprToAsm(expr, symbolTable, instructions, registers, assemblyState = assemblyState)
    // Loads the amount of space to be allocated to the pair elem into r0
    val size = getNumBytes(expr, symbolTable)
    instructions += LDR(r0, ImmediateMemory(size))
    instructions += BL("malloc")
    // Stores first element into the newly assigned location in memory
    instructions += STR(registers.head, OffsetByInteger(r0), ByteOp = BYTE_SIZE == size)
    // Loads pointer to first element into assigned location in memory from previous malloc
    // This address is passed into this function (targetAddress)
    instructions += STR(r0, targetAddress)
  }

  // Loads the address of an arrayElemExpr into the head of available registers
  def loadArrayElemExprAddress(arrayElemExpr: ArrayElemExpr, symbolTable: VarSymbolTable,
                               instructions: ListBuffer[Instr], registers: List[Register],
                               assemblyState: AssemblyState): Unit = {
    instructions += ADD(registers.head, sp, ImmediateAddress(symbolTable.getOffset(arrayElemExpr.arrName.ident)))
    val dimension = symbolTable.getIdentifierSymbol(arrayElemExpr.arrName.ident).get.getDimension
    var i = 1
    for (expr <- arrayElemExpr.exprList) {
      // Puts expression value into registers(1)
      convertExprToAsm(expr, symbolTable, instructions, registers.tail, assemblyState = assemblyState)
      // Loads pointer to length into registers.head
      instructions += LDR(registers.head, OffsetByInteger(registers.head))
      // Put length into r0 and expr value into r1, for procedure call
      instructions += MOV(r0, RegAsOperand(registers(1)))
      instructions += MOV(r1, RegAsOperand(registers.head))
      // Call built in index check
      IndexOutOfBoundsCheck.addProcedure(Some(instructions), assemblyState)
      // Get the element at the given index
      instructions += ADD(registers.head, registers.head, ImmediateAddress(WORD_BYTES))

      // Array is just a pointer so size is word size
      var size = WORD_BYTES
      if (i == dimension) {
        // Then the expression is not of type array so get the actual size
        size = getNumBytes(arrayElemExpr, symbolTable)
      }
      // Puts size of elements into registers(2)
      instructions += LDR(registers(2), ImmediateMemory(size))
      instructions += MLA(registers.head, registers(1), registers(2), registers.head)
      i += 1
    }
  }
}
