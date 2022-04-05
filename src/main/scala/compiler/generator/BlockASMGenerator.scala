package compiler.generator

import compiler.generator.ExprASMGenerator.convertExprToAsm
import compiler.generator.ProcedureGenerator._
import compiler.generator.RhsLhsASMGenerator.{convertRHStoAsm, loadLHSAddress}
import compiler.generator.procedure.BuiltInProcedure._
import compiler.generator.utils.AssemblyState
import compiler.generator.utils.GeneratorUtils._
import compiler.generator.utils.InstrUtils.Cond.{EQ, NE}
import compiler.generator.utils.InstrUtils._
import compiler.generator.utils.OperandUtils.Register.{Register, r0, sp}
import compiler.generator.utils.OperandUtils.{ImmediateAddress, OffsetByInteger, RegAsOperand}
import compiler.parser.ast.expr.Expr
import compiler.parser.ast.expr.Expr.Ident
import compiler.parser.ast.stats.AssignRHS.Call
import compiler.parser.ast.stats.Stat
import compiler.parser.ast.stats.Stat.Block
import compiler.parser.ast.types.Type.BaseType.{CharType, IntType}
import compiler.parser.ast.types.Type.PairType
import compiler.semantics.VarSymbolTable

import scala.collection.mutable.ListBuffer

object BlockASMGenerator {

  // Converts a block of statements to assembly
  def convertBlockToAsm(block: Block, symbolTable: VarSymbolTable, instructions: ListBuffer[Instr],
                        registers: List[Register], inNest: Boolean = false, isFunc: Boolean = false,
                        assemblyState: AssemblyState): BlockExitStatus = {
    var statements = block.statements
    // Process each of the statements
    for (stat <- block.statements) {
      statements = statements.tail
      val tempBlock = Block(statements, block.symbolTable)
      val result =
        convertStatToAsm(stat, symbolTable, instructions, registers, tempBlock, inNest, isFunc, assemblyState)
      // If status is RETURNED or BRANCHED then we know the whole block has been processed (either because
      // the block has a return or because the rest of the block has been handled by a recursive call)
      result match {
        case BRANCHED | RETURNED => return result
        case NORMAL => // Don't return
      }
    }
    NORMAL
  }

  // In the case where a block has a return we don't process
  // any statements after it and we also have to pop pc
  def convertStatToAsm(stat: Stat, symbolTable: VarSymbolTable, instructions: ListBuffer[Instr],
                       registers: List[Register], remainingBlock: Block, inNest: Boolean = false, isFunc: Boolean = false,
                       assemblyState: AssemblyState): BlockExitStatus = {

    stat match {
      case Stat.Skip => // Do nothing

      case Stat.VarInit(varType, name, rhs) =>
        symbolTable.setOffset(name.ident)
        val symbol = symbolTable.getIdentifierSymbol(name.ident).get
        val size = getBytes(varType)
        // we malloc and set the memory to the RHS, before then copying that reference/value
        // to the identifiers reference/value
        convertRHStoAsm(rhs, symbolTable, instructions, registers, Some(symbol), assemblyState)
        instructions += STR(registers.head, OffsetByInteger(sp, symbolTable.getOffset(name.ident)),
          ByteOp = size == BYTE_SIZE)

      case Stat.VarAssign(lhs, rhs) =>
        // We malloc and set the memory to the RHS, before then copying that reference to the LHS reference
        lhs match {
          // As length of arrays can change for idents, we pass the symbol in so that we can update if needed
          case Ident(ident) =>
            val symbol = symbolTable.getIdentifierSymbol(ident).get
            convertRHStoAsm(rhs, symbolTable, instructions, registers, Some(symbol), assemblyState)
          case _ =>
            convertRHStoAsm(rhs, symbolTable, instructions, registers, assemblyState = assemblyState)
        }
        val size = rhs match {
          case expr: Expr => getNumBytes(expr, symbolTable)
          case Call(name, _) => getBytes(assemblyState.funcSymbolTable.getIdentifierSymbol(name.ident).get.returnType)
          case _ => WORD_BYTES
        }
        loadLHSAddress(lhs, symbolTable, instructions, registers.tail, assemblyState)
        instructions += STR(rd = registers.head, address = OffsetByInteger(registers(1)), ByteOp = size == BYTE_SIZE)

      case Stat.Read(readVal) =>
        loadLHSAddress(readVal, symbolTable, instructions, registers, assemblyState)
        instructions += MOV(r0, RegAsOperand(registers.head))
        getExprType(readVal, symbolTable) match {
          case IntType =>
            ReadInt.addProcedure(Some(instructions), assemblyState)
          case CharType =>
            ReadChar.addProcedure(Some(instructions), assemblyState)
        }

      case Stat.Free(expr) =>
        // loads value of expression into registers.head
        convertExprToAsm(expr, symbolTable, instructions, registers, assemblyState = assemblyState)
        instructions += MOV(r0, RegAsOperand(registers.head))
        getExprType(expr, symbolTable) match {
          case PairType(_, _) =>
            NullReferenceError.addProcedure(Some(instructions), assemblyState)
            instructions += PUSH(List(r0))
            // free first pointer
            instructions += LDR(r0, OffsetByInteger(r0))
            instructions += BL("free")
            // free second pointer
            instructions += LDR(r0, OffsetByInteger(sp))
            instructions += LDR(r0, OffsetByInteger(r0, WORD_BYTES))
            instructions += BL("free")
            instructions += POP(List(r0))
          case _ =>
        }
        // frees actual pointer
        instructions += BL("free")


      case Stat.Return(expr) =>
        convertExprToAsm(expr, symbolTable, instructions, registers, assemblyState = assemblyState)
        instructions += MOV(r0, RegAsOperand(registers.head))


        if (inNest) {
          var totalNumBytes = 0
          var currSymbolTable: Option[VarSymbolTable] = Some(symbolTable)
          while (currSymbolTable.isDefined) {
            totalNumBytes += currSymbolTable.get.totalNumBytes
            currSymbolTable = currSymbolTable.get.parent
          }
          instructions += ADD(sp, sp, ImmediateAddress(totalNumBytes))
        }

        return RETURNED

      case Stat.Exit(expr) =>
        convertExprToAsm(expr, symbolTable, instructions, registers, assemblyState = assemblyState)
        instructions += MOV(r0, RegAsOperand(registers.head))
        // We don't need to round the exit status to be within 0-255
        // since the c exit function handles that for us
        instructions += BL("exit")

      case Stat.Print(expr) =>
        convertExprToAsm(expr, symbolTable, instructions, registers, assemblyState = assemblyState)
        addPrintInstr(instructions, expr, registers, symbolTable, assemblyState)

      case Stat.Println(expr) =>
        convertExprToAsm(expr, symbolTable, instructions, registers, assemblyState = assemblyState)
        addPrintInstr(instructions, expr, registers, symbolTable, assemblyState)
        PrintLn.addProcedure(Some(instructions), assemblyState)

      case Stat.IfElse(condition, ifBlock, elseBlock) =>
        // puts value of boolean expression condition into registers.head
        convertExprToAsm(condition, symbolTable, instructions, registers, assemblyState = assemblyState)
        // checks if boolean is false
        instructions += CMP(registers.head, ImmediateAddress(0))
        // creates labels for new branches
        val elseLabel = assemblyState.getBranchLabel
        val remainingLabel = assemblyState.getBranchLabel
        // BEQ to else clause, evaluating elseBlock
        instructions += B(elseLabel, cond = EQ)
        // translate the if block as the remaining section of the current procedure
        instructions += convertToProcedure("", ifBlock, NewScopeBranch, inNest = true, isFunc = isFunc, assemblyState = assemblyState)

        // add a branch statement to the end section
        instructions += B(remainingLabel)
        // otherwise evaluate elseBlock in its own procedure
        instructions += convertToProcedure(elseLabel, elseBlock, NewScopeBranch, inNest = true, isFunc = isFunc, assemblyState = assemblyState)
        var procTy: ProcedureType = EndProcedure
        if (isFunc) {
          procTy = EndFuncProcedure
        }
        if (inNest) {
          procTy = NestedBranch
        }
        instructions += convertToProcedure(remainingLabel, remainingBlock, procTy, inNest = inNest, isFunc = isFunc, assemblyState = assemblyState)
        return BRANCHED

      case Stat.While(condition, block) =>


        val whileLabel = assemblyState.getBranchLabel
        val remainingLabel = assemblyState.getBranchLabel
        val condBranchInstr: ListBuffer[Instr] = ListBuffer()

        instructions += B(remainingLabel)
        convertExprToAsm(condition, symbolTable, condBranchInstr, registers, assemblyState = assemblyState)
        condBranchInstr += CMP(rn = registers.head, op = ImmediateAddress(0))
        condBranchInstr += B(cond = NE, label = whileLabel)

        block.symbolTable.get.getNumBytesUsed
        instructions += convertToProcedure(whileLabel, block, NewScopeBranch, inNest = true, isFunc = isFunc, assemblyState = assemblyState)
        var procTy: ProcedureType = EndProcedure
        if (isFunc) {
          procTy = EndFuncProcedure
        }
        if (inNest) {
          procTy = NestedBranch
        }
        instructions += convertToProcedure(remainingLabel, remainingBlock, procTy, condBranchInstr, inNest = inNest, isFunc = isFunc, assemblyState = assemblyState)

        return BRANCHED

      case Stat.BeginEnd(block) =>
        instructions += convertToProcedure("", block, NewScopeBranch, inNest = inNest, isFunc = isFunc, assemblyState = assemblyState)
    }
    NORMAL
  }
}
