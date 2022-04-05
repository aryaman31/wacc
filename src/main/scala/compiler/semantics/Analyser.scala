package compiler.semantics

import compiler.ErrorUtils._
import compiler.parser.ast.Program
import compiler.parser.ast.expr.Expr._
import compiler.parser.ast.expr.PairElem.{Fst, Snd}
import compiler.parser.ast.expr.{Expr, PairElem}
import compiler.parser.ast.func.Func
import compiler.parser.ast.stats.AssignRHS.{ArrayLiter, Call, NewPair}
import compiler.parser.ast.stats.Stat.{Block, _}
import compiler.parser.ast.stats.{AssignRHS, Stat}
import compiler.parser.ast.types.Type
import compiler.parser.ast.types.Type.BaseType.{BoolType, CharType, IntType}
import compiler.parser.ast.types.Type._
import compiler.semantics.ExprTypesUtil.ExprTypeInference
import compiler.semantics.SymbolTable.{FuncSymbol, VarSymbol}

import scala.collection.mutable

class Analyser {
  val errors: mutable.ListBuffer[String] = mutable.ListBuffer()
  val funcSymbolTable: FuncSymbolTable = FuncSymbolTable()
  val topVarSymbolTable: VarSymbolTable = VarSymbolTable(None)
  var lines: List[String] = Nil

  def checkTypesMatch(type1: Option[Type], type2: Option[Type]): Boolean = {
    // We use None to represent any Type
    // This can be either because the Type is actually any Type or because
    // there was an undefined reference somewhere and we don't want to propagate errors upwards
    if (type1.isEmpty || type2.isEmpty) return true
    (type1.get, type2.get) match {
      case (ArrayType(t1: Type, dim1: Int), ArrayType(t2: Type, dim2: Int)) =>
        checkTypesMatch(Some(t1), Some(t2)) && dim1 == dim2
      case (_: UntypedPair, _: UntypedPair) => true
      case (_: PairType, _: UntypedPair) => true
      case (_: UntypedPair, _: PairType) => true
      case (PairType(t11, t12), PairType(t21, t22)) =>
        checkTypesMatch(Some(t11), Some(t21)) && checkTypesMatch(Some(t12), Some(t22))
      case _ => type1.get == type2.get
    }
  }

  def analyseProgram(program: Program, linesString: String): Option[String] = {
    lines = "" +: List.from(linesString.split("\n"))
    program.funcDefs.foreach(
      func => if (!funcSymbolTable.addIdentifierSymbol(func)) {
        val lineNo = func.funcSig.name.pos._1
        addError(lineNo, IdentErrorLine(func.funcSig.name.ident, lines(lineNo), 2))
      })

    // Add funcSymbolTable to the AST
    program.funcSymbolTable = Some(funcSymbolTable)
    program.funcDefs.foreach(func => analyseFuncStat(func))
    analyseBlock(program.block, topVarSymbolTable, isFunc = false)

    if (errors.isEmpty) {
      return None
    }
    Some(errors.toList.reduce((a,b) => a ++ "\n" ++ b))
  }

  private def analyseFuncStat(func: Func): Unit = {
    val table: VarSymbolTable = VarSymbolTable(Some(topVarSymbolTable), retType = Some(func.funcSig.returnType))
    func.funcSig.params.foreach(table.addIdentifierSymbol)
    analyseBlock(func.block, table, isFunc = true)
  }

  private def analyseBlock(block: Block, table: VarSymbolTable, isFunc: Boolean): Unit = {
    // Add symbolTable to the AST
    block.symbolTable = Some(table)
    block.statements.foreach(analyseStat(_, table, isFunc))
  }

  // adds a given semantic error with information stored in the SemanticErrorLine to the list of errors to be printed
  private def addError(lineNo: Int, line: SemanticErrorLine): Unit = {
    errors += getErrorString(lineNo, line)
  }

  // checks that statements are semantically correct
  private def analyseStat(stat: Stat, table: VarSymbolTable, isFunc: Boolean): Unit = {
    stat match {
      case varInit: VarInit =>
        // checks if the identifier is already in the symbol table
        if (!table.addIdentifierSymbol(varInit)) {
          val lineNo = varInit.name.pos._1
          addError(lineNo, IdentErrorLine(varInit.name.ident, lines(lineNo), errType = 2))
        }
        val rhsType = analyseRhs(varInit.rhs, table)
        // checks if the type of the rhs matches that of the lhs
        if (!checkTypesMatch(rhsType, Some(varInit.varType))) {
          val lineNo = varInit.name.pos._1
          addError(lineNo, TypeErrorLine(varInit.varType, rhsType.get, lines(lineNo), 2))
        }
      case VarAssign(lhs, rhs) =>
        val lhsType = analyseLhs(lhs, table)
        val rhsType = analyseRhs(rhs, table)
        if (!checkTypesMatch(rhsType, lhsType)) {
          val lineNo = rhs.pos._1
          addError(lineNo, TypeErrorLine(lhsType.get, rhsType.get, lines(lineNo), 2))
        }
      case Read(readVal) =>
        val lhsType = analyseLhs(readVal, table)
        // checks that the readVal is either an Int or a Char
        if (!checkTypesMatch(lhsType, Some(IntType)) && !checkTypesMatch(lhsType, Some(CharType))) {
          val lineNo = readVal.pos._1
          addError(lineNo,
            MessageErrorLine("Attempted to read type: " +
                              lhsType.get.toString + "\n Can only read Int or Char", lines(lineNo)))
        }
      case Free(expr) =>
        val exprType = analyseExpr(expr, table)
        // checks if the expr is an array or a pair
        exprType match {
          case Some(_: ArrayType) =>
          case Some(_: PairType) =>
          case Some(_: UntypedPair) =>
          case _ =>
            val lineNo = expr.pos._1
            addError(lineNo,
            MessageErrorLine("Attempted to free type: " +
            exprType.get.toString + "\n Can only free Pair or Array", lines(lineNo)))
        }
      case Return(expr) =>
        if (!isFunc) {
          val lineNo = expr.pos._1
         addError(lineNo, MessageErrorLine("Cannot have return statement outside function", lines(lineNo)))
        }
        else {
          val exprType = analyseExpr(expr, table)
          // checks the returned value matches the return type of the function
          if (!checkTypesMatch(exprType, table.retType)) {
            val lineNo = expr.pos._1
            addError(lineNo, TypeErrorLine(table.retType.get, exprType.get, lines(lineNo), 3))
          }
        }
      case Exit(expr) =>
        val exprType = analyseExpr(expr, table)
        // checks the exit code is of type Int
        if (!checkTypesMatch(exprType, Some(IntType))) {
          val lineNo = expr.pos._1
          addError(lineNo, TypeErrorLine(IntType, exprType.get, lines(lineNo), 4))
        }
      case Print(expr) =>
        analyseExpr(expr, table)
      case Println(expr) =>
        analyseExpr(expr, table)

      case IfElse(condition, ifBlock, elseBlock) =>
        val condType = analyseExpr(condition, table)
        // checks the condition is a Boolean
        if (!checkTypesMatch(condType, Some(BoolType))) {
          val lineNo = condition.pos._1
          addError(lineNo, TypeErrorLine(BoolType, condType.get, lines(lineNo), 5))
        }
        analyseBlock(ifBlock, VarSymbolTable(Some(table), retType = table.retType), isFunc)
        analyseBlock(elseBlock, VarSymbolTable(Some(table), retType = table.retType), isFunc)
      case While(condition, block) =>
        val condType = analyseExpr(condition, table)
        // checks the condition is a Boolean
        if (!checkTypesMatch(condType, Some(BoolType))) {
          val lineNo = condition.pos._1
          addError(lineNo, TypeErrorLine(BoolType, condType.get, lines(lineNo), 5))
        }
        analyseBlock(block, VarSymbolTable(Some(table), retType = table.retType), isFunc)
      case BeginEnd(block) =>
        analyseBlock(block, VarSymbolTable(Some(table), retType = table.retType), isFunc)
      case _ =>
    }
  }

  // checks unary operator expressions are semantically correct
  def handleUnaryOpExpr(unaryOp: UnaryOp, table: VarSymbolTable): Type = {
    val exprType = analyseExpr(unaryOp.expr, table)
    // checks the input to the unary expression is of the correct type
    if (!checkTypesMatch(exprType, unaryOp.inputType)) {
      val lineNo = unaryOp.pos._1
      addError(lineNo, TypeErrorLine(unaryOp.inputType.get, exprType.get, lines(lineNo), 7))
    }
    unaryOp.outputType.get
  }

  // checks binary operator expressions are semantically correct
  def handleBinaryOpExpr(binOp: BinOp, table: VarSymbolTable): Type = {
   binOp match {
     case _: Equals | _: NotEquals =>
       val expr1Type = analyseExpr(binOp.expr1, table)
       val expr2Type = analyseExpr(binOp.expr2, table)
        if (!checkTypesMatch(expr1Type, expr2Type)) {
          val lineNo = binOp.pos._1
          addError(lineNo, TypeErrorLine(expr1Type.get, expr2Type.get, lines(lineNo), 8))
        }
        return binOp.outputType.get
     case _: GT | _: GTE | _: LT | _: LTE =>
       val expr1Type = analyseExpr(binOp.expr1, table)
       val expr2Type = analyseExpr(binOp.expr2, table)
       // checks the expressions are both of type char or type int
       if (!checkTypesMatch(expr1Type, expr2Type)) {
         val lineNo = binOp.expr1.pos._1
         addError(lineNo, TypeErrorLine(expr1Type.get, expr2Type.get, lines(lineNo), 9))
       }
       if (!checkTypesMatch(expr1Type, Some(IntType)) &&
       !checkTypesMatch(expr1Type, Some(CharType))) {
         val lineNo = binOp.expr1.pos._1
         addError(lineNo, TypeErrorLine(expr1Type.get, expr2Type.get, lines(lineNo), 10))
       }
       if (!checkTypesMatch(expr2Type, Some(IntType)) &&
         !checkTypesMatch(expr2Type, Some(CharType))) {
         val lineNo = binOp.expr2.pos._1
         addError(lineNo, TypeErrorLine(expr1Type.get, expr2Type.get, lines(lineNo), 10))
       }
       return binOp.outputType.get
     case _ =>
    }
    val expr1Type = analyseExpr(binOp.expr1, table)
    val expr2Type = analyseExpr(binOp.expr2, table)
    // checks the inputs to the binary expression are of the correct type
    if (!checkTypesMatch(expr1Type, binOp.inputType)) {
      val lineNo = binOp.expr1.pos._1
      addError(lineNo, TypeErrorLine(binOp.inputType.get, expr1Type.get, lines(lineNo), 11))
    }
    if (!checkTypesMatch(expr2Type, binOp.inputType)) {
      val lineNo = binOp.expr2.pos._1
      addError(lineNo, TypeErrorLine(binOp.inputType.get, expr2Type.get, lines(lineNo), 12))
    }
    binOp.outputType.get
  }

  // checks that expressions are semantically correct, returns None if the expression was incorrectly formed
  // or can take any type
  private def analyseExpr(expr: Expr, table: VarSymbolTable): Option[Type] = {
    expr match {
      case unaryOp: UnaryOp => Some(handleUnaryOpExpr(unaryOp, table))
      case binaryOp: BinOp => Some(handleBinaryOpExpr(binaryOp, table))
      case _: PairLiter => Some(NullPair)

      case ArrayElemExpr(name, exprList) =>
        val nameType = table.getIdentifierSymbol(name.ident)
        val varType = nameType match {
          case None =>
            val lineNo = name.pos._1
            addError(lineNo, IdentErrorLine(name.ident, lines(lineNo), 1))
            return None
          case Some(VarSymbol(ArrayType(inner: Type, dim), _, _)) =>
            dim match {
              case 1 => inner
              case _ => ArrayType(inner, dim - 1)
            }
          case _ =>
            val lineNo = name.pos._1
            addError(lineNo, IdentErrorLine(name.ident, lines(lineNo), 1))
            return None
        }

        for (expr <- exprList) {
          val exprType = analyseExpr(expr, table)
          if (exprType.isDefined && exprType.get != IntType) {
            val lineNo = name.pos._1
            addError(lineNo, TypeErrorLine(IntType, exprType.get, lines(lineNo) , 6))
            return None
          }
        }
        Some(varType)

      case id@Ident(value) =>
        val symbol = table.getIdentifierSymbol(value)
        symbol match {
          case None =>
            val lineNo = id.pos._1
            addError(lineNo, IdentErrorLine(value, lines(lineNo), 1))
            None
          case Some(VarSymbol(varType , _, _)) => Some(varType)
        }

      case _ =>
        expr.outputType
    }
  }

  private def analyseRhs(rhs: AssignRHS, table: VarSymbolTable): Option[Type] = {
    rhs match {
      case ArrayLiter(exprList) =>
        if (exprList.isEmpty) {
          None
        } else {
          val validExprTypes = exprList.map(analyseExpr(_, table)).filter(_.isDefined)
          for (exprType <- validExprTypes) {
            if (!checkTypesMatch(exprType, validExprTypes.head)) {
              val lineNo = rhs.pos._1
              addError(lineNo, TypeErrorLine(validExprTypes.head.get, exprType.get, lines(lineNo), 13))
              return None
            } // Invalid list so don't propagate errors
          }
          if (validExprTypes.length != exprList.length) {
            return None // Invalid list so don't propagate errors
            // Note - in this case we have already added the error so no need to add another
            // (means we have an undefined ident reference)

          }
          if (validExprTypes.head.isEmpty) return None
          Some(validExprTypes.head.get match {
            case ArrayType(t, dim) => ArrayType(t, dim + 1)
            case t => ArrayType(t, 1)
          })
        }

      case NewPair(expr1, expr2) =>
        val expr1Type = analyseExpr(expr1, table)
        val expr2Type = analyseExpr(expr2, table)
        // checks that a newpair assignment only takes expressions of the right types
        if (expr1Type.isEmpty || expr2Type.isEmpty) return None
        (expr1Type, expr2Type) match {
          case (None, _) => None
          case (_, None) => None
          case (Some(fstType: PairElemType), Some(sndType: PairElemType)) => Some(PairType(fstType, sndType))
        }

      case pairElem: PairElem => checkPair(pairElem, table)

      case Call(func, args) =>
        funcSymbolTable.getIdentifierSymbol(func.ident) match {
          case Some(symbol: FuncSymbol) =>
            val paramTypes = symbol.paramTypes
            val low = List(args.length, paramTypes.length).min

            // forms a list of argTypes from all well formed expressions in the args list for the purpose
            // of informative error messages
            val argTypes = args.map(a => analyseExpr(a, table)).filter(a=>a.isDefined).map(a=>a.get)

            // checks whether the number of arguments and number of parameters are the same
            if (args.length != paramTypes.length) {
              val lineNo = func.pos._1
              addError(lineNo, FuncErrorLine(argTypes, paramTypes, lines(lineNo)))
              return Some(symbol.returnType)
            }

            // checks if any argtype does not match the respective paramtype
            for (i <- 0 until  low) {
              val exprType = analyseExpr(args(i), table)
              if (!checkTypesMatch(exprType, Some(paramTypes(i)))) {
                val lineNo = args(i).pos._1
                addError(lineNo, FuncErrorLine(argTypes, paramTypes, lines(lineNo)))
                return Some(symbol.returnType)
              }
            }

            Some(symbol.returnType)
          case _ =>
            val lineNo = func.pos._1
            addError(lineNo, IdentErrorLine(func.ident, lines(lineNo), 3))
            None
        }

      case expr: Expr =>
        analyseExpr(expr, table)
    }
  }

  private def analyseLhs(lhs: AssignLHS, table: VarSymbolTable): Option[Type] = {
    lhs match {
      case ArrayElemExpr(name, exprList) =>
        val nameType = table.getIdentifierSymbol(name.ident)
        val varType = nameType match {
          case None =>
            val lineNo = name.pos._1
            addError(lineNo, IdentErrorLine(name.ident, lines(lineNo), 1))
            return None
          case Some(VarSymbol(ArrayType(inner: Type, dim), _, _)) =>
            Some(dim match {
              case 1 => inner
              case _ => ArrayType(inner, dim)
            })
          case Some(_: VarSymbol) =>
            val lineNo = name.pos._1
            addError(lineNo, IdentErrorLine(name.ident, lines(lineNo), 4))
            None
        }

        for (expr <- exprList) {
          val exprType = analyseExpr(expr, table)
          if (!checkTypesMatch(exprType, Some(IntType))) {
            val lineNo = expr.pos._1
            addError(lineNo, TypeErrorLine(IntType, exprType.get, lines(lineNo) , 6))
            return None
          }
        }
        varType

      case pairElem: PairElem => checkPair(pairElem, table)
      case id@Ident(value) =>
        // searches for the identifier in the symbol table, if not found throws an error
        val symbol = table.getIdentifierSymbol(value)
        symbol match {
          case None =>
            val lineNo = id.pos._1
            addError(lineNo, IdentErrorLine(value, lines(lineNo), 1))
            None
          case Some(v: VarSymbol) => Some(v.varType)
        }
    }
  }

  // Abstracted out checking of pairElems, as LHS and RHS do the same checks on the pairElems
  // Checks whether fst/snd are called on null pairs or non pair variables, throwing an error and returning None
  // if they are
  private def checkPair(pairElem: PairElem, table: VarSymbolTable): Option[Type] = {
    val exprType = analyseExpr(pairElem.expr, table)
    exprType match {
      case None => None
      case Some(NullPair) =>
        val lineNo = pairElem.expr.pos._1
        addError(lineNo, MessageErrorLine(
          "fst/snd called on null pair", lines(lineNo)))
        None
      case Some(PairType(t1: PairElemType, t2: PairElemType)) =>
        pairElem match {
          case _: Fst => Some(t1)
          case _: Snd => Some(t2)
        }
      case Some(_: UntypedPair) => None
      case _ =>
        val lineNo = pairElem.expr.pos._1
        addError(lineNo, MessageErrorLine(
          "fst/snd called on non pair variable, Actual: " + exprType.get.toString, lines(lineNo)))
        None
    }
  }
}
