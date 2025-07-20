package edu.uta.spl

abstract class TypeChecker {
  var trace_typecheck = false
  var st = new SymbolTable


  def expandType(tp: Type): Type
  def typecheck(e: Expr): Type
  def typecheck(e: Lvalue): Type
  def typecheck(e: Stmt, expected_type: Type)
  def typecheck(e: Definition)
  def typecheck(e: Program)
}


class TypeCheck extends TypeChecker {

  def error(msg: String): Type = {
    System.err.println("*** Typechecking Error: " + msg)
    System.err.println("*** Symbol Table: " + st)
    System.exit(1)
    null
  }


  def expandType(tp: Type): Type =
    tp match {
      case NamedType(nm) =>
        st.lookup(nm) match {
          case Some(TypeDeclaration(t)) => expandType(t)
          case _ => error("Undeclared type: " + tp)
        }
      case _ => tp
    }


  def typeEquivalence(tp1: Type, tp2: Type): Boolean =
    if (tp1 == tp2 || tp1.isInstanceOf[AnyType] || tp2.isInstanceOf[AnyType])
      true
    else expandType(tp1) match {
      case ArrayType(t1) =>
        expandType(tp2) match {
          case ArrayType(t2) => typeEquivalence(t1, t2)
          case _ => false
        }
      case RecordType(fs1) =>
        expandType(tp2) match {
          case RecordType(fs2) =>

            fs1.length == fs2.length &&
              (fs1 zip fs2).map { case (Bind(v1, t1), Bind(v2, t2)) =>
                v1 == v2 && typeEquivalence(t1, t2)
              }.reduce(_ && _)
          case _ => false
        }
      case TupleType(ts1) =>
        expandType(tp2) match {
          case TupleType(ts2) =>

            ts1.length == ts2.length &&
              (ts1 zip ts2).map { case (t1, t2) => typeEquivalence(t1, t2) }.reduce(_ && _)
          case _ => false
        }
      case _ =>
        tp2 match {
          case NamedType(n) => typeEquivalence(tp1, expandType(tp2))
          case _ => false
        }
    }


  var level: Int = -1


  def trace[T](e: Any, result: => T): T = {
    if (trace_typecheck) {
      level += 1
      println(" " * (3 * level) + "** " + e)
    }
    val res = result
    if (trace_typecheck) {
      print(" " * (3 * level))
      if (e.isInstanceOf[Stmt] || e.isInstanceOf[Definition])
        println("->")
      else println("-> " + res)
      level -= 1
    }
    res
  }


  def typecheck(e: Expr): Type =
    trace(e, e match {
      case IntConst(_) => IntType()
      case FloatConst(_) => FloatType()
      case StringConst(_) => StringType()
      case BooleanConst(_) => BooleanType()
      case NullExp() => AnyType()

      // L-value expressions (variables, array elements, etc.)
      case LvalExp(lval) => typecheck(lval)

      // Unary operations
      case UnOpExp(op, operand) =>
        val tp = typecheck(operand)
        op match {
          case "-" | "minus" =>
            if (!typeEquivalence(tp, IntType()) && !typeEquivalence(tp, FloatType()))
              error("Unary - requires int or float operand: " + e)
            tp
          case "not" =>
            if (!typeEquivalence(tp, BooleanType()))
              error("NOT requires boolean operand: " + e)
            BooleanType()
          case _ => error("Unknown unary operator: " + op)
        }

      // Binary operations
      case BinOpExp(op, l, r) =>
        val ltp = typecheck(l)
        val rtp = typecheck(r)

        if (!typeEquivalence(ltp, rtp))
          error("Incompatible types in binary operation: " + e)
        op match {
          case "and" | "or" =>
            // Logical operations
            if (!typeEquivalence(ltp, BooleanType()))
              error("AND/OR operation can only be applied to booleans: " + e)
            BooleanType()
          case "eq" | "neq" =>
            // Equality comparison
            BooleanType()
          case "gt" | "lt" | "geq" | "leq" =>
            // Comparison operations
            if (!typeEquivalence(ltp, IntType()) && !typeEquivalence(ltp, FloatType()))
              error("Comparison requires int or float operands: " + e)
            BooleanType()
          case "plus" | "minus" | "times" | "/" | "%" =>
            // Arithmetic operations
            if (!typeEquivalence(ltp, IntType()) && !typeEquivalence(ltp, FloatType()))
              error("Binary arithmetic operations can only be applied to integer or real numbers: " + e)
            ltp
          case _ => error("Unknown binary operator: " + op)
        }

      // Function calls
      case CallExp(name, args) =>
        st.lookup(name) match {
          case Some(FuncDeclaration(outtype, params, _, _, _)) =>
            if (args.length != params.length)
              error("Argument count mismatch in call to " + name + ": expected " + params.length + ", got " + args.length)
            (args zip params).foreach { case (arg, Bind(_, ptype)) =>
              val atype = typecheck(arg)
              if (!typeEquivalence(atype, ptype))
                error("Type mismatch in argument to " + name + ": expected " + ptype + ", got " + atype)
            }
            outtype
          case _ => error("Undefined function: " + name)
        }

      // Record construction
      case RecordExp(components) =>
        val ctypes = components.map { case Bind(n, exp) => Bind(n, typecheck(exp)) }
        RecordType(ctypes)

      // Array construction
      case ArrayExp(elements) =>
        if (elements.isEmpty) error("ArrayExp must have at least one element: " + e)
        val etype = typecheck(elements.head)

        elements.tail.foreach { elem =>
          val actualType = typecheck(elem)
          if (!typeEquivalence(etype, actualType))
            error("The type of " + elem + " in the array is " + actualType + ", but it was expected to be " + etype)
        }
        ArrayType(etype)

      // Array generation
      case ArrayGen(length, value) =>
        if (!typeEquivalence(typecheck(length), IntType()))
          error("The array length must be integer: " + e)
        ArrayType(typecheck(value))

      // Tuple construction
      case TupleExp(elements) =>
        if (elements.length < 2) error("Tuple must have at least 2 elements: " + e)
        TupleType(elements.map(typecheck))

      case _ => throw new Error("Wrong expression: " + e)
    })

  def typecheck(e: Lvalue): Type =
    trace(e, e match {
      // Variable reference
      case Var(name) =>
        st.lookup(name) match {
          case Some(VarDeclaration(t, _, _)) => t
          case Some(_) => error(name + " is not a variable")
          case None => error("Undefined variable: " + name)
        }

      // Array element access
      case ArrayDeref(array, index) =>
        val itype = typecheck(index)  // Check index first
        if (!typeEquivalence(itype, IntType()))
          error("Array index must be an integer: " + e)
        val atype = typecheck(array)
        expandType(atype) match {
          case ArrayType(etype) => etype
          case _ => error("Array dereference requires an array type: " + e)
        }

      // Record field access
      case RecordDeref(record, attribute) =>
        val rtype = typecheck(record)
        expandType(rtype) match {
          case RecordType(fields) =>
            fields.find(_.name == attribute) match {
              case Some(Bind(_, t)) => t
              case None => error("Unknown field " + attribute + " in record: " + e)
            }
          case _ => error("Record dereference requires a record type: " + e)
        }

      // Tuple element access
      case TupleDeref(tuple, index) =>
        val ttype = typecheck(tuple)
        expandType(ttype) match {
          case TupleType(types) =>
            if (index < 0 || index >= types.length)
              error("Tuple index " + index + " out of bounds: " + e)
            types(index)
          case _ => error("Tuple dereference requires a tuple type: " + e)
        }
    })


  def typecheck(e: Stmt, expected_type: Type) {
    trace(e, e match {
      // Assignment statement
      case AssignSt(d, s) =>
        val dtype = typecheck(d)  // Type of the left side
        val stype = typecheck(s)  // Type of the right side

        if (!typeEquivalence(dtype, stype))
          error("Incompatible types in assignment: expected " + dtype + ", got " + stype)

      // Procedure call statement
      case CallSt(name, args) =>
        st.lookup(name) match {
          case Some(FuncDeclaration(outtype, params, _, _, _)) =>

            if (!typeEquivalence(outtype, NoType()))
              error("Procedure call " + name + " used as statement must not return a value")

            if (args.length != params.length)
              error("Argument count mismatch in call to " + name + ": expected " + params.length + ", got " + args.length)

            (args zip params).foreach { case (arg, Bind(_, ptype)) =>
              val atype = typecheck(arg)
              if (!typeEquivalence(atype, ptype))
                error("Type mismatch in argument to " + name + ": expected " + ptype + ", got " + atype)
            }
          case _ => error("Undefined procedure: " + name)
        }

      // Read statement
      case ReadSt(args) =>
        args.foreach { lval =>
          val ltype = typecheck(lval)
          if (!typeEquivalence(ltype, IntType()) && !typeEquivalence(ltype, FloatType()))
            error("Read argument must be int or float: " + lval)
        }

      // Print statement
      case PrintSt(args) =>
        args.foreach { exp =>
          val etype = typecheck(exp)
          // Print can handle int, float, string, or boolean
          if (!typeEquivalence(etype, IntType()) && !typeEquivalence(etype, FloatType()) &&
            !typeEquivalence(etype, StringType()) && !typeEquivalence(etype, BooleanType()))
            error("Print argument must be int, float, string, or boolean: " + exp)
        }

      // If statement
      case IfSt(condition, then, els) =>
        val condType = typecheck(condition)
        if (!typeEquivalence(condType, BooleanType()))
          error("If condition must be boolean: " + e)
        val thenResult = typecheck(then, expected_type)
        val elsResult = if (els != null) typecheck(els, expected_type) else ()

      // While statement
      case WhileSt(condition, body) =>
        if (!typeEquivalence(typecheck(condition), BooleanType()))
          error("While condition must be boolean: " + e)
        typecheck(body, expected_type)

      // Loop statement (infinite loop)
      case LoopSt(body) =>
        typecheck(body, expected_type)

      // For statement
      case ForSt(variable, initial, step, increment, body) =>
        if (!typeEquivalence(typecheck(initial), IntType()))
          error("For initial value must be int: " + e)
        if (!typeEquivalence(typecheck(step), IntType()))
          error("For step value must be int: " + e)
        if (!typeEquivalence(typecheck(increment), IntType()))
          error("For increment must be int: " + e)

        st.begin_scope()
        st.insert(variable, VarDeclaration(IntType(), 0, 0))
        typecheck(body, expected_type)
        st.end_scope()

      // Exit statement (breaks out of loops)
      case ExitSt() =>

      // Return statement with a value
      case ReturnValueSt(value) =>
        val vtype = typecheck(value)
        if (!typeEquivalence(vtype, expected_type))
          error("Return value type " + vtype + " does not match expected type " + expected_type)

      // Return statement without a value
      case ReturnSt() =>
        if (!typeEquivalence(expected_type, NoType()))
          error("Return without value in function expecting " + expected_type)

      // Block statement
      case BlockSt(decls, stmts) =>
        st.begin_scope()
        decls.foreach(typecheck)
        stmts.foreach(typecheck(_, expected_type))
        st.end_scope()

      case _ => throw new Error("Wrong statement: " + e)
    })
  }

   //Type checks a definition (type, variable, or function).
  def typecheck(e: Definition) {
    trace(e, e match {
      // Type definition
      case TypeDef(name, isType) =>
        st.insert(name, TypeDeclaration(isType))

      // Variable definition
      case VarDef(name, hasType, value) =>
        val vtype = typecheck(value)
        val dtype = hasType match {
          case AnyType() | NoType() => vtype
          case t =>
            if (!typeEquivalence(t, vtype))
              error("Variable " + name + " declared type " + t + " does not match initializer type " + vtype)
            t  // Use the declared type
        }
        st.insert(name, VarDeclaration(dtype, 0, 0))

      // Function definition
      case FuncDef(f, ps, ot, b) =>
        st.insert(f, FuncDeclaration(ot, ps, "", 0, 0))
        st.begin_scope()
        ps.foreach { case Bind(v, tp) => st.insert(v, VarDeclaration(tp, 0, 0)) }
        typecheck(b, ot)
        st.end_scope()
    })
  }

  //Type checks the main program.
  def typecheck(e: Program) {
    typecheck(e.body, NoType())
  }
}