/****************************************************************************************************
 *
 * File: Code.scala
 * The IR code generator for SPL programs
 *
 ****************************************************************************************************/
package edu.uta.spl

abstract class CodeGenerator ( tc: TypeChecker )  {
  def typechecker: TypeChecker = tc
  def st: SymbolTable = tc.st
  def code ( e: Program ): IRstmt
  def allocate_variable ( name: String, var_type: Type, fname: String ): IRexp
}

class Code ( tc: TypeChecker ) extends CodeGenerator(tc) {

  var name_counter = 0

  /** generate a new name */
  def new_name ( name: String ): String = {
    name_counter += 1
    name + "_" + name_counter
  }

  /** IR code to be added at the end of program */
  var addedCode: List[IRstmt] = Nil

  def addCode ( code: IRstmt* ) {
    addedCode ++= code
  }

  /** allocate a new variable at the end of the current frame and return the access code */
  def allocate_variable ( name: String, var_type: Type, fname: String ): IRexp =
    st.lookup(fname) match {
      case Some(FuncDeclaration(rtp,params,label,level,min_offset))
      => // allocate variable at the next available offset in frame
        st.insert(name,VarDeclaration(var_type,level,min_offset))
        // the next available offset in frame is 4 bytes below
        st.replace(fname,FuncDeclaration(rtp,params,label,level,min_offset-4))
        // return the code that accesses the variable
        Mem(Binop("PLUS",Reg("fp"),IntValue(min_offset)))
      case _ => throw new Error("No current function: " + fname)
    }

  /** access a frame-allocated variable from the run-time stack */
  def access_variable ( name: String, level: Int ): IRexp =
    st.lookup(name) match {
      case Some(VarDeclaration(_,var_level,offset))
      => var res: IRexp = Reg("fp")
        // non-local variable: follow the static link (level-var_level) times
        for ( i <- var_level+1 to level )
          res = Mem(Binop("PLUS",res,IntValue(-8)))
        Mem(Binop("PLUS",res,IntValue(offset)))
      case _ => throw new Error("Undefined variable: " + name)
    }

  /** return the IR code from the Expr e (level is the current function nesting level,
   *  fname is the name of the current function/procedure) */
  def code ( e: Expr, level: Int, fname: String ): IRexp =
    e match {
      // Binary operations
      case BinOpExp(op,left,right)
      => val cl = code(left,level,fname)
        val cr = code(right,level,fname)
        val nop = op.toUpperCase() match {
          case "+" => "PLUS"
          case "-" => "MINUS"
          case "*" => "TIMES"
          case "/" => "DIV"
          case "%" => "MOD"
          case "==" => "EQ"
          case "!=" => "NEQ"
          case "<" => "LT"
          case "<=" => "LEQ"
          case ">" => "GT"
          case ">=" => "GEQ"
          case "&&" => "AND"
          case "||" => "OR"
          case other => other
        }
        Binop(nop,cl,cr)

      // Array generation: array(len, value)
      case ArrayGen(len,v)
      => val A = allocate_variable(new_name("A"),typechecker.typecheck(e),fname)
        val L = allocate_variable(new_name("L"),IntType(),fname)
        val V = allocate_variable(new_name("V"),typechecker.typecheck(v),fname)
        val I = allocate_variable(new_name("I"),IntType(),fname)
        val loop = new_name("loop")
        val exit = new_name("exit")
        ESeq(Seq(List(Move(L,code(len,level,fname)),   // store length in L
          Move(A,Allocate(Binop("PLUS",L,IntValue(1)))),
          Move(V,code(v,level,fname)),     // store value in V
          Move(Mem(A),L),                  // store length in A[0]
          Move(I,IntValue(0)),
          Label(loop),                     // for-loop
          CJump(Binop("GEQ",I,L),exit),
          Move(Mem(Binop("PLUS",A,Binop("TIMES",Binop("PLUS",I,IntValue(1)),IntValue(4)))),V),
          Move(I,Binop("PLUS",I,IntValue(1))),
          Jump(loop),
          Label(exit))),
          A)

      // Array literals:
      case ArrayExp(elements)
      => val array_var = allocate_variable(new_name("array"), ArrayType(IntType()), fname)
        var stmts: List[IRstmt] = List(
          Move(array_var, Allocate(IntValue(elements.length + 1))),
          Move(Mem(array_var), IntValue(elements.length))
        )
        for (i <- elements.indices) {
          stmts = stmts :+ Move(
            Mem(Binop("PLUS", array_var, IntValue((i + 1) * 4))),
            code(elements(i), level, fname)
          )
        }
        ESeq(Seq(stmts), array_var)

      // Integer literals
      case IntConst(value)
      => IntValue(value)

      // Float literals
      case FloatConst(value)
      => FloatValue(value)

      // String literals
      case StringConst(value)
      => StringValue(value)

      // Boolean literals
      case BooleanConst(value)
      => IntValue(if (value) 1 else 0)

      // Lvalue as expression
      case LvalExp(lval)
      => code(lval, level, fname)

      // Unary operations
      case UnOpExp(op, operand) =>
        val co = code(operand, level, fname)
        op.toUpperCase() match {
          case "-" => Binop("MINUS", IntValue(0), co)
          case "!" => Unop("NOT", co)
          case other => Unop(other, co)
        }

      // Function calls
      case CallExp(name, args)
      => st.lookup(name) match {
        case Some(FuncDeclaration(_, params, label, func_level, _))
        => val static_link = if (func_level == level + 1)
          Reg("fp")
        else {
          var sl: IRexp = Reg("fp")
          for (_ <- 1 to level - func_level + 1)
            sl = Mem(Binop("PLUS", sl, IntValue(-8)))
          sl
        }
          val arg_exprs = args.map(arg => code(arg, level, fname))
          Call(label, static_link, arg_exprs)
        case _ => throw new Error("Undefined function: " + name)
      }

      // Record construction
      case RecordExp(fields)
      => val record_type = typechecker.typecheck(e)
        val record_var = allocate_variable(new_name("R"), record_type, fname)
        var stmts: List[IRstmt] = List(
          Move(record_var, Allocate(IntValue(fields.length)))
        )
        typechecker.expandType(record_type) match {
          case RecordType(field_types) =>
            val field_positions = field_types.map(_.name).zipWithIndex.toMap
            for (field <- fields) {
              val pos = field_positions.getOrElse(field.name,
                throw new Error(s"Unknown field ${field.name} in record"))
              stmts = stmts :+ Move(
                Mem(Binop("PLUS", record_var, IntValue(pos * 4))),
                code(field.value, level, fname)
              )
            }
          case _ => throw new Error("Not a record type: " + record_type)
        }
        ESeq(Seq(stmts), record_var)

      // Tuple construction
      case TupleExp(exprs)
      => val tuple_var = allocate_variable(new_name("T"), typechecker.typecheck(e), fname)
        val expr_codes = exprs.map(expr => code(expr, level, fname))
        var stmts: List[IRstmt] = List(
          Move(tuple_var, Allocate(IntValue(exprs.length)))
        )
        for (i <- exprs.indices) {
          stmts = stmts :+ Move(
            Mem(Binop("PLUS", tuple_var, IntValue(i * 4))),
            expr_codes(i)
          )
        }
        ESeq(Seq(stmts), tuple_var)

      // Null pointer
      case NullExp()
      => IntValue(0)

      case _ => throw new Error("Wrong expression: "+e)
    }

  /** return the IR code from the Lvalue e (level is the current function nesting level,
   *  fname is the name of the current function/procedure) */
  def code ( e: Lvalue, level: Int, fname: String ): IRexp =
    e match {
      // Record field access:
      case RecordDeref(r,a)
      => val cr = code(r,level,fname)
        typechecker.expandType(typechecker.typecheck(r)) match {
          case RecordType(cl)
          => val i = cl.map(_.name).indexOf(a)
            Mem(Binop("PLUS",cr,IntValue(i*4)))
          case _ => throw new Error("Unknown record: "+e)
        }

      // Variable access
      case Var(name)
      => access_variable(name, level)

      // Array indexing:
      case ArrayDeref(array, index)
      => val array_expr = code(array, level, fname)
        val index_expr = code(index, level, fname)
        Mem(Binop("PLUS", array_expr,
          Binop("TIMES", Binop("PLUS", index_expr, IntValue(1)), IntValue(4))))

      // Tuple component access:
      case TupleDeref(tuple, i)
      => val tuple_expr = code(tuple, level, fname)
        Mem(Binop("PLUS", tuple_expr, IntValue(i * 4)))

      case _ => throw new Error("Wrong lvalue: " + e)
    }

  /** return the IR code from the Statement e (level is the current function nesting level,
   *  fname is the name of the current function/procedure)
   *  and exit_label is the exit label */
  def code ( e: Stmt, level: Int, fname: String, exit_label: String ): IRstmt =
    e match {
      // For loop:
      case ForSt(v,a,b,c,s)
      => val loop = new_name("loop")
        val exit = new_name("exit")
        val cv = allocate_variable(v,IntType(),fname)
        val ca = code(a,level,fname)
        val cb = code(b,level,fname)
        val cc = code(c,level,fname)
        val cs = code(s,level,fname,exit)
        Seq(List(Move(cv,ca),
          Label(loop),
          CJump(Binop("GT",cv,cb),exit),
          cs,
          Move(cv,Binop("PLUS",cv,cc)),
          Jump(loop),
          Label(exit)))

      // Assignment statement:
      case AssignSt(lhs, rhs)
      => val dest = code(lhs, level, fname)
        val src = code(rhs, level, fname)
        Move(dest, src)

      // Procedure call
      case CallSt(name, args)
      => st.lookup(name) match {
        case Some(FuncDeclaration(_, _, label, func_level, _))
        => val static_link = if (func_level == level + 1)
          Reg("fp")
        else {
          var sl: IRexp = Reg("fp")
          for (_ <- 1 to level - func_level + 1)
            sl = Mem(Binop("PLUS", sl, IntValue(-8)))
          sl
        }
          val arg_exprs = args.map(arg => code(arg, level, fname))
          CallP(label, static_link, arg_exprs)
        case _ => throw new Error("Undefined procedure: " + name)
      }

      // Read statement:
      case ReadSt(lhsList)
      => val readStatements = lhsList.map(lhs => {
        val var_type = typechecker.typecheck(lhs)
        val sys_op = var_type match {
          case IntType() => "READ_INT"
          case FloatType() => "READ_FLOAT"
          case _ => "READ_INT"
        }
        SystemCall(sys_op, code(lhs, level, fname))
      })
        Seq(readStatements)

      // Write statement:
      case PrintSt(args)
      => var new_line_syscall = List(SystemCall("WRITE_STRING", StringValue("\\n")))
        var arg_syscall = args.map(item => {
          val item_type = typechecker.typecheck(item)
          val sys_op = item_type match {
            case IntType() => "WRITE_INT"
            case FloatType() => "WRITE_FLOAT"
            case BooleanType() => "WRITE_BOOL"
            case StringType() => "WRITE_STRING"
            case _ => "WRITE_INT"
          }
          SystemCall(sys_op, code(item, level, fname))
        })
        Seq(arg_syscall ::: new_line_syscall)

      // If statement:
      case IfSt(cond, thenStmt, elseStmt) =>
        val cont_label = new_name("cont")
        val exit_label = new_name("exit")
        val cond_code = code(cond, level, fname)
        val then_code = code(thenStmt, level, fname, exit_label)
        val else_code = if (elseStmt == null) Seq(List()) else code(elseStmt, level, fname, exit_label)
        Seq(List(
          CJump(cond_code, exit_label),
          else_code,
          Jump(cont_label),
          Label(exit_label),
          then_code,
          Label(cont_label)))

      // While statement:
      case WhileSt(cond, body)
      => val loop_label = new_name("loop")
        val exit_label = new_name("exit")
        val cond_code = code(cond, level, fname)
        val body_code = code(body, level, fname, exit_label)
        Seq(List(
          Label(loop_label),
          CJump(Unop("NOT", cond_code), exit_label),
          body_code,
          Jump(loop_label),
          Label(exit_label)))

      // Loop statement:
      case LoopSt(body)
      => val loop_label = new_name("loop")
        val exit_label2 = new_name("exit")
        val body_code = code(body, level, fname, exit_label2)
        Seq(List(
          Label(loop_label),
          body_code,
          Jump(loop_label),
          Label(exit_label2)))

      // Exit statement:
      case ExitSt()
      => Jump(exit_label)

      // Return with value:
      case ReturnValueSt(expr)
      => val expr_code = code(expr, level, fname)
        Seq(List(
          Move(Reg("a0"), expr_code),
          Move(Reg("ra"), Mem(Binop("PLUS", Reg("fp"), IntValue(-4)))),
          Move(Reg("sp"), Reg("fp")),
          Move(Reg("fp"), Mem(Reg("fp"))),
          Return()))

      // Return without value
      case ReturnSt()
      => Seq(List(
        Move(Reg("ra"), Mem(Binop("PLUS", Reg("fp"), IntValue(-4)))),
        Move(Reg("sp"), Reg("fp")),
        Move(Reg("fp"), Mem(Reg("fp"))),
        Return()))

      // Block statement:
      case BlockSt(defs, stmts)
      => st.begin_scope()
        val def_code = defs.map(d => code(d, fname, level))
        val stmt_code = stmts.map(s => code(s, level, fname, exit_label))
        st.end_scope()
        Seq(def_code ++ stmt_code)

      case _ => throw new Error("Wrong statement: " + e)
    }

  /** return the IR code for the declaration block of function fname
   * (level is the current function nesting level) */
  def code ( e: Definition, fname: String, level: Int ): IRstmt =
    e match {
      // Function definition
      case FuncDef(f,ps,ot,b)
      => val flabel = if (f == "main") f else new_name(f)
        st.insert(f,FuncDeclaration(ot,ps,flabel,level+1,-12))
        st.begin_scope()
        ps.zipWithIndex.foreach{ case (Bind(v,tp),i)
        => st.insert(v,VarDeclaration(tp,level+1,(ps.length-i)*4)) }
        val body = code(b,level+1,f,"")
        st.end_scope()
        st.lookup(f) match {
          case Some(FuncDeclaration(_,_,_,_,offset))
          => addCode(Label(flabel),
            Move(Mem(Reg("sp")),Reg("fp")),
            Move(Reg("fp"),Reg("sp")),
            Move(Mem(Binop("PLUS",Reg("fp"),IntValue(-4))),Reg("ra")),
            Move(Mem(Binop("PLUS",Reg("fp"),IntValue(-8))),Reg("v0")),
            Move(Reg("sp"),Binop("PLUS",Reg("sp"),IntValue(offset))),
            body,
            Move(Reg("ra"),Mem(Binop("PLUS",Reg("fp"),IntValue(-4)))),
            Move(Reg("sp"),Reg("fp")),
            Move(Reg("fp"),Mem(Reg("fp"))),
            Return())
            Seq(List())
          case _ => throw new Error("Unknown function: "+f)
        }

      // Variable declaration:
      case VarDef(name, tp, null)
      => val var_mem = allocate_variable(name, tp, fname)
        Move(var_mem, IntValue(0))

      // Variable declaration with initialization:
      case VarDef(name, tp, expr) if expr != null
      => val actual_type = if (tp == AnyType()) typechecker.typecheck(expr) else tp
        val var_mem = allocate_variable(name, actual_type, fname)
        val expr_code = code(expr, level, fname)
        Move(var_mem, expr_code)

      // Type definition:
      case TypeDef(name, tp)
      => st.insert(name, TypeDeclaration(tp))
        Seq(List())

      case _ => throw new Error("Wrong statement: " + e)
    }

  def code ( e: Program ): IRstmt =
    e match {
      case Program(b@BlockSt(_,_))
      => st.begin_scope()
        val res = code(FuncDef("main",List(),NoType(),b),"",0)
        st.end_scope()
        Seq(res::addedCode)
      case _ => throw new Error("Wrong program "+e)
    }
}