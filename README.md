# SPL Compiler 

This project implements a five-stage compiler for the Small Programming Language (SPL) using Scala, JFlex, and CUP. The compiler gradually builds from a scanner to a complete IR code generator, following the SPL specification and test programs.

---

## 📁 Project Structure

```
spl/
├── src/main/scala/edu/uta/spl/
│   ├── spl.lex               # Scanner (Assignment 1)
│   ├── spl.cup               # Parser + AST Actions (Assignments 2 & 3)
│   ├── AST.scala             # Abstract Syntax Tree structures
│   ├── TypeCheck.scala       # Type checking logic (Assignment 4)
│   ├── Code.scala            # IR Code generation (Assignment 5)
│   └── SPL.scala             # Main compiler entrypoint
├── tests/                    # Test SPL programs
├── errors_test/              # Programs with intentional syntax/semantic errors
├── spl-solution.jar          # Reference solution for comparison
├── My_Output.txt             # Output from this compiler
├── Solution_Output.txt       # Output from the reference compiler
├── pom.xml                   # Maven build file
```

---

## 🔧 Build Instructions

```bash
mvn clean install
```

To test against a SPL file:

```bash
scala lib/spl.jar [stage] tests/hello.spl
```

Where `[stage]` is:
- `1` for Scanner
- `2` for Parser
- `3` for AST
- `4` for Type Checker
- `5` for IR Code Generator

Compare with solution:

```bash
scala spl-solution.jar [stage] tests/hello.spl
```

---

## 🚀 Project Progression

### Assignment 1 – Scanner with JFlex

- Implemented lexical analyzer in `spl.lex`
- Recognized tokens defined in `spl.cup`
- Used JFlex to scan `.spl` files and output token streams
- Verified correctness against `tests/*.spl` using stage `1`

```bash
scala lib/spl.jar 1 tests/*.spl
```

---

### Assignment 2 – Parser with CUP

- Implemented grammar rules in `spl.cup`
- Covered all constructs from the SPL manual
- Removed semantic actions and types for this stage
- Verified syntax-only parsing using:

```bash
scala lib/spl.jar 2 tests/*.spl
```

---

### Assignment 3 – AST Construction

- Re-introduced semantic actions to `spl.cup`
- Built AST nodes using constructors in `AST.scala`
- Ensured each SPL program resulted in a valid AST tree
- Verified against solution’s AST output using:

```bash
scala lib/spl.jar 3 tests/*.spl
```

---

### Assignment 4 – Type Checker

- Implemented semantic checks in `TypeCheck.scala`
- Used `SymbolTable` and type inference to verify types
- Detected undeclared variables, mismatched types, and illegal operations
- Output matched reference type checker:

```bash
scala lib/spl.jar 4 tests/*.spl
```

---

### Assignment 5 – Intermediate Code Generation

- Implemented IR translation logic in `Code.scala`
- Generated IR trees using classes from `IR.scala`
- Supported code for control structures, expressions, records, tuples, arrays, and procedures
- Verified correctness using:

```bash
scala lib/spl.jar 5 tests/*.spl
```

---

## ✅ Testing & Validation

- Used `tests/` directory for valid SPL programs
- Used `errors_test/` for syntactic/semantic error detection
- Compared `My_Output.txt` with `Solution_Output.txt` for all phases
- Cleaned build files using:

```bash
mvn clean
```

---

## 👨‍💻 Author

Rency Kansagra 
University of Texas at Arlington  


