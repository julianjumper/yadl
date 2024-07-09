import parser.*
import ArithmaticOps.{Add, Div, Expo, Mod, Mul, Sub}
import BooleanOps.{And, Not, Or}
import CompareOps.{Eq, Greater, GreaterEq, Less, LessEq, NotEq}

val builtins = stdlib.stdlib

type HashMap[K, V] = scala.collection.mutable.HashMap[K, V]
type MutArray[V] = scala.collection.mutable.ArrayBuffer[V]

class Scope(
    parent: Scope = null,
    funArgs: Seq[String] = Seq(),
    callArgs: Seq[Value] = Seq()
):
  private var parentScope: Scope = parent
  private var _result: Value = null
  private var localVars: HashMap[String, Value] =
    new HashMap().addAll(funArgs.zip(callArgs))
  private var localFuncs: HashMap[String, parser.Function] = new HashMap

  def result: Option[Value] =
    if (this._result == null) None
    else
      val tmp = this._result
      this._result = null
      Some(tmp)

  def hasResult: Boolean = this._result != null
  def isGlobal: Boolean = this.parentScope == null

  def returnValue(value: Value): Scope =
    this._result = value
    this

  def lookup(identifier: Identifier): Option[Value] =
    this.localVars
      .get(identifier.name)
      .filter {
        case id: Identifier => id.name != identifier.name
        case _              => true
      }
      .orElse(this.lookupInParent(identifier))
      .orElse(this.lookupFunction(identifier))

  def lookupFunction(identifier: Identifier): Option[parser.Function] =
    this.localFuncs.get(identifier.name) match {
      case value: Some[parser.Function] => value
      case None                         => lookupFunctionInParent(identifier)
    }

  private def lookupInParent(identifier: Identifier): Option[Value] =
    if (this.parentScope != null)
      this.parentScope.lookup(identifier)
    else None

  private def lookupFunctionInParent(
      identifier: Identifier
  ): Option[parser.Function] =
    if (this.parentScope != null)
      this.parentScope.lookupFunction(identifier)
    else
      None

  def update(identifier: Identifier, value: Value): Scope =
    value match {
      case f: Function =>
        this.lookupFunction(identifier) match {
          case None =>
            this.localFuncs.update(identifier.name, f)
          case Some(func: Function) =>
            val tmp =
              Function(f.args, captureExternals(this, f.args, f.body))
            this.localFuncs.update(identifier.name, f)
        }
      case v: Value =>
        this.localVars.update(identifier.name, v)
    }
    this
end Scope

def captureExternalsValue(
    v: Value,
    locals: MutArray[String],
    scope: Scope
): Value =
  v match {
    case Identifier(name) =>
      if (!locals.contains(name))
        val vl = scope.lookup(Identifier(name)) match {
          case Some(v) => v
          case None    => assert(false, s"'$name' does not exist")
        }
        val Some(value) = evalValue(vl, scope).result: @unchecked
        value
      else Identifier(name)
    case Wrapped(value) => captureExternalsValue(value, locals, scope)
    case FunctionCall(functionExpr, args) =>
      val fx = captureExternalsValue(functionExpr, locals, scope)
      val fas = args.map(captureExternalsValue(_, locals, scope))
      FunctionCall(fx, fas)
    case Function(args, body) =>
      val ls = locals.clone().appendAll(args)
      val b = body.map(captureExternalsStatement(_, locals, scope))
      Function(args, b)
    case ArrayLiteral(elements) =>
      val tmp = elements.map(captureExternalsValue(_, locals, scope))
      ArrayLiteral(tmp)
    case BinaryOp(l, op, r) =>
      val nl = captureExternalsValue(l, locals, scope)
      val nr = captureExternalsValue(r, locals, scope)
      BinaryOp(nl, op, nr)
    case UnaryOp(op, vl) =>
      val v = captureExternalsValue(vl, locals, scope)
      UnaryOp(op, v)
    // TODO: how should keys in dictionaries be replaced?
    case value => value
  }

def captureExternalsStatement(
    st: Statement,
    locals: MutArray[String],
    scope: Scope
): Statement =
  st match {
    case Assignment(varName, value) =>
      if (!locals.contains(varName)) {
        val newVal = captureExternalsValue(value, locals, scope)
        locals.addOne(varName)
        Assignment(varName, newVal)
      } else Assignment(varName, value)
    case fc: FunctionCall =>
      val tmp = captureExternalsValue(fc.functionExpr, locals, scope)
      val tmp2 = fc.args.map(x => captureExternalsValue(x, locals, scope))
      FunctionCall(tmp, tmp2)
    case Return(value) =>
      val v = captureExternalsValue(value, locals, scope)
      Return(v)
    case WhileLoop(loop) =>
      val cond = captureExternalsValue(loop.condition, locals, scope)
      val body = loop.body.map(captureExternalsStatement(_, locals, scope))
      WhileLoop(Branch(cond, body))
    case Expression(expr) =>
      val e = captureExternalsValue(expr, locals, scope)
      Expression(e)
    case If(ifBranch, elifBranches, elseBranch) =>
      val ifCond = captureExternalsValue(ifBranch.condition, locals, scope)
      val ifBody =
        ifBranch.body.map(captureExternalsStatement(_, locals, scope))
      val newIf = Branch(ifCond, ifBody)

      val elifBranchs =
        elifBranches.foldLeft(Seq(): Seq[Branch])((acc, br) =>
          val cond = captureExternalsValue(br.condition, locals, scope)
          val b = br.body.map(captureExternalsStatement(_, locals, scope))
          acc.appended(Branch(cond, b))
        )

      val newElseBranch: Option[Seq[Statement]] = elseBranch match {
        case Some(statements) =>
          Some(statements.map(captureExternalsStatement(_, locals, scope)))
        case None =>
          None
      }
      If(newIf, elifBranchs, newElseBranch)
  }

def captureExternals(
    scope: Scope,
    functionArguments: Seq[String],
    functionBody: Seq[Statement]
): Seq[Statement] =
  var tmp: MutArray[String] = new MutArray
  tmp.addOne("print")
  tmp.appendAll(builtins.keys)
  tmp.appendAll(functionArguments)
  functionBody.map(captureExternalsStatement(_, tmp, scope))

enum CallContext:
  case Statement, Value

def evalFunctionCall(
    functionExpr: Value,
    callArgs: Seq[Value],
    scope: Scope,
    context: CallContext
): Scope =
  val evaledCallArgs = callArgs.map(evalValue(_, scope).result.get)
  functionExpr match {
    case Identifier(identifier) =>
      if (identifier == "print") {
        printValues(evaledCallArgs, scope)
        print("\n")
        scope
      } else if (builtins.contains(identifier)) {
        val callArgsNew = evaledCallArgs.map { value =>
          interpreterdata.toDataObject(value)
        }
        val Some(func) = builtins.get(identifier): @unchecked
        assert(
          func.n_args == callArgs.length,
          s"function call: expected ${func.n_args} argument(s) but got ${callArgs.length}"
        )
        val result = func.function(callArgsNew)
        if (context == CallContext.Value)
          scope.returnValue(interpreterdata.toAstNode(result))
        else scope
      } else {
        scope.lookupFunction(Identifier(identifier)) match {
          case Some(Function(args, body)) =>
            // TODO: merge the capture with the current scope
            val res =
              body.foldLeft(Scope(scope, args, evaledCallArgs))(evalStatement)
            res.result match {
              case Some(value) =>
                if (context == CallContext.Value)
                  scope.returnValue(value)
                else scope
              case None =>
                // NOTE: this case is only fine if we evaluate a statement function call
                // We maybe want to check if it is a statement
                scope
            }
          case None =>
            assert(
              false,
              s"function '$identifier' not found"
            )
        }
      }
    case Wrapped(value) =>
      evalFunctionCall(value, evaledCallArgs, scope, context)

    case Function(args, body) =>
      val res =
        body.foldLeft(Scope(scope, args, evaledCallArgs))(evalStatement)
      val Some(value) = res.result: @unchecked
      if (context == CallContext.Value)
        scope.returnValue(value)
      else scope

    case FunctionCall(functionExpr, args) =>
      val Some(Function(args1, body1)) =
        evalFunctionCall(functionExpr, args, scope, context).result: @unchecked
      val res =
        body1.foldLeft(Scope(scope, args1, evaledCallArgs))(evalStatement)
      val Some(value) = res.result: @unchecked
      if (context == CallContext.Value)
        scope.returnValue(value)
      else scope
  }

def evalReturn(value: Value, scope: Scope): Scope =
  if (scope.isGlobal) assert(false, "can not return from global scope")
  else
    value match {
      case id: Identifier =>
        (scope.result, scope.lookup(id)) match {
          case (None, Some(v)) => scope.returnValue(v)
          case (Some(v), _)    => scope.returnValue(v)
          case (None, None) =>
            assert(
              false,
              s"identifier '${id.name}' does not exist that could be returned"
            )
        }

      case f: Function =>
        val tmp =
          Function(f.args, captureExternals(scope, f.args, f.body))
        scope.returnValue(f)
      case va =>
        scope.result match {
          case None =>
            val Some(res) = evalValue(va, scope).result: @unchecked
            scope.returnValue(res)
          case Some(v) => scope.returnValue(v)
        }
    }

def evalStatement(
    scope: Scope,
    st: Statement
): Scope =
  // First we differentiate of what type the current statement is.
  // The types are given by the `statementP` parsing rule in the Parser file.
  // Our goal of this function obviously is to handle all statements. This can either be a definition/re-definition
  // of a variable. Then we want to update the Scope. Or loops, if-else, function calls, return (see statementP rule).
  if (scope.hasResult)
    scope
  else
    st match {
      case Assignment(name, value) =>
        // Assignments have the form variable = Value. See the `valueP` rule of the parser for the types that are assignable.
        // Our goal here is it to evaluate the right-hand side. Eg. x = 4+9, then we want to add the variable x to the
        // Scope with the value 13 as a number.
        val result = evalValue(value, scope)
        // NOTE: `.result` should allways exist. It indicates a bug in `evalValue` if it does not
        // The evalValue-function stores the newly calculated value in the Scope in the variable `result`. We need to
        // update the variable `name` with this new value.
        val Some(newValue) = result.result: @unchecked
        scope.update(Identifier(name), newValue)
      case FunctionCall(identifier, callArgs) =>
        evalFunctionCall(
          identifier,
          callArgs,
          scope,
          CallContext.Statement
        )
      case Return(value) =>
        evalReturn(value, scope)
      case Expression(expr) =>
        val Some(result) = evalValue(expr, scope).result: @unchecked
        scope.returnValue(result)
      case If(ifBranch, elifBranches, elseBranch) => {
        evalIf(ifBranch, elifBranches, elseBranch, scope)
      }
      case WhileLoop(loop) =>
        evalWhileLoop(loop, scope)
    }

def evalWhileLoop(whileLoop: Branch, scope: Scope): Scope = {
  var currentScope = scope
  while (
    evalValue(whileLoop.condition, currentScope).result.contains(Bool(true))
  ) {
    currentScope =
      whileLoop.body.foldLeft(currentScope)((accScope, statement) => {
        if (accScope.hasResult)
          accScope // Early exit if result is set (like return statements)
        else evalStatement(accScope, statement)
      })
  }
  currentScope
}

def evalIf(
    initialBranch: Branch,
    elifBranches: Seq[Branch],
    elseBranch: Option[Seq[Statement]],
    scope: Scope
): Scope = {

  def evalBranch(branch: Branch, scope: Scope): Option[Scope] = {
    val result = evalValue(branch.condition, scope)
    result.result match {
      case Some(Bool(true)) =>
        Some(branch.body.foldLeft(scope)(evalStatement))
      case _ =>
        None
    }
  }

  // Check initial branch condition
  var conditionsMet = false
  evalBranch(initialBranch, scope) match {
    case Some(updatedScope) =>
      updatedScope // Return updated scope if the initial branch is true
    case None =>
      val finalScope = elifBranches.foldLeft(scope) { (currentScope, branch) =>
        evalBranch(branch, currentScope) match {
          case Some(updatedScope) =>
            conditionsMet = true
            updatedScope
          case None => currentScope
        }
      }

      if (conditionsMet) {
        return finalScope
      } else if (elseBranch.isEmpty) {
        return scope
      }

      // Evaluate the else branch if no conditions were met
      elseBranch match {
        case Some(statements) =>
          statements.foldLeft(finalScope)(evalStatement)
        case _ => finalScope
      }
  }
}

def evalValue(
    v: Value,
    scope: Scope
): Scope =
  v match {
    case Function(args, body) =>
      scope.returnValue(
        Function(args, captureExternals(scope, args, body))
      )
    case FunctionCall(identifier, callArgs) =>
      evalFunctionCall(identifier, callArgs, scope, CallContext.Value)
    case BinaryOp(left, op, right) =>
      val left_result = evalValue(left, scope)
      val Some(new_left) = left_result.result: @unchecked
      val right_result = evalValue(right, scope)
      val Some(new_right) = right_result.result: @unchecked
      evalBinaryOp(op, new_left, new_right, scope)
    case UnaryOp(op, value) =>
      val Some(result) = evalValue(value, scope).result: @unchecked
      op match {
        case ArithmaticOp(Add) => scope.returnValue(result)
        case ArithmaticOp(Sub) =>
          result match {
            case n: Number =>
              scope.returnValue(Number(-n.value))
            case v =>
              assert(false, s"unary op: value case '$v' is not implemented")
          }
        case BooleanOp(Not) =>
          val tmp = result match {
            case Bool(value) => Bool(!value)
            case v =>
              assert(false, s"unary operator 'not' is not defined for '$v'")
          }
          scope.returnValue(tmp)
        case o => assert(false, s"unary op: op case '$o' is not implemented")
      }
    case StructureAccess(id, v) =>
      id match {
        case s: StructureAccess =>
          val Some(value) = evalValue(s, scope).result: @unchecked
          evalValue(StructureAccess(value, v), scope)
        case Wrapped(value) =>
          evalValue(StructureAccess(value, v), scope)
        case f: FunctionCall =>
          val Some(value) =
            evalFunctionCall(f.functionExpr, f.args, scope).result: @unchecked
          evalValue(StructureAccess(value, v), scope)
        case id: Identifier =>
          scope.lookup(id) match {
            case Some(d: Dictionary) =>
              evalValue(StructureAccess(d, v), scope)
            case Some(a: ArrayLiteral) =>
              evalValue(StructureAccess(a, v), scope)
            case _ =>
              assert(false, s"no structure found by the name '${id.name}'")
          }
        case Dictionary(entries) =>
          val result: Option[Value] = entries.foldLeft(None) { (acc, curr) =>
            acc match {
              case None =>
                val res = evalValue(curr.key, scope)
                val Some(value) = res.result: @unchecked
                val res2 = evalCompareOps(Eq, value, v, scope)
                res2.result match {
                  case Some(Bool(true)) =>
                    Some(curr.value)
                  case _ => None
                }
              case r => r
            }
          }
          result match {
            case Some(value) => scope.returnValue(value)
            case None =>
              assert(false, s"no value found for the key '$v' in a dictionary")
          }
        case ArrayLiteral(elements) =>
          evalValue(v, scope).result match {
            case None => assert(false, s"Expr \"$v\" is not interpretable")
            case Some(Number(n)) => {
              if (n != n.toInt) {
                throw IllegalArgumentException(
                  "expected hole number, but got number with decimal part"
                )
              }
              val tmp = elements(n.toInt)
              scope.returnValue(tmp)
            }
            case x =>
              throw IllegalArgumentException(
                "expected number, not: " + x.toString
              )
          }
      }
    case Identifier(name) =>
      scope.lookup(Identifier(name)) match {
        case Some(value) =>
          scope.returnValue(value)
        case None =>
          assert(false, s"identifier '$name' does not exist")
      }
    case Number(value) =>
      scope.returnValue(Number(value))
    case StdString(value) =>
      scope.returnValue(StdString(value))
    case Bool(value) =>
      scope.returnValue(Bool(value))
    case Wrapped(value) =>
      evalValue(value, scope)
    case Dictionary(entries) =>
      scope.returnValue(Dictionary(entries))
    case ArrayLiteral(elements) =>
      scope.returnValue(ArrayLiteral(elements))
    case NoneValue() =>
      scope.returnValue(NoneValue())
    case err =>
      assert(false, f"TODO: not implemented '$err'")
  }

def evalBinaryOp(
    op: Operator,
    left: Value,
    right: Value,
    scope: Scope
): Scope = {
  op match
    case ArithmaticOp(ops) => evalArithmeticOps(ops, left, right, scope)
    case CompareOp(ops)    => evalCompareOps(ops, left, right, scope)
    case BooleanOp(ops)    => evalBooleanOps(ops, left, right, scope)
    case _ => throw new IllegalArgumentException("Binary operation invalid.")
}

def evalBooleanOps(
    op: BooleanOps,
    left: Value,
    right: Value,
    scope: Scope
): Scope = {
  // evaluate left and right value
  val Some(leftEval) = evalValue(left, scope).result: @unchecked
  val Some(rightEval) = evalValue(right, scope).result: @unchecked

  // if left or right is string.
  if (leftEval.isInstanceOf[Identifier] || rightEval.isInstanceOf[Identifier]) {
    assert(false, "No boolean operators allowed on strings")
  }

  // otherwise left and right are bools or numbers
  val result = op match {
    case And => (extractNumber(leftEval) > 0) && (extractNumber(rightEval) > 0)
    case Or  => (extractNumber(leftEval) > 0) || (extractNumber(rightEval) > 0)
    case _   => assert(false, "Unexpected comparison operation.")
  }

  scope.returnValue(Bool(result)) // Adding the result to the scope
}

def evalCompareOps(
    op: CompareOps,
    left: Value,
    right: Value,
    scope: Scope
): Scope = {
  // evaluate left and right value
  val Some(leftEval) = evalValue(left, scope).result: @unchecked
  val Some(rightEval) = evalValue(right, scope).result: @unchecked
  // if left or right is string.
  (leftEval, rightEval) match {
    case (s1: StdString, s2: StdString) => {
      op match {
        case Eq =>
          scope.returnValue(Bool(s1.value == s2.value))
        case NotEq =>
          scope.returnValue(Bool(s1.value != s2.value))
        case _ =>
          assert(
            false,
            "For Strings only the operators '==' and '!=' are allowed."
          )
      }
    }
    case (
          value1: (Number | Bool | NoneValue),
          value2: (Number | Bool | NoneValue)
        ) => {
      // otherwise left and right are bools, numbers or none
      val lhs_num = extractNumber(value1)
      val rhs_num = extractNumber(value2)
      val result = op match {
        case Less      => lhs_num < rhs_num
        case LessEq    => lhs_num <= rhs_num
        case Greater   => lhs_num > rhs_num
        case GreaterEq => lhs_num >= rhs_num
        case Eq        => lhs_num == rhs_num
        case NotEq     => lhs_num != rhs_num
      }

      scope.returnValue(Bool(result)) // Adding the result to the scope
    }
    case (v1, v2) =>
      assert(
        false,
        s"the values '$v1' and '$v2' are not comparable under '$op'"
      )
  }
}

def typeOf(value: Value): String =
  value match {
    case _: StdString  => "string"
    case _: Number     => "number"
    case _: Bool       => "bool"
    case _: Dictionary => "dictionary"
    // case _: ArrayLiteral => "array"
    case _: Function => "function"
    case _           => "'not defined'"
  }

def evalArithmeticOps(
    op: ArithmaticOps,
    left: Value,
    right: Value,
    scope: Scope
): Scope = {
  // evaluate left and right value
  val Some(leftEval) = evalValue(left, scope).result: @unchecked
  val Some(rightEval) = evalValue(right, scope).result: @unchecked

  (leftEval, rightEval) match {
    case (s1: StdString, s2: StdString) =>
      op match {
        case Add =>
          val tmp = s1.value + s2.value
          scope.returnValue(StdString(tmp))
        case _ => assert(false, "only operator '+' is defined for strings")
      }
    case (value1: (Number | Bool), value2: (Number | Bool)) =>
      val leftNumber = extractNumber(value1)
      val rightNumber = extractNumber(value2)
      // calculate arithmetic operation
      val result = op match {
        case Add  => leftNumber + rightNumber
        case Sub  => leftNumber - rightNumber
        case Mul  => leftNumber * rightNumber
        case Div  => leftNumber / rightNumber
        case Mod  => leftNumber % rightNumber
        case Expo => scala.math.pow(leftNumber, rightNumber)
        case null => assert(false, "unreachable")
      }
      scope.returnValue(Number(result)) // Adding the result to the scope
    case (v1, v2) =>
      val type1 = typeOf(v1)
      val type2 = typeOf(v2)
      assert(false, s"Operator '$op' is not defined for '$type1' and '$type2'")
  }
}

// Input: Number or Bool. Output: Double (true == 1, false == 0)
def extractNumber(value: Value): Double = value match {
  case Number(n)   => n
  case Bool(b)     => if (b) 1.0 else 0.0
  case NoneValue() => 0.0
  case v => assert(false, s"Expected number or boolean in comparison. Got '$v'")
}

def printValues(values: Seq[Value], scope: Scope): Unit =
  val output = values
    .map(_.toString)
    .mkString(" ")
  print(output)
