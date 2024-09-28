import parser.*
import ArithmaticOps.{Add, Div, Expo, Mod, Mul, Sub}
import BooleanOps.{And, Not, Or}
import CompareOps.{Eq, Greater, GreaterEq, Less, LessEq, NotEq}
import scala.collection.mutable.Stack
import scala.annotation.unused

val builtins = stdlib.stdlib

type HashMap[K, V] = scala.collection.mutable.HashMap[K, V]
type MutArray[V] = scala.collection.mutable.ArrayBuffer[V]

class Scope(
    parent: Scope = null,
    funArgs: Seq[String] = Seq(),
    callArgs: Seq[Expression] = Seq()
):
  private var parentScope: Scope = parent
  private var _result: Expression = null
  private var localVars: HashMap[String, Expression] =
    new HashMap().addAll(funArgs.zip(callArgs))
  private var localFuncs: HashMap[String, parser.Function] = new HashMap

  def result: Option[Expression] =
    if (this._result == null) None
    else
      val tmp = this._result
      this._result = null
      Some(tmp)

  def hasResult: Boolean = this._result != null
  def isGlobal: Boolean = this.parentScope == null

  def returnExpression(value: Expression): Scope =
    this._result = value
    this

  def lookup(identifier: Identifier): Option[Expression] =
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

  private def lookupInParent(identifier: Identifier): Option[Expression] =
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

  def update(identifier: Identifier, value: Expression): Scope =
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
      case v: Expression =>
        this.localVars.update(identifier.name, v)
    }
    this
end Scope

def captureExternalsExpression(
    v: Expression,
    locals: MutArray[String],
    scope: Scope
): Expression =
  v match {
    case Identifier(name) =>
      if (!locals.contains(name))
        val vl = scope.lookup(Identifier(name)) match {
          case Some(v) => v
          case None    => assert(false, s"'$name' does not exist")
        }
        val Some(value) = evalExpression(vl, scope).result: @unchecked
        value
      else Identifier(name)
    case Wrapped(value) => captureExternalsExpression(value, locals, scope)
    case FunctionCall(functionExpr, args) =>
      val fx = captureExternalsExpression(functionExpr, locals, scope)
      val fas = args.map(captureExternalsExpression(_, locals, scope))
      FunctionCall(fx, fas)
    case Function(args, body) =>
      val ls = locals.clone().appendAll(args)
      val b = body.map(captureExternalsStatement(_, locals, scope))
      Function(args, b)
    case ArrayLiteral(elements) =>
      val tmp = elements.map(captureExternalsExpression(_, locals, scope))
      ArrayLiteral(tmp)
    case BinaryOp(l, op, r) =>
      val nl = captureExternalsExpression(l, locals, scope)
      val nr = captureExternalsExpression(r, locals, scope)
      BinaryOp(nl, op, nr)
    case UnaryOp(op, vl) =>
      val v = captureExternalsExpression(vl, locals, scope)
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
        val newVal = captureExternalsExpression(value, locals, scope)
        locals.addOne(varName)
        Assignment(varName, newVal)
      } else Assignment(varName, value)
    case StructuredAssignment(struct, value) =>
      val s = captureExternalsExpression(struct.identifier, locals, scope)
      val v = captureExternalsExpression(value, locals, scope)
      StructuredAssignment(StructureAccess(s, struct.key), v)
    case fc: FunctionCall =>
      val tmp = captureExternalsExpression(fc.functionExpr, locals, scope)
      val tmp2 = fc.args.map(x => captureExternalsExpression(x, locals, scope))
      FunctionCall(tmp, tmp2)
    case Return(value) =>
      val v = captureExternalsExpression(value, locals, scope)
      Return(v)
    case WhileLoop(loop) =>
      val cond = captureExternalsExpression(loop.condition, locals, scope)
      val body = loop.body.map(captureExternalsStatement(_, locals, scope))
      WhileLoop(Branch(cond, body))
    case If(ifBranch, elifBranches, elseBranch) =>
      val ifCond = captureExternalsExpression(ifBranch.condition, locals, scope)
      val ifBody =
        ifBranch.body.map(captureExternalsStatement(_, locals, scope))
      val newIf = Branch(ifCond, ifBody)

      val elifBranchs =
        elifBranches.foldLeft(Seq(): Seq[Branch])((acc, br) =>
          val cond = captureExternalsExpression(br.condition, locals, scope)
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
  case Statement, Expression

def evalFunctionCall(
    functionExpr: Expression,
    callArgs: Seq[Expression],
    scope: Scope,
    context: CallContext
): Scope =
  val evaledCallArgs = callArgs.map(evalExpression(_, scope).result.get)
  functionExpr match {
    case Identifier(identifier) =>
      if (identifier == "print") {
        printExpressions(evaledCallArgs, scope)
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
        if (context == CallContext.Expression)
          scope.returnExpression(interpreterdata.toAstNode(result))
        else scope
      } else {
        scope.lookupFunction(Identifier(identifier)) match {
          case Some(Function(args, body)) =>
            // TODO: merge the capture with the current scope
            val res =
              body.foldLeft(Scope(scope, args, evaledCallArgs))(evalStatement)
            res.result match {
              case Some(value) =>
                if (context == CallContext.Expression)
                  scope.returnExpression(value)
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
      if (context == CallContext.Expression)
        scope.returnExpression(value)
      else scope

    case FunctionCall(functionExpr, args) =>
      val Some(Function(args1, body1)) =
        evalFunctionCall(functionExpr, args, scope, context).result: @unchecked
      val res =
        body1.foldLeft(Scope(scope, args1, evaledCallArgs))(evalStatement)
      val Some(value) = res.result: @unchecked
      if (context == CallContext.Expression)
        scope.returnExpression(value)
      else scope
  }

def evalReturn(value: Expression, scope: Scope): Scope =
  if (scope.isGlobal) assert(false, "can not return from global scope")
  else
    value match {
      case id: Identifier =>
        (scope.result, scope.lookup(id)) match {
          case (None, Some(v)) => scope.returnExpression(v)
          case (Some(v), _)    => scope.returnExpression(v)
          case (None, None) =>
            assert(
              false,
              s"identifier '${id.name}' does not exist that could be returned"
            )
        }

      case f: Function =>
        val tmp =
          Function(f.args, captureExternals(scope, f.args, f.body))
        scope.returnExpression(f)
      case va =>
        scope.result match {
          case None =>
            val Some(res) = evalExpression(va, scope).result: @unchecked
            scope.returnExpression(res)
          case Some(v) => scope.returnExpression(v)
        }
    }

class AccessContext():
  private var context: Stack[(Expression, Expression)] = new Stack

  def push(key: Expression, value: Expression): Unit =
    this.context.push((key, value))

  def pop(): Option[(Expression, Expression)] =
    try {
      Some(this.context.pop())
    } catch {
      case e =>
        None
    }

end AccessContext

def modifyStructure(
    struct: Expression,
    index: Expression,
    value: Expression
): Expression =
  struct match {
    case Dictionary(entries) =>
      if (entries.foldLeft(false)((acc, e) => acc || e.key == index))
        Dictionary(
          entries.map(e =>
            if (e.key == index) DictionaryEntry(e.key, value) else e
          )
        )
      else
        Dictionary(entries :+ DictionaryEntry(index, value))
    case ArrayLiteral(elements) =>
      assert(
        index.isInstanceOf[Number] && index
          .asInstanceOf[Number]
          .isInstanceOf[YadlInt],
        s"arrays may only be indexed by a number. got: $index"
      )
      val v = index.asInstanceOf[Number].asInstanceOf[YadlInt]
      assert(v.value == v.value.toInt, "index to array is not an integer")
      var tmp = elements.toArray
      tmp.update(v.value.toInt, value)
      ArrayLiteral(tmp)
    case v => assert(false, s"not modifiable structure: $v")
  }

def evalStructAssignment(
    st: StructureAccess,
    value: Expression,
    accessContext: AccessContext,
    scope: Scope
): Scope =
  val Some(struct) = evalExpression(st.identifier, scope).result: @unchecked
  val Some(index) = evalExpression(st.key, scope).result: @unchecked
  val newStruct = modifyStructure(struct, index, value)
  st.identifier match {
    case id: Identifier =>
      scope.update(id, newStruct)
    case s: StructureAccess =>
      accessContext.push(s.key, newStruct)
      evalStructAssignment(
        s,
        newStruct,
        accessContext,
        scope
      )
    case struct: (ArrayLiteral | Dictionary) =>
      accessContext.pop() match {
        case Some((_, v)) =>
          val Some(index) = evalExpression(st.key, scope).result: @unchecked
          val newStruct = modifyStructure(struct, index, v)
          scope.returnExpression(newStruct)
        case None =>
          scope.returnExpression(struct)
      }
    case v: Expression =>
      assert(false, s"Expression '$v' can not be accessed/modified")
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
        // Assignments have the form variable = Expression. See the `valueP` rule of the parser for the types that are assignable.
        // Our goal here is it to evaluate the right-hand side. Eg. x = 4+9, then we want to add the variable x to the
        // Scope with the value 13 as a number.
        val result = evalExpression(value, scope)
        // NOTE: `.result` should allways exist. It indicates a bug in `evalExpression` if it does not
        // The evalExpression-function stores the newly calculated value in the Scope in the variable `result`. We need to
        // update the variable `name` with this new value.
        val Some(newExpression) = result.result: @unchecked
        scope.update(Identifier(name), newExpression)
      case FunctionCall(identifier, callArgs) =>
        evalFunctionCall(
          identifier,
          callArgs,
          scope,
          CallContext.Statement
        )
      case StructuredAssignment(struct, value) =>
        evalStructAssignment(
          struct,
          value,
          AccessContext(),
          scope
        ).result match {
          case _ => scope
        }
      case Return(value) =>
        evalReturn(value, scope)
      case If(ifBranch, elifBranches, elseBranch) => {
        evalIf(ifBranch, elifBranches, elseBranch, scope)
      }
      case WhileLoop(loop) =>
        evalWhileLoop(loop, scope)
    }

def evalWhileLoop(whileLoop: Branch, scope: Scope): Scope = {
  var currentScope = scope
  while (
    evalExpression(whileLoop.condition, currentScope).result.contains(
      Bool(true)
    )
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
    val result = evalExpression(branch.condition, scope)
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

def evalExpression(
    v: Expression,
    scope: Scope
): Scope =
  v match {
    case Function(args, body) =>
      scope.returnExpression(
        Function(args, captureExternals(scope, args, body))
      )
    case FunctionCall(identifier, callArgs) =>
      evalFunctionCall(identifier, callArgs, scope, CallContext.Expression)
    case BinaryOp(left, op, right) =>
      val left_result = evalExpression(left, scope)
      val Some(new_left) = left_result.result: @unchecked
      val right_result = evalExpression(right, scope)
      val Some(new_right) = right_result.result: @unchecked
      evalBinaryOp(op, new_left, new_right, scope)
    case UnaryOp(op, value) =>
      val Some(result) = evalExpression(value, scope).result: @unchecked
      op match {
        case ArithmaticOp(Add) => scope.returnExpression(result)
        case ArithmaticOp(Sub) =>
          result match {
            case YadlInt(n) =>
              scope.returnExpression(YadlInt(-n))
            case YadlFloat(n) =>
              scope.returnExpression(YadlFloat(-n))
            case v =>
              assert(false, s"unary op: value case '$v' is not implemented")
          }
        case BooleanOp(Not) =>
          val tmp = result match {
            case Bool(value) => Bool(!value)
            case v =>
              assert(false, s"unary operator 'not' is not defined for '$v'")
          }
          scope.returnExpression(tmp)
        case o => assert(false, s"unary op: op case '$o' is not implemented")
      }
    case StructureAccess(id, v) =>
      id match {
        case s: StructureAccess =>
          val Some(value) = evalExpression(s, scope).result: @unchecked
          evalExpression(StructureAccess(value, v), scope)
        case Wrapped(value) =>
          evalExpression(StructureAccess(value, v), scope)
        case f: FunctionCall =>
          val Some(value) =
            evalFunctionCall(
              f.functionExpr,
              f.args,
              scope,
              CallContext.Expression
            ).result: @unchecked
          evalExpression(StructureAccess(value, v), scope)
        case id: Identifier =>
          scope.lookup(id) match {
            case Some(d: Dictionary) =>
              evalExpression(StructureAccess(d, v), scope)
            case Some(a: ArrayLiteral) =>
              evalExpression(StructureAccess(a, v), scope)
            case _ =>
              assert(false, s"no structure found by the name '${id.name}'")
          }
        case Dictionary(entries) =>
          val result: Option[Expression] = entries.foldLeft(None) {
            (acc, curr) =>
              acc match {
                case None =>
                  val res = evalExpression(curr.key, scope)
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
            case Some(value) => scope.returnExpression(value)
            case None =>
              assert(false, s"no value found for the key '$v' in a dictionary")
          }
        case ArrayLiteral(elements) =>
          val Some(value) = evalExpression(v, scope).result: @unchecked
          value match {
            case n: Number => {
              if (!n.isInstanceOf[YadlInt]) {
                throw IllegalArgumentException(
                  "expected hole number, but got number with decimal part"
                )
              }
              val YadlInt(index) = n: @unchecked
              val tmp = elements(index.toInt)
              scope.returnExpression(tmp)
            }
            case x =>
              throw IllegalArgumentException(
                "expected number, not: " + x.toString
              )
          }
        case value => assert(false, s"not an accessable structure: $value")
      }
    case Identifier(name) =>
      scope.lookup(Identifier(name)) match {
        case Some(value) =>
          scope.returnExpression(value)
        case None =>
          assert(false, s"identifier '$name' does not exist")
      }
    case value: Number =>
      scope.returnExpression(value)
    case StdString(value) =>
      scope.returnExpression(StdString(value))
    case Bool(value) =>
      scope.returnExpression(Bool(value))
    case Wrapped(value) =>
      evalExpression(value, scope)
    case Dictionary(entries) =>
      scope.returnExpression(Dictionary(entries))
    case FormatString(value) =>
      assert(false, "TODO: Format strings in eval implementation")
    case ArrayLiteral(elements) =>
      scope.returnExpression(ArrayLiteral(elements))
    case NoneValue() =>
      scope.returnExpression(NoneValue())
    case err =>
      assert(false, f"TODO: not implemented '$err'")
  }

def evalBinaryOp(
    op: Operator,
    left: Expression,
    right: Expression,
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
    left: Expression,
    right: Expression,
    scope: Scope
): Scope = {
  // evaluate left and right value
  val Some(leftEval) = evalExpression(left, scope).result: @unchecked
  val Some(rightEval) = evalExpression(right, scope).result: @unchecked

  // if left or right is string.
  if (leftEval.isInstanceOf[Identifier] || rightEval.isInstanceOf[Identifier]) {
    assert(false, "No boolean operators allowed on strings")
  }

  val Some(Bool(valueLeft)) = evalCompareOps(
    NotEq,
    extractNumber(leftEval),
    YadlInt(0),
    scope
  ).result: @unchecked
  val Some(Bool(valueRight)) = evalCompareOps(
    NotEq,
    extractNumber(rightEval),
    YadlInt(0),
    scope
  ).result: @unchecked

  // otherwise left and right are bools or numbers
  val result = op match {
    case And => valueLeft && valueRight
    case Or  => valueLeft || valueRight
    case _   => assert(false, "Unexpected comparison operation.")
  }

  scope.returnExpression(Bool(result)) // Adding the result to the scope
}

def evalCompareOps(
    op: CompareOps,
    left: Expression,
    right: Expression,
    scope: Scope
): Scope = {
  // evaluate left and right value
  val Some(leftEval) = evalExpression(left, scope).result: @unchecked
  val Some(rightEval) = evalExpression(right, scope).result: @unchecked
  // if left or right is string.
  (leftEval, rightEval) match {
    case (s1: StdString, s2: StdString) => {
      op match {
        case Eq =>
          scope.returnExpression(Bool(s1.value == s2.value))
        case NotEq =>
          scope.returnExpression(Bool(s1.value != s2.value))
        case _ =>
          assert(
            false,
            "For Strings only the operators '==' and '!=' are allowed."
          )
      }
    }
    case (
          value1: (YadlInt | YadlFloat | Bool | NoneValue),
          value2: (YadlInt | YadlFloat | Bool | NoneValue)
        ) => {
      // otherwise left and right are bools, numbers or none
      val lhs_num = extractNumber(value1)
      val rhs_num = extractNumber(value2)
      val result = op match {
        case Less => lhs_num < rhs_num
        case Eq   => lhs_num == rhs_num

        case LessEq    => lhs_num < rhs_num || lhs_num == rhs_num
        case Greater   => !(lhs_num < rhs_num || lhs_num == rhs_num)
        case GreaterEq => !(lhs_num < rhs_num)
        case NotEq     => !(lhs_num == rhs_num)
      }

      scope.returnExpression(Bool(result)) // Adding the result to the scope
    }
    case (v1, v2) =>
      // TODO: handling these cases properly
      op match {
        case Eq =>
          scope.returnExpression(Bool(false)) // Adding the result to the scope
        case NotEq =>
          scope.returnExpression(Bool(true)) // Adding the result to the scope
        case op =>
          val type_v1 = typeOf(v1)
          val type_v2 = typeOf(v2)
          assert(
            false,
            s"the values '$v1'($type_v1) and '$v2'($type_v2) are not comparable under '$op'"
          )
      }
  }
}

def typeOf(value: Expression): String =
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
    left: Expression,
    right: Expression,
    scope: Scope
): Scope = {
  // evaluate left and right value
  val Some(leftEval) = evalExpression(left, scope).result: @unchecked
  val Some(rightEval) = evalExpression(right, scope).result: @unchecked

  (leftEval, rightEval) match {
    case (s1: StdString, s2: StdString) =>
      op match {
        case Add =>
          val tmp = s1.value + s2.value
          scope.returnExpression(StdString(tmp))
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
        case Expo => leftNumber ^ rightNumber
        case null => assert(false, "unreachable")
      }
      scope.returnExpression(result) // Adding the result to the scope
    case (v1, v2) =>
      val type1 = typeOf(v1)
      val type2 = typeOf(v2)
      val result = op match {
        case Add => v1.toString() + v2.toString()
        case Mul =>
          if (type1 == "number" && v1.isInstanceOf[YadlInt])
            v2.toString().repeat(v1.asInstanceOf[YadlInt].value.toInt)
          else if (type2 == "number" && v2.isInstanceOf[YadlInt])
            v1.toString().repeat(v2.asInstanceOf[YadlInt].value.toInt)
          else
            assert(
              false,
              s"Operator '$op' is not defined for '$type1' and '$type2'"
            )
        case _ =>
          assert(
            false,
            s"Operator '$op' is not defined for '$type1' and '$type2'"
          )
      }
      scope.returnExpression(
        StdString(result)
      ) // Adding the result to the scope
  }
}

// Input: Number or Bool. Output: Double (true == 1, false == 0)
def extractNumber(value: Expression): Number = value match {
  case n: Number   => n
  case Bool(b)     => if (b) YadlInt(1) else YadlInt(0)
  case NoneValue() => YadlInt(0)
  case v => assert(false, s"Expected number or boolean in comparison. Got '$v'")
}

def printExpressions(values: Seq[Expression], scope: Scope): Unit =
  val output = values
    .map(_.toString)
    .mkString(" ")
  print(output)
