import ArithmaticOps.{Add, Div, Expo, Mul, Sub}
import BooleanOps.{And, Or}
import CompareOps.{Eq, Greater, GreaterEq, Less, LessEq, NotEq}
import scala.util.control.Breaks._

type HashMap[K, V] = scala.collection.mutable.HashMap[K, V]

class Scope(
    parent: Scope = null,
    funArgs: Seq[String] = Seq(),
    callArgs: Seq[Value] = Seq()
):
  private var parentScope: Scope = parent
  private var _result: Value = null
  private var local: HashMap[String, Value] =
    new HashMap().addAll(funArgs.zip(callArgs))

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
    this.local.get(identifier.name) match {
      case Some(Identifier(name)) =>
        if (name == identifier.name)
          lookupInParent(identifier)
        else
          this.lookup(Identifier(name))
      case Some(value) => Some(value)
      case None        => lookupInParent(identifier)
    }

  private def lookupInParent(identifier: Identifier): Option[Value] =
    if (this.parentScope != null)
      this.parentScope.lookup(identifier)
    else
      None

  def update(identifier: Identifier, value: Value): Scope =
    this.local.update(identifier.name, value)
    this
end Scope

def evalFunctionCall(
    identifier: String,
    callArgs: Seq[Value],
    scope: Scope
): Scope =
  if (identifier == "print") {
    for (arg <- callArgs)
      val tmp = evalValue(arg, scope)
      val Some(res) = tmp.result: @unchecked
      printValue(res)
    print("\n")
    scope
  } else {
    scope.lookup(Identifier(identifier)) match {
      case Some(Function(args, body)) =>
        val res =
          body.foldLeft(Scope(scope, args, callArgs))(evalStatement)
        val Some(value) = res.result: @unchecked
        scope.returnValue(value)
      case Some(_) => assert(false, "Only functions may be called")
      case None =>
        assert(
          false,
          s"TODO: case None in function call in eval '$identifier'"
        )
    }
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

      case va =>
        scope.result match {
          case None    => scope.returnValue(va)
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
        evalFunctionCall(identifier, callArgs, scope)
      case Return(value) =>
        evalReturn(value, scope)
      case Expression(expr) =>
        val result = evalValue(expr, scope)
        val Some(return_value) = result.result: @unchecked
        result.returnValue(return_value)
      case If(ifBranch, elifBranches, elseBranch) => {
        evalIf(ifBranch, elifBranches, elseBranch, scope)
      }

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
        Some(branch.body.foldLeft(scope)((currentScope, statement) => evalStatement(currentScope, statement)))
      case _ =>
        None
    }
  }

  // Check initial branch condition
  var resultScope = scope
  evalBranch(initialBranch, scope) match {
    case Some(updatedScope) => return updatedScope // Return updated scope if the initial branch is true
    case None =>
  }

  // Check elif branches by iteration.
  var conditionMet = false
  breakable {
    for (branch <- elifBranches) {
      evalBranch(branch, resultScope) match {
        case Some(updatedScope) =>
          conditionMet = true
          resultScope = updatedScope
          break // Break out of the loop as soon as one branch is true
        case None => // Continue to the next elif if the current one fails
      }
    }
  }

  // Check if the else branch should be executed
  if (!conditionMet && elseBranch.isDefined) {
    evalBranch(Branch(Bool(true), elseBranch.get), resultScope) match { // Create a mock branch with a true condition and the else branch body
      case Some(updatedScope) => return updatedScope
      case None =>
    }
  }

  resultScope // Return the updated or original scope
}

def evalValue(
    v: Value,
    scope: Scope
): Scope =
  v match {
    case Function(args, body) =>
      scope.returnValue(Function(args, body))
    case FunctionCall(identifier, callArgs) =>
      evalFunctionCall(identifier, callArgs, scope)
    case BinaryOp(left, op, right) =>
      val left_result = evalValue(left, scope)
      val Some(new_left) = left_result.result: @unchecked
      val right_result = evalValue(right, scope)
      val Some(new_right) = right_result.result: @unchecked
      evalBinaryOp(op, new_left, new_right, scope)
    case Identifier(name) =>
      scope.lookup(Identifier(name)) match {
        case Some(value) =>
          evalValue(value, scope)
        case None =>
          assert(false, s"identifier '$name' does not exist")
      }
    case Number(value) =>
      scope.returnValue(Number(value))
    case Bool(value) =>
      scope.returnValue(Bool(value))
    case Wrapped(value) =>
      evalValue(value, scope)
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
  if (leftEval.isInstanceOf[Identifier] || rightEval.isInstanceOf[Identifier]) {
    op match {
      case Eq =>
        scope.returnValue(
          Bool(
            leftEval.asInstanceOf[Identifier].name == rightEval
              .asInstanceOf[Identifier]
              .name
          )
        )
      case NotEq =>
        scope.returnValue(
          Bool(
            leftEval.asInstanceOf[Identifier].name != rightEval
              .asInstanceOf[Identifier]
              .name
          )
        )
      case _ => assert(false, "On Strings, only == and != are allowed.")
    }
    return scope
  }

  // otherwise left and right are bools or numbers
  val result = op match {
    case Less      => extractNumber(leftEval) < extractNumber(rightEval)
    case LessEq    => extractNumber(leftEval) <= extractNumber(rightEval)
    case Greater   => extractNumber(leftEval) > extractNumber(rightEval)
    case GreaterEq => extractNumber(leftEval) >= extractNumber(rightEval)
    case Eq        => leftEval == rightEval
    case NotEq     => leftEval != rightEval
    case null      => assert(false, "Unexpected comparison operation.")
  }

  scope.returnValue(Bool(result)) // Adding the result to the scope
}

def evalArithmeticOps(
    op: ArithmaticOps,
    left: Value,
    right: Value,
    scope: Scope
): Scope = {
  // evaluate left and right value
  val resLeft = evalValue(left, scope)
  val Some(leftEval) = resLeft.result: @unchecked
  val resRight = evalValue(right, scope)
  val Some(rightEval) = resRight.result: @unchecked

  if (leftEval.isInstanceOf[Identifier] || rightEval.isInstanceOf[Identifier]) {
    assert(false, "No arithmetic operations of string allowed.")
  }

  val leftNumber = extractNumber(leftEval)
  val rightNumber = extractNumber(rightEval)
  // calculate arithmetic operation
  val result = op match {
    case Add  => leftNumber + rightNumber
    case Sub  => leftNumber - rightNumber
    case Mul  => leftNumber * rightNumber
    case Div  => leftNumber / rightNumber
    case Expo => scala.math.pow(leftNumber, rightNumber)
    case null => assert(false, "Arithmetic operation is null")
  }
  scope.returnValue(Number(result)) // Adding the result to the scope
}

// Input: Number or Bool. Output: Double (true == 1, false == 0)
def extractNumber(value: Value): Double = value match {
  case Number(n) => n
  case Bool(b)   => if (b) 1.0 else 0.0
  case _         => assert(false, "Expected number or boolean in comparison.")
}

def printValue(value: Value) =
  // NOTE: We assume we have only primitive values
  value match {
    case Number(value) =>
      print(value)
    case err =>
      assert(false, s"Value is not printable: $err")
  }
  print(" ")
