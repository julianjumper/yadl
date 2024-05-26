import ArithmaticOps.{Add, Div, Expo, Mul, Sub}
import BooleanOps.{And, Or}
import CompareOps.{Eq, Greater, GreaterEq, Less, LessEq, NotEq}

type HashMap[K, V] = scala.collection.mutable.HashMap[K, V]

val RETURN_VALUE = "a newly returned value"
val NEW_VALUE = "a new value"

def evalStatement(
    st: Statement,
    scope: HashMap[String, Value]
): HashMap[String, Value] =
  // First we differentiate of what the current statement is.
  // The types are given by the statementP parsing rule in the Parser file.
  // Our goal of this function obviously is to handle all statements. This can either be a definition/re-definition
  // of a variable. Then we want to update the Hashmap. Or loops, if-else, function calls, return (see statementP rule).
  st match {
    case Assignment(name, value) =>
      // Assignments have the form variable = Value. See the ValueP rule of the parser for the types that are assignable.
      // Our goal here is it to evaluate the right-hand side. Eg. x = 4+9, then we want to add the variable x to the
      // Hashmap with the value 13 as a number.
      val result = evalValue(value, scope)
      // NOTE: `NEW_VALUE` should allways exist. It indicates a bug in `evalValue` if it does not
      // The evalValue-function stores the newly calculated value in the Hashmap with the key NEW_VALUE. We need to
      // replace NEW_VALUE with the actual variable name.
      val Some(newValue) = result.remove(NEW_VALUE): @unchecked
      scope.update(name, newValue)
      scope
    case FunctionCall(identifier, callArgs) =>
      if (identifier == "print") {
        for (arg <- callArgs)
          val tmp = evalValue(arg, scope)
          // NOTE: `NEW_VALUE` should allways exist. It indicates a bug in `evalValue` if it does not
          val Some(res) = tmp.remove(NEW_VALUE): @unchecked
          printValue(res)
        print("\n")
        scope
      } else {
        scope.get(identifier) match {
          case Some(Function(args, body)) =>
            call(args, body, callArgs, scope)
          case Some(_) => assert(false, "Only functions may be called")
          case None =>
            assert(
              false,
              s"TODO: case None in function call in eval '$identifier'"
            )
        }
      }
    case Return(_) =>
      assert(false, "Return Statement is not allowed in global scope")
  }

def evalValue(
    v: Value,
    scope: HashMap[String, Value]
): HashMap[String, Value] =
  v match {
    case Function(args, body) =>
      scope.addOne((NEW_VALUE, Function(args, body)))
    case FunctionCall(identifier, callArgs) =>
      scope.get(identifier) match {
        case Some(Function(args, body)) =>
          val result = call(args, body, callArgs, scope)
          // NOTE: `RETURN_VALUE` should allways exist. It indicates a bug in `call` if it does not
          val Some(value) = result.remove(RETURN_VALUE): @unchecked
          result.addOne((NEW_VALUE, value))
        case None =>
          assert(false, s"function '$identifier' not found")
        case Some(_) =>
          assert(false, s"the identifier '$identifier' is not a function")
      }
    case BinaryOp(left, op, right) =>
      val left_result = evalValue(left, scope)
      val Some(new_left) = left_result.remove(NEW_VALUE): @unchecked
      evalValue(right, scope)
      val Some(new_right) = left_result.remove(NEW_VALUE): @unchecked
      evalBinaryOp(op, new_left, new_right, scope)
    case Identifier(name) =>
      scope.get(name) match {
        case Some(value) =>
          scope.addOne((NEW_VALUE, value))
        case None =>
          assert(false, s"identifier '$name' does not exist")
      }
    case Number(value) =>
      scope.addOne((NEW_VALUE, Number(value)))
    case Bool(value) =>
      scope.addOne((NEW_VALUE, Bool(value)))
    case Wrapped(value) =>
      evalValue(value, scope)
    case err =>
      assert(false, f"TODO: not implemented '$err'")
  }
  scope
  // assert(false, "not implemented") // TODO des

def evalBinaryOp(
    op: Operator,
    left: Value,
    right: Value,
    scope: HashMap[String, Value]
): HashMap[String, Value] = {
    op match
      case ArithmaticOp(ops) => evalArithmeticOps(ops, left, right, scope)
      case CompareOp(ops) => evalCompareOps(ops, left, right, scope)
      case BooleanOp(ops) => evalBooleanOps(ops, left, right, scope)
      case _ => throw new IllegalArgumentException("Binary operation invalid.")
}

def evalBooleanOps(op: BooleanOps,
                   left: Value,
                   right: Value,
                   scope: HashMap[String, Value]
                  ): HashMap[String, Value] = {
  // evaluate left and right value
  val leftEval = left match {
    case Number(n) => Number(n)
    case Bool(b) => Bool(b)
    case id: Identifier => scope.getOrElse(id.name, assert(false, "Variable not in scope."))
    case _ => assert(false, "Unexpected left-hand type in boolean operator.")
  }

  val rightEval = right match {
    case Number(n) => Number(n)
    case Bool(b) => Bool(b)
    case id: Identifier => scope.getOrElse(id.name, assert(false, "Variable not found in scope."))
    case _ => assert(false, "Unexpected right-hand type in boolean operator.")
  }

  // if left or right is string.
  if (leftEval.isInstanceOf[Identifier] || rightEval.isInstanceOf[Identifier]) {
    assert(false, "No boolean operators allowed on strings")
  }

  // otherwise left and right are bools or numbers
  val result = op match {
    case And => (extractNumber(leftEval) > 0) && (extractNumber(rightEval) > 0)
    case Or => (extractNumber(leftEval) > 0) || (extractNumber(rightEval) > 0)
    case _ => assert(false, "Unexpected comparison operation.")
  }

  scope.addOne((NEW_VALUE, Bool(result))) // Adding the result to the scope
  scope
}

def evalCompareOps(op: CompareOps,
                   left: Value,
                   right: Value,
                   scope: HashMap[String, Value]
                  ): HashMap[String, Value] = {
  // evaluate left and right value
  val leftEval = left match {
    case Number(n) => Number(n)
    case Bool(b) => Bool(b)
    case id: Identifier => scope.getOrElse(id.name, assert(false, "Variable not in scope."))
    case _ => assert(false, "Unexpected left-hand type in comparison.")
  }

  val rightEval = right match {
    case Number(n) => Number(n)
    case Bool(b) => Bool(b)
    case id: Identifier => scope.getOrElse(id.name, assert(false, "Variable not found in scope."))
    case _ => assert(false, "Unexpected right-hand type in comparison.")
  }

  // if left or right is string.
  if (leftEval.isInstanceOf[Identifier] || rightEval.isInstanceOf[Identifier]) {
    op match {
      case Eq => scope.addOne((NEW_VALUE, Bool(leftEval.asInstanceOf[Identifier].name == rightEval.asInstanceOf[Identifier].name)))
      case NotEq => scope.addOne((NEW_VALUE, Bool(leftEval.asInstanceOf[Identifier].name != rightEval.asInstanceOf[Identifier].name)))
      case _ => assert(false, "On Strings, only == and != are allowed.")
    }
    return scope
  }

  // otherwise left and right are bools or numbers
  val result = op match {
    case Less => extractNumber(leftEval) < extractNumber(rightEval)
    case LessEq => extractNumber(leftEval) <= extractNumber(rightEval)
    case Greater => extractNumber(leftEval) > extractNumber(rightEval)
    case GreaterEq => extractNumber(leftEval) >= extractNumber(rightEval)
    case Eq => leftEval == rightEval
    case NotEq => leftEval != rightEval
    case null => assert(false, "Unexpected comparison operation.")
  }

  scope.addOne((NEW_VALUE, Bool(result))) // Adding the result to the scope
  scope
}

def evalArithmeticOps(
    op: ArithmaticOps,
    left: Value,
    right: Value,
    scope: HashMap[String, Value]
): HashMap[String, Value] = {
  // evaluate left and right value
  val leftEval = left match {
    case Number(n) => Number(n)
    case Bool(b) => Bool(b)
    case id: Identifier => scope.getOrElse(id.name, assert(false, "Variable not in scope."))
    case _ => assert(false, "Unexpected left-hand type in comparison.")
  }

  val rightEval = right match {
    case Number(n) => Number(n)
    case Bool(b) => Bool(b)
    case id: Identifier => scope.getOrElse(id.name, assert(false, "Variable not found in scope."))
    case _ => assert(false, "Unexpected right-hand type in comparison.")
  }

  if (leftEval.isInstanceOf[Identifier] || rightEval.isInstanceOf[Identifier]) {
    assert(false, "No arithmetic operations of string allowed.")
  }

  val leftNumber = extractNumber(leftEval)
  val rightNumber = extractNumber(rightEval)
  // calculate arithmetic operation
  val result = op match {
    case Add => leftNumber + rightNumber
    case Sub => leftNumber - rightNumber
    case Mul => leftNumber * rightNumber
    case Div => leftNumber / rightNumber
    case Expo => scala.math.pow(leftNumber, rightNumber)
    case null => assert(false, "Arithmetic operation is null")
  }
  scope.addOne((NEW_VALUE, Number(result))) // Adding the result to the scope
  scope
}

// Input: Number or Bool. Output: Double (true == 1, false == 0)
def extractNumber(value: Value): Double = value match {
  case Number(n) => n
  case Bool(b) => if (b) 1.0 else 0.0
  case _ => assert(false, "Expected number or boolean in comparison.")
}

def call(
    args: Seq[String],
    body: Seq[Statement],
    callArgs: Seq[Value],
    scope: HashMap[String, Value]
): HashMap[String, Value] =
  val newScope = scope.addAll(args.zip(callArgs))
  var returnScope = body.foldLeft(newScope)((acc, x) =>
    if (acc.contains(RETURN_VALUE))
      acc
    else
      x match {
        case Assignment(name, value) =>
          val result = evalValue(value, acc)
          val Some(newValue) = result.remove(NEW_VALUE): @unchecked
          acc.update(name, newValue)
          acc
        case FunctionCall(id, callArgsInner) =>
          acc.get(id) match {
            case Some(Function(a, b)) =>
              call(a, b, callArgsInner, acc)
            case Some(_) =>
              assert(
                false,
                s"'eval function call': '$id' is not a function"
              )
            case None =>
              assert(
                false,
                f"in function call: no function with identifier '$id%s'"
              )
          }
        case Return(Identifier(name)) =>
          (acc.get(RETURN_VALUE), acc.get(name)) match {
            case (None, Some(v)) => acc.addOne((RETURN_VALUE, v))
            case (_, _)          => acc
          }
        case Return(value) =>
          acc.get(RETURN_VALUE) match {
            case None => acc.addOne((RETURN_VALUE, value))
            case _    => acc
          }
        case Expression(expr) =>
          val result = evalValue(expr, acc)
          val Some(return_value) = result.remove(NEW_VALUE): @unchecked
          result.addOne((RETURN_VALUE, return_value))
      }
  )
  returnScope.subtractAll(args)
  returnScope

def printValue(value: Value) =
  // NOTE: We assume we have only primitive values
  value match {
    case Number(value) =>
      print(value)
    case err =>
      assert(false, s"Value is not printable: $err")
  }
  print(" ")
