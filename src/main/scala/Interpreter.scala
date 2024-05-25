type HashMap[K, V] = scala.collection.mutable.HashMap[K, V]

val RETURN_VALUE = "a newly returned value"
val NEW_VALUE = "a new value"

def evalStatement(
    st: Statement,
    scope: HashMap[String, Value]
): HashMap[String, Value] =
  st match {
    case Assignment(name, value) =>
      val result = evalValue(value, scope)
      // NOTE: `NEW_VALUE` should allways exist. It indicates a bug in `evalValue` if it does not
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
      // TODO: Add proper handling of binary operations
      val left_result = evalValue(left, scope)
      val Some(new_left) = left_result.remove(NEW_VALUE): @unchecked
      evalValue(right, scope)
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
