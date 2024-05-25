type HashMap[K, V] = scala.collection.mutable.HashMap[K, V]

def evalStatement(
    st: Statement,
    scope: HashMap[String, Value]
): HashMap[String, Value] =
  st match {
    case Assignment(name, value) =>
      val result = evalValue(value, scope)
      val newValue = result.get("new value") match {
        case Some(v) => v
        case None =>
          assert(false, "unreachable: a new value should always be returned")
      }
      val _ = result.remove("new value")
      scope.update(name, newValue)
      scope
    case FunctionCall(identifier, callArgs) =>
      scope.get(identifier) match {
        case Some(Function(args, body)) =>
          call(args, body, callArgs, scope)
        case Some(_) => assert(false, "Only functions may be called")
        case None => assert(false, "TODO: case None in function call in eval")
      }
    case Return(_) =>
      assert(false, "Return Statement is not allowed in global scope")
  }

def evalValue(
    v: Value,
    scope: HashMap[String, Value]
): HashMap[String, Value] =
  assert(false, "TODO: implement evaluation of values")

def call(
    args: Seq[String],
    body: Seq[Statement],
    callArgs: Seq[Value],
    scope: HashMap[String, Value]
): HashMap[String, Value] =
  val newScope = scope.addAll(args.zip(callArgs))
  var returnScope = body.foldLeft(newScope)((acc, x) =>
    x match {
      case Assignment(name, value) =>
        val result = evalValue(value, acc)
        val newValue = result.get("new value") match {
          case Some(v) => v
          case None =>
            assert(false, "unreachable: a new value should always be returned")
        }
        val _ = result.remove("new value")
        acc.update(name, newValue)
        acc
      case FunctionCall(id, callArgsInner) =>
        acc.get(id) match {
          case Some(Function(a, b)) =>
            call(a, b, callArgsInner, acc)
          case Some(_) =>
            assert(false, "'eval function call': Only functions may be called")
          case None =>
            assert(
              false,
              f"in function call: no function with identifier '$id%s'"
            )
        }
      case Return(Identifier(name)) =>
        (acc.get("returned value"), acc.get(name)) match {
          case (None, Some(v)) => acc.addOne(("returned value", v))
          case (_, _)          => acc
        }
      case Return(value) =>
        acc.get("returned value") match {
          case None => acc.addOne(("returned value", value))
          case _    => acc
        }
    }
  )
  returnScope.subtractAll(args)
  returnScope
