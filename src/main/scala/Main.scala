import fastparse._, NoWhitespace._

type HashMap[K, V] = scala.collection.mutable.HashMap[K, V]

def wsSingle[$: P] = P(" " | "\t")
def ws[$: P] = P(wsSingle.rep)
def newline[$: P] = P("\n\r" | "\r" | "\n")

def numberP[$: P] =
  P((CharPred(_.isDigit) ~ CharPred(_.isDigit).rep).!).map(s => Number(s.toInt))

def stringP[$: P] = P("'" ~ AnyChar.rep.! ~ "'")
def stringConcatP[$: P] = P("++")

enum BooleanOps:
  case And, Or, Not

enum CompareOps:
  case Less, LessEq, Greater, GreaterEq, Eq, NotEq

enum ArithmaticOps:
  case Add, Sub, Mul, Div, Expo

def arihmeticOperatorP[$: P] = P((CharIn("*/+^") | "-").!).map {
  case "*" => ArithmaticOps.Mul
  case "+" => ArithmaticOps.Add
  case "-" => ArithmaticOps.Sub
  case "/" => ArithmaticOps.Div
  case "^" => ArithmaticOps.Expo
  case _   => assert(false, "arithmatic operator not defined")
}

def booleanOperatorP[$: P] = P(("and" | "or" | "not").!).map {
  case "and" => BooleanOps.And
  case "or"  => BooleanOps.Or
  case "not" => BooleanOps.Not
  case _     => assert(false, "boolean operator not defined")
}

def compareOperatorP[$: P] = P(("==" | "!=" | "<=" | ">=" | "<" | ">").!).map {
  case "<"  => CompareOps.Less
  case "<=" => CompareOps.LessEq
  case ">"  => CompareOps.Greater
  case ">=" => CompareOps.GreaterEq
  case "==" => CompareOps.Eq
  case "!=" => CompareOps.NotEq
  case _    => assert(false, "comparision operator not defined")
}

trait Value
trait Statement

case class Identifier(name: String) extends Value
case class Number(value: Integer) extends Value
case class Bool(b: Boolean) extends Value
case class BinaryOp(left: Value, op: ArithmaticOps, right: Value) extends Value
case class BoolBinaryOp(left: Value, op: BooleanOps, right: Value) extends Value
case class BinaryCompareOp(left: Value, op: CompareOps, right: Value)
    extends Value
case class Function(args: Seq[String], body: Seq[Statement]) extends Value
case class Wrapped(value: Value) extends Value

case class Assignment(varName: String, value: Value) extends Statement
case class Return(value: Value) extends Statement

case class FunctionCall(identifier: String, args: Seq[Value])
    extends Value,
      Statement

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

def statementP[$: P]: P[Statement] =
  assignmentP | valueFunctionCallP | ("return" ~ ws ~ valueP).map(Return(_))

def functionDefBodyP[$: P]: P[Seq[Statement]] = (
  "{" ~ newline ~ (ws ~ statementP ~ ws ~ newline).rep ~ "}" | statementP.map(
    Seq(_)
  )
)

def functionDefArgsP[$: P]: P[Seq[String]] = (
  identifierP ~ (ws ~ "," ~ ws ~ functionDefArgsP).?
).map((i, is) =>
  (i, is) match {
    case (Identifier(n), Some(args)) => n +: args
    case (Identifier(n), None)       => Seq(n)
  }
)

def functionDefP[$: P]: P[Value] = (
  "(" ~ ws ~ functionDefArgsP.? ~ ws ~ ")" ~ ws ~ "=>" ~ ws ~ functionDefBodyP
).map((bs, b) =>
  bs match {
    case Some(args) => Function(args, b)
    case None       => Function(Seq(), b)
  }
)

def valueTerminalP[$: P]: P[Value] =
  valueFunctionCallP | identifierP | numberP | booleanP

def booleanP[$: P]: P[Value] = P(
  ("true" | "false").!
).map {
  case "true"  => Bool(true)
  case "false" => Bool(false)
  case _       => assert(false, "unreachable")
}

// TODO: complete parsing of binary boolean operations
def booleanBinaryOpP[$: P]: P[Value] = P(
  booleanP ~ ws ~ booleanOperatorP ~ ws ~ booleanP
).map((l, op, r) => BoolBinaryOp(l, op, r))

def functionCallArgsP[$: P]: P[Seq[Value]] = (
  valueTerminalP ~ (ws ~ "," ~ ws ~ functionCallArgsP).?
).map((v, vs) =>
  vs match {
    case None     => Seq(v)
    case Some(xs) => v +: xs
  }
)

def valueFunctionCallP[$: P]: P[FunctionCall] = (
  identifierP.! ~ ws ~ "(" ~ ws ~ functionCallArgsP.? ~ ws ~ ")"
).map((n, bs) =>
  bs match {
    case None     => FunctionCall(n, Seq())
    case Some(xs) => FunctionCall(n, xs)
  }
)

def valueBinaryOpP[$: P]: P[Value] = (
  valueTerminalP ~ ws ~ arihmeticOperatorP ~ ws ~ valueP
).map((l, op, r) => BinaryOp(l, op, r))

def valueWrappedP[$: P]: P[Value] =
  ("(" ~ valueP ~ ")").map(Wrapped(_))

def valueP[$: P]: P[Value] = (
  valueWrappedP | valueBinaryOpP | valueTerminalP
)

def identifierStartP[$: P] = P(CharIn("a-z") | CharIn("A-Z"))
def identifierRestP[$: P] = P(
  CharIn("a-z") | CharIn("A-Z") | CharIn("0-9") | "_"
)
def identifierP[$: P]: P[Value] =
  ((identifierStartP ~ identifierRestP.rep).!).map(Identifier(_))

def assignmentP[$: P]: P[Statement] =
  (identifierP.! ~ ws ~ "=" ~ ws ~ valueP).map((n, v) => Assignment(n, v))

def mapper(sts: Seq[Option[Statement]]): Seq[Statement] =
  sts match {
    case Some(st) :: rest  => st +: mapper(rest)
    case None :: rest      => mapper(rest)
    case Some(st) :: Seq() => Seq(st)
  }

def fileP[$: P]: P[Seq[Statement]] =
  ((statementP.? ~ ws ~ newline).rep ~ End).map(mapper(_))

def precedence(op: ArithmaticOps) = op match {
  case ArithmaticOps.Add  => 4
  case ArithmaticOps.Sub  => 4
  case ArithmaticOps.Mul  => 5
  case ArithmaticOps.Div  => 5
  case ArithmaticOps.Expo => 6
}

def precedence(op: BooleanOps) = op match {
  case BooleanOps.And => 2
  case BooleanOps.Or  => 1
  case BooleanOps.Not => 3
}

def precedence(_n: CompareOps) = 0

def precedenceOf(value: Value) = value match {
  case BinaryOp(_, op, _)        => precedence(op)
  case BoolBinaryOp(_, op, _)    => precedence(op)
  case BinaryCompareOp(_, op, _) => precedence(op)
  case _                         => 100000
}

object Main {
  def main(args: Array[String]): Unit =
    for (f <- args)
      val content = java.io.FileInputStream(f)
      val result = parse(content, fileP)
      result match {
        case Parsed.Success(v, _) => println(v)
        case v                    => println(v)
      }
}
