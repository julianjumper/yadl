import fastparse._, NoWhitespace._

type HashMap[K, V] = scala.collection.mutable.HashMap[K, V]

def wsSingle[$: P] = P(" " | "\t")
def ws[$: P] = P(wsSingle.rep)
def newline[$: P] = P("\n\r" | "\r" | "\n")

def numberP[$: P] =
  P((CharPred(_.isDigit) ~ CharPred(_.isDigit).rep).!).map(s => Number(s.toInt))

def stringP[$: P] = P("'" ~ AnyChar.rep.! ~ "'")
def stringConcatP[$: P] = P("++")

enum boolOperators:
  case And, Or, Not

def arihmeticOperatorP[$: P] = P((CharIn("*/+^") | "-").!)
def booleanOperatorP[$: P] = P(("and" | "or" | "not").!).map {
  case "and" => boolOperators.And
  case "or"  => boolOperators.Or
  case "not" => boolOperators.Not
  case _     => assert(false, "boolean operator not defined")
}

trait Value
trait Statement

case class Identifier(name: String) extends Value
case class Number(v: Integer) extends Value
case class Bool(b: Boolean) extends Value
case class BinaryOp(left: Value, op: String, right: Value) extends Value
case class BoolBinaryOp(left: Value, op: boolOperators, right: Value)
    extends Value
case class Function(args: Seq[String], body: Seq[Statement]) extends Value

case class Assignment(varName: String, value: Value) extends Statement
case class Return(value: Value) extends Statement

def eval(st: Statement, scope: HashMap[String, Value]): HashMap[String, Value] =
  st match {
    case Assignment(name, value) => scope.addOne((name, value))
    case FunctionCall(identifier, callArgs) =>
      scope.get(identifier) match {
        case Some(Function(a, b)) =>
          val fun = Function(a, b)
          evalFunctionCall(fun, callArgs, scope)
        case Some(_) => assert(false, "Only functions may be called")
        case None    => assert(false, "TODO: function call in eval")
      }
    case Return(_) =>
      assert(false, "Return Statement is not allowed in global scope")
  }

def evalFunctionCall(
    fun: Function,
    callArgs: Seq[Value],
    scope: HashMap[String, Value]
): HashMap[String, Value] =
  val Function(args, body) = fun
  val newScope = scope.addAll(args.zip(callArgs))
  body.foldLeft(newScope)((acc, x) =>
    x match {
      case Assignment(name, value) => acc.addOne((name, value))
      case FunctionCall(id, callArgsInner) =>
        acc.get(id) match {
          case Some(Function(a, b)) =>
            val f = Function(a, b)
            evalFunctionCall(f, callArgsInner, newScope)
          case Some(_) =>
            assert(false, "'eval function call': Only functions may be called")
          case None =>
            assert(
              false,
              "'eval function call': no function with identifier '$identifier%s'"
            )
        }
      case Return(Identifier(name)) =>
        (newScope.get("returned value"), newScope.get(name)) match {
          case (None, Some(v)) => newScope.addOne(("returned value", v))
          case (_, _)          => newScope
        }
    }
  )

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

def valueP[$: P]: P[Value] = (
  "(" ~ valueP ~ ")" | valueBinaryOpP | valueTerminalP
)

def identifierStartP[$: P] = P(CharIn("a-z") | CharIn("A-Z"))
def identifierRestP[$: P] = P(
  CharIn("a-z") | CharIn("A-Z") | CharIn("0-9") | "_"
)
def identifierP[$: P]: P[Value] =
  ((identifierStartP ~ identifierRestP.rep).!).map(Identifier(_))

def assignmentP[$: P]: P[Statement] =
  (identifierP.! ~ ws ~ "=" ~ ws ~ valueP).map((n, v) => Assignment(n, v))

@main def hello(): Unit =
  val Parsed.Success(v, _) =
    parse(
      "aoeu = 15 + f(3, 4) - (3 * (23 + 3))",
      assignmentP(using _)
    ): @unchecked
  println(v)

  val Parsed.Success(w, _) =
    parse("aoeu = 15 + f() - (3 * (23 + 3))", assignmentP(using _)): @unchecked
  println(w)

  val Parsed.Success(f, _) =
    parse(
      "(x, y) => {\n  aoeu = 15 + 4\n  return aoeu\n}",
      functionDefP(using _)
    ): @unchecked
  println(f)
