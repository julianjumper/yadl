import fastparse._, NoWhitespace._

def wsSingle[$: P] = P(" " | "\t")
def ws[$: P] = P(wsSingle.rep)
def newline[$: P] = P("\n" | "\r" | "\n\r")

def numberP[$: P] =
  P((CharPred(_.isDigit) ~ CharPred(_.isDigit).rep).!).map(s => Number(s.toInt))

def stringP[$: P] = P("'" ~ AnyChar.rep.! ~ "'")
def stringConcatP[$: P] = P("++")

enum boolOperators:
  case And, Or, Not

def arihmeticOperatorP[$: P] = P((CharIn("*/+") | "-").!)
def booleanOperatorP[$: P] = P(("and" | "or" | "not").!).map {
  case "and" => boolOperators.And
  case "or" => boolOperators.Or
  case "not" => boolOperators.Not
  case _ => assert(false, "boolean operator not defined")
}

trait Value
case class Identifier(name: String) extends Value
case class Number(v: Integer) extends Value
case class Bool(b: Boolean) extends Value
case class BinaryOp(left: Value, op: String, right: Value) extends Value
case class FunctionCall(id: String, args: Seq[Value]) extends Value

def valueTerminalP[$: P]: P[Value] = P(
  valueFunctionCallP | identifierP | numberP | booleanP
)

def booleanP[$: P]: P[Value] = P(
  ("true" | "false").!
).map {
  case "true" => Bool(true)
  case "false" => Bool(false)
  case _ => assert(false, "unreachable")
}

// TODO
def booleanBinaryOpP[$: P]: P[Value] = P(
  booleanP ~ ws ~ booleanOperatorP ~ ws ~ booleanP
).map((l, op, r) => BinaryOp(l, op, r))

def functionCallArgs[$: P]: P[Seq[Value]] = P(
  valueTerminalP ~ (ws ~ "," ~ ws ~ functionCallArgs).?
).map((v, vs) =>
  vs match {
    case None     => Seq(v)
    case Some(xs) => v +: xs
  }
)

def valueFunctionCallP[$: P]: P[Value] = P(
  identifierP.! ~ ws ~ "(" ~ ws ~ functionCallArgs.? ~ ws ~ ")"
).map((n, bs) =>
  bs match {
    case None     => FunctionCall(n, Seq())
    case Some(xs) => FunctionCall(n, xs)
  }
)

def valueBinaryOpP[$: P]: P[Value] = P(
  valueTerminalP ~ ws ~ arihmeticOperatorP ~ ws ~ valueP
).map((l, op, r) => BinaryOp(l, op, r))

def valueP[$: P]: P[Value] = P(
  "(" ~ valueP ~ ")" | valueBinaryOpP | valueTerminalP
)

def identifierStartP[$: P] = P(CharIn("a-z") | CharIn("A-Z"))
def identifierRestP[$: P] = P(
  CharIn("a-z") | CharIn("A-Z") | CharIn("0-9") | "_"
)
def identifierP[$: P]: P[Value] =
  P((identifierStartP ~ identifierRestP.rep).!).map(Identifier(_))

def assignmentP[$: P] = P(identifierP ~ ws ~ "=" ~ ws ~ valueP)

@main def hello(): Unit =
  val Parsed.Success(v, _) =
    parse("aoeu = 15 + f(3, 4) - (3 * (23 + 3))", assignmentP(_))
  println(v)
  val Parsed.Success(w, _) =
    parse("aoeu = 15 + f() - (3 * (23 + 3))", assignmentP(_))
  println(w)
