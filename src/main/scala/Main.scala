import fastparse._, NoWhitespace._

def wsSingle[$: P] = P(" " | "\t")
def ws[$: P] = P(wsSingle.rep)
def newline[$: P] = P("\n\r" | "\r" | "\n")

def numberP[$: P] =
  P((CharPred(_.isDigit) ~ CharPred(_.isDigit).rep).!).map(s => Number(s.toInt))

def stringP[$: P] = P("'" ~ AnyChar.rep.! ~ "'")
def stringConcatP[$: P] = P("++")

enum boolOperators:
  case And, Or, Not

def arihmeticOperatorP[$: P] = P((CharIn("*/+") | "-").!)
def booleanOperatorP[$: P] = P(("and" | "or" | "not").!).map {
  case "and" => boolOperators.And
  case "or"  => boolOperators.Or
  case "not" => boolOperators.Not
  case _     => assert(false, "boolean operator not defined")
}

trait Value
case class Identifier(name: String) extends Value
case class Number(v: Integer) extends Value
case class Bool(b: Boolean) extends Value
case class BinaryOp(left: Value, op: String, right: Value) extends Value
case class Function(args: Seq[String], body: Seq[Statement]) extends Value
case class FunctionCall(id: String, args: Seq[Value]) extends Value, Statement

trait Statement
case class Assignment(varName: String, value: Value) extends Statement
case class Return(value: Value) extends Statement

def statementP[$: P]: P[Statement] = P(
  assignmentP | valueFunctionCallP | ("return" ~ ws ~ valueP).map(Return(_))
)

def functionDefBodyP[$: P]: P[Seq[Statement]] = P(
  "{" ~ newline ~ (ws ~ statementP ~ ws ~ newline).rep ~ "}" | statementP.map(
    Seq(_)
  )
)
def functionDefArgsP[$: P]: P[Seq[String]] = P(
  identifierP ~ (ws ~ "," ~ ws ~ functionDefArgsP).?
).map((i, is) =>
  (i, is) match {
    case (Identifier(n), Some(args)) => n +: args
    case (Identifier(n), None)       => Seq(n)
  }
)

def functionDefP[$: P]: P[Value] = P(
  "(" ~ ws ~ functionDefArgsP.? ~ ws ~ ")" ~ ws ~ "=>" ~ ws ~ functionDefBodyP
).map((bs, b) =>
  bs match {
    case Some(args) => Function(args, b)
    case None       => Function(Seq(), b)
  }
)

def valueTerminalP[$: P]: P[Value] = P(
  valueFunctionCallP | identifierP | numberP | booleanP
)

def booleanP[$: P]: P[Value] = P(
  ("true" | "false").!
).map {
  case "true"  => Bool(true)
  case "false" => Bool(false)
  case _       => assert(false, "unreachable")
}

// TODO
def booleanBinaryOpP[$: P]: P[Value] = P(
  booleanP ~ ws ~ booleanOperatorP ~ ws ~ booleanP
).map((l, op, r) => BinaryOp(l, op, r))

def functionCallArgsP[$: P]: P[Seq[Value]] = P(
  valueTerminalP ~ (ws ~ "," ~ ws ~ functionCallArgsP).?
).map((v, vs) =>
  vs match {
    case None     => Seq(v)
    case Some(xs) => v +: xs
  }
)

def valueFunctionCallP[$: P]: P[FunctionCall] = P(
  identifierP.! ~ ws ~ "(" ~ ws ~ functionCallArgsP.? ~ ws ~ ")"
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

def assignmentP[$: P]: P[Statement] =
  P(identifierP.! ~ ws ~ "=" ~ ws ~ valueP).map((n, v) => Assignment(n, v))

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
