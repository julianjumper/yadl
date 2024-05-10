import fastparse._, NoWhitespace._

def wsSingle[$: P] = P(" " | "\t")
def ws[$: P] = P(wsSingle.rep)
def newline[$: P] = P("\n" | "\r" | "\n\r")

def numberP[$: P] =
  P((CharPred(_.isDigit) ~ CharPred(_.isDigit).rep).!).map(s => Number(s.toInt))

def stringP[$: P] = P("'" ~ AnyChar.rep.! ~ "'")
def stringConcatP[$: P] = P("++")

def arihmeticOperatorP[$: P] = P((CharIn("*/+") | "-").!)
def booleanOperatorP[$: P] = P(("and" | "or" | "not").!)

trait Value
case class Identifier(name: String) extends Value
case class Number(v: Integer) extends Value
case class BinaryOp(left: Value, op: String, right: Value) extends Value
case class FunctionCall(id: String, args: Seq[Value]) extends Value

def valueTerminalP[$: P]: P[Value] = P(
  valueFunctionCallP | identifierP | numberP
)

def valueFunctionCallP[$: P]: P[Value] = P(
  identifierP.! ~ ws ~ "(" ~ ws ~ valueP.rep ~ ws ~ ")"
).map((n, bs) => FunctionCall(n, bs))

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
  val Parsed.Success(v, idx) =
    parse("aoeu = 15 + f(3) - (3 * (23 + 3))", assignmentP(_))
  println(v)
