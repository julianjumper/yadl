import fastparse._, NoWhitespace._

def wsSingle[$: P] = P(" " | "\t")
def ws[$: P] = P(wsSingle.rep)
def newline[$: P] = P("\n\r" | "\r" | "\n")

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

trait Operator
trait Value
trait Statement

case class ArithmaticOp(op: ArithmaticOps) extends Operator
case class CompareOp(op: CompareOps) extends Operator
case class BooleanOp(op: BooleanOps) extends Operator

case class Identifier(name: String) extends Value
case class Number(value: Double) extends Value
case class Bool(b: Boolean) extends Value
case class BinaryOp(left: Value, op: Operator, right: Value) extends Value
case class Function(args: Seq[String], body: Seq[Statement]) extends Value
case class Wrapped(value: Value) extends Value
case class StdString(value: String) extends Value
case class FormatString(value: List[Value]) extends Value

case class Assignment(varName: String, value: Value) extends Statement
class Branch(condition: Value, boby: Seq[Statement])
case class If(
    inital: Branch,
    elifs: Seq[Branch],
    end: Option[Seq[Statement]]
) extends Statement
case class WhileLoop(loop: Branch) extends Statement
case class Expression(expr: Value) extends Statement
case class Return(value: Value) extends Statement

case class FunctionCall(identifier: String, args: Seq[Value])
    extends Value,
      Statement

def condition[$: P]: P[Value] =
  P("(" ~ ws ~ valueP ~ ws ~ ")")

def initialBranch[$: P]: P[Branch] =
  P(
    "if" ~ ws ~ condition ~ ws ~ codeBlock
  ).map((v, sts) => Branch(v, sts))

def whileloop[$: P]: P[Statement] =
  P("while" ~ ws ~ condition ~ ws ~ codeBlock).map((c, cb) =>
    WhileLoop(Branch(c, cb))
  )

def elif[$: P]: P[Branch] =
  P(
    "elif" ~ ws ~ condition ~ ws ~ codeBlock
  ).map((v, sts) => Branch(v, sts))

def endBranch[$: P]: P[Seq[Statement]] =
  P("else" ~ ws ~ codeBlock)

def ifStatement[$: P]: P[Statement] =
  (initialBranch ~ ws ~ elif.rep ~ ws ~ endBranch.?).map((i, m, e) =>
    If(i, m, e)
  )

def returnP[$: P]: P[Statement] =
  P("return" ~ ws ~ valueP).map(Return(_))

def statementP[$: P]: P[Statement] =
  returnP | whileloop | ifStatement | functionCallP | assignmentP

def codeBlock[$: P]: P[Seq[Statement]] =
  P("{" ~ newline ~ (ws ~ statementP ~ ws ~ newline).rep ~ ws ~ "}")

def functionDefBodyP[$: P]: P[Seq[Statement]] =
  codeBlock | valueP.map((v) => Seq(Expression(v)))

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
  functionCallP./ | identifierP | numberP | booleanP

def booleanP[$: P]: P[Value] = P(
  ("true" | "false").!
).map {
  case "true"  => Bool(true)
  case "false" => Bool(false)
  case _       => assert(false, "unreachable")
}

def functionCallArgsP[$: P]: P[Seq[Value]] = (
  valueP ~ (ws ~ "," ~ ws ~ functionCallArgsP).?
).map((v, vs) =>
  vs match {
    case None     => Seq(v)
    case Some(xs) => v +: xs
  }
)

def functionCallP[$: P]: P[FunctionCall] = (
  identifierP.! ~ "(" ~ ws ~ functionCallArgsP.? ~ ws ~ ")"
).map((n, bs) =>
  bs match {
    case None     => FunctionCall(n, Seq())
    case Some(xs) => FunctionCall(n, xs)
  }
)

def binaryOperator[$: P]: P[Operator] =
  arihmeticOperatorP.map(ArithmaticOp(_)) | booleanOperatorP.map(
    BooleanOp(_)
  ) | compareOperatorP.map(CompareOp(_))

def valueBinaryOpP[$: P]: P[Value] = (
  (valueWrappedP | valueTerminalP./) ~ (ws ~ binaryOperator ~ ws ~ valueP).?
).map((l, rest) =>
  rest match {
    case Some((op, r)) => BinaryOp(l, op, r)
    case None          => l
  }
)

def valueWrappedP[$: P]: P[Value] =
  ("(" ~ valueP ~ ")").map(Wrapped(_))

def valueP[$: P]: P[Value] = (
  functionDefP | valueBinaryOpP | valueWrappedP | valueTerminalP./
)

def identifierP[$: P]: P[Identifier] = P(
  (CharIn("a-zA-Z") ~ CharIn("a-zA-z0-9_").rep).!.map(x => Identifier(x))
)

def assignmentP[$: P]: P[Statement] =
  (identifierP.! ~/ ws ~ "=" ~ ws ~ valueP).map((n, v) => Assignment(n, v))

def mapper(sts: Seq[Option[Statement]]): Seq[Statement] =
  sts match {
    case Some(st) :: rest => st +: mapper(rest)
    case None :: rest     => mapper(rest)
    case Seq()            => Seq()
  }

def fileP[$: P]: P[Seq[Statement]] =
  ((statementP.? ~ ws ~ newline).rep).map(mapper(_))

//Hilfsparser Number
def numberDezimalP[$: P] = P(
  CharIn("0-9") ~ (("_" ~ CharIn("0-9")) | CharIn("0-9")).rep
)
def numberBinaryP[$: P] = P(
  CharIn("01") ~ (("_" ~ CharIn("01")) | CharIn("01")).rep
)
def numberOctalP[$: P] = P(
  CharIn("0-7") ~ (("_" ~ CharIn("0-7")) | CharIn("0-7")).rep
)
def numberHexadezimal[$: P] = P(
  CharIn("0-9a-z") ~ (("_" ~ CharIn("0-9a-z")) | CharIn("0-9a-z")).rep
)

//Hilffunktion für Number map
def basisToDecimal(
    numberString: String,
    restString: String,
    basis: Int
): Double =
  println(
    "numberString: " + numberString + "; restString: " + restString + "; basis: " + basis
  )
  val numberLong =
    java.lang.Long.parseLong(numberString.replaceAll("_", ""), basis)
  val restLong = java.lang.Long
    .parseLong(restString.replaceAll("\\.", "").replaceAll("_", ""), basis)
  val resultString = numberLong.toString + "." + restLong.toString
  resultString.toDouble

//Parser Number
def numberP[$: P]: P[Number] = P(
  (
    ("0b".! ~ numberBinaryP.!.? ~ ("." ~ numberBinaryP).!.?) |
      ("0o".! ~ numberOctalP.!.? ~ ("." ~ numberOctalP).!.?) |
      ("0x".! ~ numberHexadezimal.!.? ~ ("." ~ numberHexadezimal).!.?) |
      (numberDezimalP.!.? ~ ("." ~ numberDezimalP).!.?)
  ).map(x =>
    x match {
      // Dezimal Number Cases
      case (Some(number), None) => Number(number.replaceAll("_", "").toDouble)
      case (Some(number), Some(rest)) =>
        Number((number + rest).replaceAll("_", "").toDouble)
      case (None, Some(rest)) => Number(rest.replaceAll("_", "").toDouble)
      // Binary Number Cases
      case ("0b", Some(number), None) => Number(basisToDecimal(number, ".0", 2))
      case ("0b", Some(number), Some(rest)) =>
        Number(basisToDecimal(number, rest, 2))
      case ("0b", None, Some(rest)) => Number(basisToDecimal("0", rest, 2))
      // Octal Number Cases
      case ("0o", Some(number), None) => Number(basisToDecimal(number, ".0", 8))
      case ("0o", Some(number), Some(rest)) =>
        Number(basisToDecimal(number, rest, 8))
      case ("0o", None, Some(rest)) => Number(basisToDecimal("0", rest, 8))
      // Hexadezimal Number Cases
      case ("0x", Some(number), None) =>
        Number(basisToDecimal(number, ".0", 16))
      case ("0x", Some(number), Some(rest)) =>
        Number(basisToDecimal(number, rest, 16))
      case ("0x", None, Some(rest)) => Number(basisToDecimal("0", rest, 16))
      // Default Case
      case _ => assert(false, "error occured while parsing a number")
    }
  )
)

//Hilfsparser String
def unescape(input: String): String =
  input
    .replaceAllLiterally("\\\\", "\\")
    .replaceAllLiterally("\\t", "\t")
    .replaceAllLiterally("\\b", "\b")
    .replaceAllLiterally("\\n", "\n")
    .replaceAllLiterally("\\r", "\r")
    .replaceAllLiterally("\\f", "\f")
    .replaceAllLiterally("\\\"", "\"")
    .replaceAllLiterally("\\\'", "\'")

def charForString1P[$: P] = P(!("\"" | "\\r" | "\\n") ~ AnyChar)
def charForString2P[$: P] = P(!("\'" | "\\r" | "\\n") ~ AnyChar)
def charForMultilineString1P[$: P] = P(!"\"\"\"" ~ AnyChar)
def charForMultilineString2P[$: P] = P(!"\'\'\'" ~ AnyChar)

//Parser String
//def formatStringP[$: P] = P()
def stdStringP[$: P] = P(
  (("\"\"\"" ~ charForMultilineString1P.rep.! ~ "\"\"\"") |
    ("\'\'\'" ~ charForMultilineString1P.rep.! ~ "\'\'\'") |
    ("\"" ~ charForString1P.rep.! ~ "\"") |
    ("\'" ~ charForString2P.rep.! ~ "\'")).map(x => StdString(unescape(x)))
)

def stdMultiStringP[$: P] = P(
  (("\"\"\"" ~ charForMultilineString1P.rep.! ~ "\"\"\"") ~ End |
    ("\'\'\'" ~ charForMultilineString1P.rep.! ~ "\'\'\'") ~ End).map(x =>
    StdString(unescape(x))
  )
)
