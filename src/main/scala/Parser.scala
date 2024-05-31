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
class DictionaryEntry(var key: Value, var value: Value)
case class Dictionary(entries: Seq[DictionaryEntry]) extends Value
case class StructureAccess(identifier: Identifier, key: Value) extends Value

case class Assignment(varName: String, value: Value) extends Statement
case class Branch(condition: Value, body: Seq[Statement])
case class If(
    ifBranch: Branch,
    elifBranches: Seq[Branch],
    elseBranch: Option[Seq[Statement]]
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
  (initialBranch ~ (ws ~ elif).rep ~ ws ~ endBranch.?).map((i, m, e) =>
    If(i, m, e)
  )

def returnP[$: P]: P[Statement] =
  P("return" ~ ws ~ valueP).map(Return(_))

def statementP[$: P]: P[Statement] =
  returnP | whileloop | ifStatement | functionCallP | assignmentP

def codeBlock[$: P]: P[Seq[Statement]] =
  P("{" ~ newline.? ~ (ws ~ statementP ~ ws ~ newline).rep ~ ws ~ "}")

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
  dictionaryP | structureAccess | booleanP | functionCallP | identifierP | numberP

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
  (CharIn("a-zA-Z") ~ CharIn("a-zA-z0-9_").rep).!.map(Identifier(_))
)

def assignmentP[$: P]: P[Statement] =
  (identifierP.! ~/ ws ~ "=" ~ ws ~ valueP).map((n, v) => Assignment(n, v))

def mapper(sts: Seq[Option[Statement]]): Seq[Statement] =
  sts match {
    case Some(st) :: rest => st +: mapper(rest)
    case None :: rest     => mapper(rest)
    case Seq()            => Seq()
  }

// Root rule
def fileP[$: P]: P[Seq[Statement]] =
  ((statementP.? ~ ws ~ newline).rep).map(mapper(_))
  // fastparse (the parsing library that we use) syntax:
  // This code means that we call a parser for a statement, then a parser for whitespaces, then for newlines.
  // This can be repeated any number of times (signaled by .rep). As regex: (statement whitespace* newline)*

//Hilfsparser Number
def digitsP[$: P](digitParser: => P[Char]): P[String] =
  (digitParser ~ (("_" ~ digitParser) | digitParser).rep)
    .map((f, r) => r.foldLeft(StringBuffer().append(f))(_.append(_)).toString)

def dezimalP[$: P] = P(CharIn("0-9").!).map(_.head)
def binaryP[$: P] = P(CharIn("01").!).map(_.head)
def octalP[$: P] = P(CharIn("0-7").!).map(_.head)
def hexadezimalP[$: P] = P(CharIn("0-9a-fA-F").!).map(_.head)

//Hilffunktion für Number map
def basisToDecimal(
    numberString: String,
    restString: String,
    basis: Int
): Double =
  val numberLong = java.lang.Long.parseLong(numberString, basis).toDouble
  val restLong = java.lang.Long.parseLong(restString, basis).toDouble
  val fraction = restLong / scala.math.pow(basis, restString.length)
  numberLong + fraction

//Parser Number
enum Base:
  case Binary, Octal, Decimal, Hexadecimal

def basePrefix[$: P] =
  P("0b".! | "0o".! | "0x".!).?.map {
    case Some("0b") => Base.Binary
    case Some("0o") => Base.Octal
    case Some("0x") => Base.Hexadecimal
    case None       => Base.Decimal
  }

def numberDigits[$: P](baseType: Base) = baseType match {
  case Base.Decimal     => digitsP(dezimalP)
  case Base.Octal       => digitsP(octalP)
  case Base.Binary      => digitsP(binaryP)
  case Base.Hexadecimal => digitsP(hexadezimalP)
}

def dotDigits[$: P](base: Base): P[Number] =
  ("." ~ numberDigits(base)).map(s =>
    base match
      case Base.Binary      => Number(basisToDecimal("0", s, 2))
      case Base.Octal       => Number(basisToDecimal("0", s, 8))
      case Base.Decimal     => Number(("0." + s).toDouble)
      case Base.Hexadecimal => Number(basisToDecimal("0", s, 16))
  )

def digitsDotDigits[$: P](base: Base): P[Number] =
  (numberDigits(base) ~ "." ~ numberDigits(base)).map((f, r) =>
    base match
      case Base.Binary      => Number(basisToDecimal(f, r, 2))
      case Base.Octal       => Number(basisToDecimal(f, r, 8))
      case Base.Decimal     => Number((f + "." + r).toDouble)
      case Base.Hexadecimal => Number(basisToDecimal(f, r, 16))
  )

def digits[$: P](base: Base): P[Number] =
  (numberDigits(base)).map(s =>
    base match
      case Base.Binary      => Number(basisToDecimal(s, "0", 2))
      case Base.Octal       => Number(basisToDecimal(s, "0", 8))
      case Base.Decimal     => Number(s.toDouble)
      case Base.Hexadecimal => Number(basisToDecimal(s, "0", 16))
  )

def numberFull[$: P](base: Base): P[Number] =
  dotDigits(base) | digitsDotDigits(base) | digits(base)

def numberP[$: P]: P[Number] =
  basePrefix.flatMap(numberFull)

//Hilfsparser String
def unescape(input: String): String =
  input
    .replace("\\\\", "\\")
    .replace("\\t", "\t")
    .replace("\\b", "\b")
    .replace("\\n", "\n")
    .replace("\\r", "\r")
    .replace("\\f", "\f")
    .replace("\\\"", "\"")
    .replace("\\\'", "\'")

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

// @language-team because you are indecisive of where to put the comma
// could be simpler
def dictionaryEntry[$: P]: P[DictionaryEntry] =
  (valueP ~ ws ~ ":" ~ ws ~ valueP).map(DictionaryEntry(_, _))

def dictionaryEntries[$: P]: P[Dictionary] =
  def entryCommaRight[$: P]: P[DictionaryEntry] =
    dictionaryEntry ~ ws ~ ","

  def repeatedEntries[$: P](
      entry: => P[DictionaryEntry]
  ): P[Seq[DictionaryEntry]] =
    (ws ~ entry ~/ ws ~ newline.?).rep

  (repeatedEntries(entryCommaRight) ~ ws ~ dictionaryEntry ~/ ws ~ newline.?)
    .map((es, e) => Dictionary(es :+ e))

def dictionaryP[$: P]: P[Dictionary] =
  P("{" ~ ws ~ newline.? ~ dictionaryEntries.? ~ ws ~ "}")
    .map {
      case Some(value) => value
      case None        => Dictionary(Seq())
    }

def structureAccess[$: P]: P[Value] =
  P(
    (!"[" ~ CharIn("a-zA-z0-9_")).rep.! ~ "[" ~ ws ~ valueP ~ ws ~ "]"
  )
    .map((i, v) => StructureAccess(Identifier(i), v))
