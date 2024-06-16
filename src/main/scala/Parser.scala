package parser

import fastparse._, NoWhitespace._

def wsSingle[$: P] = P(" " | "\t")
def ws[$: P] = P((multilineCommentP | wsSingle).rep)
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
case class Number(value: Double) extends Value:
  override def toString(): String =
    if (value - value.toInt == 0) value.toInt.toString
    else value.toString

case class Bool(b: Boolean) extends Value:
  override def toString(): String = b.toString

case class BinaryOp(left: Value, op: Operator, right: Value) extends Value
case class Function(args: Seq[String], body: Seq[Statement]) extends Value
case class Wrapped(value: Value) extends Value
case class StdString(value: String) extends Value:
  override def toString(): String = value.toString

case class FormatString(value: List[Value]) extends Value
class DictionaryEntry(var key: Value, var value: Value):
  override def toString(): String = key.toString + ": " + value.toString

case class Dictionary(entries: Seq[DictionaryEntry]) extends Value:
  override def toString(): String =
    "{" + entries.map { _.toString }.mkString(", ") + "}"

case class ArrayLiteral(elements: Seq[Value]) extends Value
case class ArrayAccess(array: String, index: Value) extends Value

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

case class FunctionCall(functionExpr: Value, args: Seq[Value])
    extends Value,
      Statement

def condition[$: P]: P[Value] =
  P("(" ~ ws ~ expression ~ ws ~ ")")

def initialBranch[$: P]: P[Branch] =
  P(
    "if" ~ ws ~ condition ~ ws ~ codeBlock
  ).map(Branch(_, _))

def whileLoop[$: P]: P[Statement] =
  P("while" ~ ws ~ condition ~ ws ~ codeBlock).map((c, sts) =>
    WhileLoop(Branch(c, sts))
  )

def elif[$: P]: P[Branch] =
  P(
    "elif" ~ ws ~ condition ~ ws ~ codeBlock
  ).map(Branch(_, _))

def endBranch[$: P]: P[Seq[Statement]] =
  P("else" ~ ws ~ codeBlock)

def ifStatement[$: P]: P[Statement] =
  (initialBranch ~ (ws ~ elif).rep ~ ws ~ endBranch.?).map(If(_, _, _))

def returnP[$: P]: P[Statement] =
  P("return" ~ ws ~ expression).map(Return(_))

def statementP[$: P]: P[Statement] =
  returnP | whileLoop | ifStatement | functionCallP | assignmentP

def codeBlock[$: P]: P[Seq[Statement]] =
  P("{" ~ newline.? ~ (ws ~ statementP ~ ws ~ newline).rep ~ ws ~ "}")

def functionDefBodyP[$: P]: P[Seq[Statement]] =
  codeBlock | expression.map((v) => Seq(Expression(v)))

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

def valueP[$: P]: P[Value] =
  dictionaryP | structureAccess | booleanP | functionCallP | identifierP | numberP | arrayLiteralP | arrayAccessP

def booleanP[$: P]: P[Value] = P(
  ("true" | "false").!
).map {
  case "true"  => Bool(true)
  case "false" => Bool(false)
  case _       => assert(false, "unreachable")
}

def functionCallArgsP[$: P]: P[Seq[Value]] = (
  expression ~ (ws ~ "," ~ ws ~ functionCallArgsP).?
).map((v, vs) =>
  vs match {
    case None     => Seq(v)
    case Some(xs) => v +: xs
  }
)

def functionName[$: P]: P[Value] =
  identifierP | wrappedExpression

def functionCallP[$: P]: P[FunctionCall] = (
  functionName ~ "(" ~ ws ~ functionCallArgsP.? ~ ws ~ ")"
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

// Some operators should be calculated before other operators.
// eg. 4 - 4 * 4 => 4*4 gets calculated before 4-4.
// So the "precedence" of * is higher than of -. This is handled here.
def precedence(op: ArithmaticOps) = op match {
  case ArithmaticOps.Add  => 4
  case ArithmaticOps.Sub  => 4
  case ArithmaticOps.Mul  => 5
  case ArithmaticOps.Div  => 5
  case ArithmaticOps.Expo => 7
}

def precedence(op: BooleanOps) = op match {
  case BooleanOps.And => 2
  case BooleanOps.Or  => 1
  case BooleanOps.Not => 3
}

def precedenceOf(value: BinaryOp): Int = value match {
  case BinaryOp(_, ArithmaticOp(op), _) => precedence(op)
  case BinaryOp(_, BooleanOp(op), _)    => precedence(op)
  case BinaryOp(_, CompareOp(_), _)     => 0
}

def orderBy(binOp: BinaryOp, pred: BinaryOp => Int): BinaryOp =
  val BinaryOp(left, op, right) = binOp
  right match {
    case b: BinaryOp =>
      if (pred(binOp) >= pred(b))
        val inner = orderBy(BinaryOp(left, op, b.left), pred)
        BinaryOp(inner, b.op, b.right)
      else binOp
    case _ => binOp
  }

def binaryOpExpression[$: P]: P[Value] = (
  (wrappedExpression | valueP./) ~ (ws ~ binaryOperator ~ ws ~ expression).?
).map((l, rest) =>
  rest match {
    case Some((op, r)) =>
      orderBy(BinaryOp(l, op, r), precedenceOf)
    case None => l
  }
)

def wrappedExpression[$: P]: P[Value] =
  ("(" ~ expression ~ ")").map(Wrapped(_))

def expression[$: P]: P[Value] = (
  functionDefP | binaryOpExpression
)

def identifierP[$: P]: P[Identifier] = P(
  (CharIn("a-zA-Z") ~ CharIn("a-zA-z0-9_").rep).!.map(Identifier(_))
)

def assignmentP[$: P]: P[Statement] =
  (identifierP.! ~/ ws ~ "=" ~ ws ~ expression).map((n, v) => Assignment(n, v))

def inlineTextP[$: P]: P[Unit] = P(!newline ~ AnyChar).rep
def inlineCommentP[$: P]: P[Unit] = P("//" ~ inlineTextP ~ newline)

def multilineCommentP[$: P]: P[Unit] = P("/*" ~ (!("*/") ~ AnyChar).rep ~ "*/")

def commentP[$: P] = P(inlineCommentP | multilineCommentP)

// Root rule
def yadlParser[$: P]: P[Seq[Statement]] =
  ((statementP.? ~ ws ~ (commentP | newline))).rep.map(l =>
    l.map(_.toList).flatten
  )
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

def dictionaryEntries[$: P]: P[Dictionary] =
  def dictionaryEntry[$: P]: P[DictionaryEntry] =
    (valueP ~ ws ~ ":" ~ ws ~ valueP).map(DictionaryEntry(_, _))

  def repeatedEntries[$: P](
      entry: => P[DictionaryEntry]
  ): P[Seq[DictionaryEntry]] =
    P((ws ~ entry).rep(sep = (ws ~ "," ~ ws ~ newline.?)))

  (ws ~ repeatedEntries(dictionaryEntry) ~/ ws ~ newline.?)
    .map(Dictionary(_))

def dictionaryP[$: P]: P[Dictionary] =
  P("{" ~ ws ~ newline.? ~ dictionaryEntries ~ ws ~ "}")

def structureAccess[$: P]: P[Value] =
  P(
    (!"[" ~ CharIn("a-zA-z0-9_")).rep(min = 1).! ~ "[" ~ ws ~ valueP ~ ws ~ "]"
  )
    .map((i, v) => StructureAccess(Identifier(i), v))


//Parser Array
def arrayLiteralP[$: P]: P[ArrayLiteral] =
  P("[" ~ ws ~ valueP.rep(sep = ws ~ "," ~ ws) ~ ws ~ "]").map(ArrayLiteral.apply)

def arrayAccessP[$: P]: P[ArrayAccess] =
  P(identifierP.! ~ ws ~ "[" ~ ws ~ valueP ~ ws ~ "]").map {
    
    case (array, index) => ArrayAccess(array, index)
  }
