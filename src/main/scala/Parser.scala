package parser

import fastparse._, NoWhitespace._

def wsSingle[$: P] = P(" " | "\t")
def ws[$: P] = P((multilineCommentP | wsSingle).rep).opaque("<whitespace>")
def newline[$: P] = P("\n\r" | "\r" | "\n").opaque("<newline>")

enum DataFormats:
  case characters, commas, lines, csv, json

enum BooleanOps:
  case And, Or, Not

enum CompareOps:
  case Less, LessEq, Greater, GreaterEq, Eq, NotEq

enum ArithmaticOps:
  case Add, Sub, Mul, Div, Expo, Mod

def arihmeticOperatorP[$: P] = P((CharIn("*/+^%") | "-").!).map {
  case "*" => ArithmaticOps.Mul
  case "+" => ArithmaticOps.Add
  case "-" => ArithmaticOps.Sub
  case "/" => ArithmaticOps.Div
  case "^" => ArithmaticOps.Expo
  case "%" => ArithmaticOps.Mod
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

case class NoneValue() extends Value:
  override def toString(): String =
    "none"
case class Identifier(name: String) extends Value
case class Number(value: Double) extends Value:
  override def toString(): String =
    if (value - value.toInt == 0) value.toInt.toString
    else value.toString

case class Bool(b: Boolean) extends Value:
  override def toString(): String = b.toString

case class BinaryOp(left: Value, op: Operator, right: Value) extends Value
case class UnaryOp(op: Operator, operant: Value) extends Value
case class Function(args: Seq[String], body: Seq[Statement]) extends Value
case class Load(filename: Value, dataformat: DataFormats) extends Value
case class Wrapped(value: Value) extends Value
case class StdString(value: String) extends Value:
  override def toString(): String = value.toString

case class FormatString(value: List[Value]) extends Value
class DictionaryEntry(var key: Value, var value: Value):
  override def toString(): String = key.toString + ": " + value.toString

case class Dictionary(val entries: Seq[DictionaryEntry]) extends Value:
  override def toString(): String =
    "{" + entries.mkString(", ") + "}"

case class ArrayLiteral(val elements: Seq[Value]) extends Value:
  override def toString(): String =
    "(" + elements.mkString(", ") + ")"

case class StructureAccess(identifier: Value, key: Value) extends Value

case class Assignment(varName: String, value: Value) extends Statement
case class StructuredAssignment(struct: StructureAccess, value: Value)
    extends Statement
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
  P("(" ~ ws ~ expression ~ ws ~ ")").opaque("<condition>")

def initialBranch[$: P]: P[Branch] =
  P(
    "if" ~ ws ~ condition ~ ws ~ codeBlock
  ).map(Branch(_, _))

def whileLoop[$: P]: P[Statement] =
  P("while" ~ ws ~ condition ~ ws ~ codeBlock)
    .opaque("<while loop>")
    .map((c, sts) => WhileLoop(Branch(c, sts)))

def elif[$: P]: P[Branch] =
  P(
    "elif" ~ ws ~ condition ~ ws ~ codeBlock
  ).map(Branch(_, _))

def endBranch[$: P]: P[Seq[Statement]] =
  P("else" ~ ws ~ codeBlock)

def ifStatement[$: P]: P[Statement] =
  (initialBranch ~ newline.? ~ (ws ~ elif ~ newline.?).rep ~ ws ~ (newline.? ~ ws ~ endBranch).?)
    .map(If(_, _, _))

def returnP[$: P]: P[Statement] =
  P("return" ~ ws ~ expression).map(Return(_))

def statementP[$: P]: P[Statement] =
  returnP | whileLoop | ifStatement | functionCallStatement | structuredAssignmentP | assignmentP

def codeBlock[$: P]: P[Seq[Statement]] =
  P("{" ~ newline ~ (ws ~ statementP.? ~ ws ~ newline).rep ~ ws ~ "}").map(l =>
    l.map(_.toList).flatten
  )

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

def noneP[$: P]: P[Value] = P("none").!.map(_ => NoneValue())

def valueP[$: P]: P[Value] =
  noneP | booleanP | stringP | unaryOpExpression | dictionaryP | arrayLiteralP | structureAccess | functionCallValue | numberP

def booleanP[$: P]: P[Value] = P(
  ("true" | "false").!
).opaque("<boolean value>").map {
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

def functionCallManyArgs[$: P]: P[Seq[Seq[Value]]] =
  P(ws ~ "(" ~ ws ~ functionCallArgsP.? ~ ws ~ ")")
    .map(_.getOrElse(Seq()))
    .rep

def functionCallValue[$: P]: P[Value] = P(
  functionName ~ functionCallManyArgs
) // .opaque("<function call>")
  .map((n, bs) =>
    bs.length match {
      case 0 =>
        n
      case _ =>
        bs.foldLeft(n)(FunctionCall(_, _)) match {
          case fc: FunctionCall => fc
        }
    }
  )

def functionCallStatement[$: P]: P[Statement] = P(
  functionName ~ functionCallManyArgs
).opaque("<function call>")
  .filter((_, bs) => bs.length > 0)
  .map((n, bs) =>
    bs.foldLeft(n)(FunctionCall(_, _)) match {
      case fc: FunctionCall => fc
      case _                => assert(false, "unreachable")
    }
  )

def binaryOperator[$: P]: P[Operator] =
  arihmeticOperatorP.map(ArithmaticOp(_)) | booleanOperatorP.map(
    BooleanOp(_)
  ) | compareOperatorP.map(CompareOp(_))

def unaryOperator[$: P]: P[Operator] =
  import ArithmaticOps._, BooleanOps._, CompareOps._
  import ArithmaticOp as A, BooleanOp as B, CompareOp as C

  def eq(left: Operator, right: Operator): Boolean = (left, right) match {
    case (l: A, r: A) => l.op == r.op
    case (l: B, r: B) => l.op == r.op
    case (l: C, r: C) => l.op == r.op
    case _            => false
  }

  def isAnyOf(op: Operator, targets: Seq[Operator]): Boolean =
    targets.foldLeft(false)((acc: Boolean, x: Operator) => acc || eq(x, op))

  val unaryOps = Seq(A(Add), A(Sub), B(Not))

  binaryOperator.filter { isAnyOf(_, unaryOps) }

// Some operators should be calculated before other operators.
// eg. 4 - 4 * 4 => 4*4 gets calculated before 4-4.
// So the "precedence" of * is higher than of -. This is handled here.
enum OperatorContext:
  case Binary, Unary

def precedence(op: ArithmaticOps, ctxt: OperatorContext) = op match {
  case ArithmaticOps.Add =>
    if (ctxt == OperatorContext.Binary) 4
    else 6
  case ArithmaticOps.Sub =>
    if (ctxt == OperatorContext.Binary) 4
    else 6
  case ArithmaticOps.Mul  => 5
  case ArithmaticOps.Div  => 5
  case ArithmaticOps.Mod  => 5
  case ArithmaticOps.Expo => 7
}

def precedence(op: BooleanOps, ctxt: OperatorContext) = op match {
  case BooleanOps.And => 2
  case BooleanOps.Or  => 1
  case BooleanOps.Not =>
    if (ctxt == OperatorContext.Unary) 3
    else assert(false, "Unary 'Not' in Binary context")
}

def precedenceOf(value: Value): Int = value match {
  case BinaryOp(_, ArithmaticOp(op), _) =>
    precedence(op, OperatorContext.Binary)
  case BinaryOp(_, BooleanOp(op), _) =>
    precedence(op, OperatorContext.Binary)
  case BinaryOp(_, CompareOp(_), _)          => 0
  case UnaryOp(BooleanOp(BooleanOps.Not), _) => 3
  case UnaryOp(op: ArithmaticOp, _) =>
    precedence(op.op, OperatorContext.Unary)
  case UnaryOp(_, _) => 0
}

def orderBy(opExpr: BinaryOp | UnaryOp, pred: Value => Int): Value =
  opExpr match {
    case BinaryOp(left, op, right) =>
      right match {
        case b: BinaryOp =>
          if (pred(opExpr) >= pred(b))
            val inner = orderBy(BinaryOp(left, op, b.left), pred)
            BinaryOp(inner, b.op, b.right)
          else opExpr
        case _ => opExpr
      }

    case UnaryOp(op, value) =>
      value match {
        case b: BinaryOp =>
          if (pred(opExpr) >= pred(b))
            val inner = orderBy(UnaryOp(op, b.left), pred)
            BinaryOp(inner, b.op, b.right)
          else opExpr
        case _ => opExpr
      }
  }

def unaryOpExpression[$: P]: P[Value] =
  (unaryOperator ~ ws ~ expression)
    .opaque("<unary operator>")
    .map((op, value) => orderBy(UnaryOp(op, value), precedenceOf))

def binaryOpExpression[$: P]: P[Value] = (
  valueP ~/ (ws ~ binaryOperator ~ ws ~ expression).?
).map((l, rest) =>
  rest match {
    case Some((op, r)) =>
      orderBy(BinaryOp(l, op, r), precedenceOf)
    case None => l
  }
)

def dataFormatsP[$: P]: P[DataFormats] =
  P("characters" | "commas" | "lines" | "csv" | "json").!.map {
    case "characters" => DataFormats.characters
    case "commas"     => DataFormats.commas
    case "lines"      => DataFormats.lines
    case "csv"        => DataFormats.csv
    case "json"       => DataFormats.json
    case _            => assert(false, "Unexpected file format.")
  }

def loadP[$: P]: P[Value] =
  (P("load") ~ ws ~ (stdStringP | identifierP) ~ ws ~ P(
    "as"
  ) ~ ws ~ dataFormatsP).map((file, format) => Load(file, format))

def wrappedExpression[$: P]: P[Value] =
  P("(" ~ ws ~ expression ~ ws ~ ")").map(Wrapped(_))

def expression[$: P]: P[Value] = (
  loadP | functionDefP | binaryOpExpression
)

def identifierP[$: P]: P[Identifier] = P(
  (CharIn("a-zA-Z") ~ CharIn("a-zA-z0-9_").rep).!.map(Identifier(_))
).opaque("<identifier>")

def assignmentP[$: P]: P[Statement] =
  (identifierP.! ~/ ws ~ "=" ~ ws ~ expression).map((n, v) => Assignment(n, v))

def structuredAssignmentP[$: P]: P[Statement] =
  (structureAccess ~/ ws ~ "=" ~ ws ~ expression).map((n, v) =>
    assert(n.isInstanceOf[StructureAccess])
    StructuredAssignment(n.asInstanceOf[StructureAccess], v)
  )

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
  basePrefix.flatMap(numberFull).opaque("<number>")

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

def charForStringDoubleQuote[$: P] = P(
  !("\"" | newline) ~ ("\\\"" | "\\\\" | AnyChar)
)
def charForStringSingleQuote[$: P] = P(
  !("\'" | newline) ~ ("\\\'" | "\\\\" | AnyChar)
)
def charForMultilineStringDoubleQuote[$: P] = P(!"\"\"\"" ~ ("\\\\" | AnyChar))
def charForMultilineStringSingleQuote[$: P] = P(!"\'\'\'" ~ ("\\\\" | AnyChar))

def expressionEnd[$: P] = P(ws ~ expression ~ ws ~ End)

def formatStringMap(input: String): FormatString = {
  // Replace all occurrences of "\\{" with newline "\n"
  val replacedInput = input.replace("\\{", "\n").replace("\\}", "\r")

  var result: List[Value] = List()

  var next_input = ""
  var braces_open = false
  for (a <- replacedInput) {
    if (a == '{') {
      if (braces_open == false) {
        braces_open = true
        result = result :+ StdString(
          unescape(next_input.replace("\n", "{").replace("\r", "}"))
        )
        next_input = ""
      } else assert(false, "Braces opened before old one closed")
    } else if (a == '}') {
      if (braces_open == false)
        assert(false, "Braces closed without being open")
      else {
        braces_open = false
        parse(next_input, expressionEnd(_)) match {
          case Parsed.Success(ident, _) => result = result :+ ident
          case _                        => assert(false, "parsing failed")
        }
        // val Parsed.Success(ident, _) = parse(next_input, expressionEnd(_))
        // result = result :+ ident
        next_input = ""
      }
    } else {
      next_input += a
    }
  }
  if (braces_open == true) assert(false, "Braces not closed")
  if (next_input != "") {
    result = result :+ StdString(
      unescape(next_input.replace("\n", "{").replace("\r", "}"))
    )
  }

  FormatString(result)
}

//Parser String
//def formatStringP[$: P] = P()
def stdStringP[$: P] = P(
  (("\"" ~ charForStringDoubleQuote.rep.! ~ "\"") |
    ("\'" ~ charForStringSingleQuote.rep.! ~ "\'")).map(x =>
    StdString(unescape(x))
  )
)

def stdMultiStringP[$: P] = P(
  (("\"\"\"" ~ charForMultilineStringDoubleQuote.rep.! ~ "\"\"\"") |
    ("\'\'\'" ~ charForMultilineStringSingleQuote.rep.! ~ "\'\'\'")).map(x =>
    StdString(unescape(x))
  )
)

def formatStringP[$: P]: P[FormatString] = P(
  (
    ("f\"" ~ charForStringDoubleQuote.rep.! ~ "\"") |
      ("f\'" ~ charForStringSingleQuote.rep.! ~ "\'")
  ).map(formatStringMap)
)

def stringP[$: P]: P[Value] = formatStringP | stdMultiStringP | stdStringP

// @language-team because you are indecisive of where to put the comma
// could be simpler

def dictionaryEntries[$: P]: P[Dictionary] =
  def dictionaryEntry[$: P]: P[DictionaryEntry] =
    (valueP ~ ws ~ ":" ~ ws ~ valueP)
      .opaque("<dictionary entry>")
      .map(DictionaryEntry(_, _))

  def repeatedEntries[$: P](
      entry: => P[DictionaryEntry]
  ): P[Seq[DictionaryEntry]] =
    P((ws ~ entry).rep(sep = (ws ~ "," ~ ws ~ newline.?)))

  (ws ~ repeatedEntries(dictionaryEntry) ~/ ws ~ newline.?)
    .map(Dictionary(_))

def dictionaryP[$: P]: P[Dictionary] =
  P("{" ~ ws ~ newline.? ~ dictionaryEntries ~ ws ~ "}")

def structureAccess[$: P]: P[Value] =
  def openIndex[$: P]: P[Unit] =
    P(CharPred(_ == '[')).opaque("<open index>")
  def closeIndex[$: P]: P[Unit] =
    P(CharPred(_ == ']')).opaque("<close index>")

  def access[$: P]: P[Value] =
    P(openIndex ~ ws ~ expression ~ ws ~ closeIndex)
  def internal[$: P]: P[Value] =
    P(!openIndex ~ CharIn("a-zA-z0-9_"))
      .rep(min = 1)
      .!
      .filter(s => !(s(0) == '_' || s(0).isDigit))
      .map(Identifier(_))
  P(internal ~ (ws ~ access).rep(min = 1))
    .map((i, v) => v.foldLeft(i)((acc, a) => StructureAccess(acc, a)))

//Parser Array (we use structureAccess for accessing arrays)
def arrayLiteralP[$: P]: P[ArrayLiteral] =
  P("[" ~ ws ~ expression.rep(sep = ws ~ "," ~ ws) ~ ws ~ "]")
    .map(ArrayLiteral.apply)
