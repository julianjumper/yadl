package parser

enum BooleanOps:
  case And, Or, Not

enum CompareOps:
  case Less, LessEq, Greater, GreaterEq, Eq, NotEq

enum ArithmaticOps:
  case Add, Sub, Mul, Div, Expo, Mod

trait Operator
trait Expression
trait Statement
trait Number extends Expression:
  def `+`(other: Number): Number
  def `*`(other: Number): Number
  def `/`(other: Number): Number
  def `-`(other: Number): Number
  def `^`(other: Number): Number
  def `%`(other: Number): Number

  def `==`(other: Number): Boolean
  def `<`(other: Number): Boolean

  def asFloat: Double

case class ArithmaticOp(op: ArithmaticOps) extends Operator
case class CompareOp(op: CompareOps) extends Operator
case class BooleanOp(op: BooleanOps) extends Operator

case class NoneValue() extends Expression:
  override def toString(): String =
    "none"
case class Identifier(name: String) extends Expression
case class YadlFloat(value: Double) extends Number:
  override def toString(): String = value.toString
  override def `+`(other: Number): Number = YadlFloat(value + other.asFloat)
  override def `*`(other: Number): Number = YadlFloat(value * other.asFloat)
  override def `/`(other: Number): Number = YadlFloat(value / other.asFloat)
  override def `-`(other: Number): Number = YadlFloat(value - other.asFloat)
  override def `%`(other: Number): Number = YadlFloat(value % other.asFloat)
  override def `^`(other: Number): Number = YadlFloat(
    scala.math.pow(value, other.asFloat)
  )
  def `==`(other: Number): Boolean = value == other.asFloat
  def `<`(other: Number): Boolean = value < other.asFloat
  override def asFloat: Double = value

case class YadlInt(value: Long) extends Number:
  override def toString(): String = value.toString
  override def `+`(other: Number): Number = other match
    case YadlInt(v)   => YadlInt(value + v)
    case YadlFloat(v) => YadlFloat(value.toDouble + v)

  override def `*`(other: Number): Number = other match
    case YadlInt(v)   => YadlInt(value * v)
    case YadlFloat(v) => YadlFloat(value.toDouble * v)

  override def `/`(other: Number): Number = YadlFloat(
    value.toDouble / other.asFloat
  )

  override def `-`(other: Number): Number = other match
    case YadlInt(v)   => YadlInt(value - v)
    case YadlFloat(v) => YadlFloat(value.toDouble - v)

  override def `%`(other: Number): Number = other match
    case YadlInt(v)   => YadlInt(value % v)
    case YadlFloat(v) => YadlFloat(value.toDouble % v)

  override def `^`(other: Number): Number =
    if (other.isInstanceOf[YadlInt] && other.asInstanceOf[YadlInt].value >= 0)
      YadlInt(scala.math.pow(value.toDouble, other.asFloat).toLong)
    else
      YadlFloat(scala.math.pow(value.toDouble, other.asFloat))

  def `==`(other: Number): Boolean = value == other.asFloat
  def `<`(other: Number): Boolean = value < other.asFloat

  override def asFloat: Double = value.toDouble

case class Bool(b: Boolean) extends Expression:
  override def toString(): String = b.toString

case class BinaryOp(left: Expression, op: Operator, right: Expression)
    extends Expression
case class UnaryOp(op: Operator, operant: Expression) extends Expression
case class Function(args: Seq[String], body: Seq[Statement]) extends Expression
case class Wrapped(value: Expression) extends Expression
case class StdString(value: String) extends Expression:
  override def toString(): String = value.toString

case class FormatString(value: List[Expression]) extends Expression
class DictionaryEntry(var key: Expression, var value: Expression):
  override def toString(): String = key.toString + ": " + value.toString

case class Dictionary(val entries: Seq[DictionaryEntry]) extends Expression:
  override def toString(): String =
    "{" + entries.mkString(", ") + "}"

case class ArrayLiteral(val elements: Seq[Expression]) extends Expression:
  override def toString(): String =
    "[" + elements.mkString(", ") + "]"

case class StructureAccess(identifier: Expression, key: Expression)
    extends Expression

case class Assignment(varName: String, value: Expression) extends Statement
case class StructuredAssignment(struct: StructureAccess, value: Expression)
    extends Statement
case class Branch(condition: Expression, body: Seq[Statement])
case class If(
    ifBranch: Branch,
    elifBranches: Seq[Branch],
    elseBranch: Option[Seq[Statement]]
) extends Statement
case class WhileLoop(loop: Branch) extends Statement
case class Return(value: Expression) extends Statement

case class FunctionCall(functionExpr: Expression, args: Seq[Expression])
    extends Expression,
      Statement
