package interpreterdata

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

import parser.{
  ArrayLiteral,
  Bool,
  Function,
  Number,
  StdString,
  Expression,
  Dictionary,
  DictionaryEntry
}

/** Base Trait of all Interpreter objects.
  */
sealed trait DataObject {
  def typeName: String
}

type InterpreterFunctionType = Seq[DataObject] => DataObject

/** @param params
  *   A list of the names of all mandatory arguments.
  * @param optionals
  *   A list of all optional arguments and their defaults.
  * @param argList
  *   Specifies the name of the argList argument if one exists.
  * @param function
  *   The actual function. It expects a list of |params|+|optionals| arguments
  *   if no ArgList exists, |params|+|optionals|+1 otherwise. (ArgList is always
  *   an iterator.) The order of the arguments should reflect: params,
  *   optionals, ArgList If an optional is unused, the default value must be
  *   supplied. If an ArgList exists, it will be supplied as an iterator. If the
  *   function originates from the AST, the AST eval should be wrapped into
  *   another function of this format. Note: the function itself is still
  *   responsible for throwing an runtime error if the number/type of the
  *   arguments is incorrect.
  */
class FunctionObj(
    val params: Seq[String],
    val optionals: Seq[(String, DataObject)],
    val argList: Option[String],
    val function: InterpreterFunctionType
) extends DataObject {
  def typeName = "function"

  def n_args: Integer = params.length + optionals.length + (argList match {
    case Some(_) => 1
    case None    => 0
  })
}
object FunctionObj {
  def unapply(fn: FunctionObj): Option[
    (
        Seq[String],
        Seq[(String, DataObject)],
        Option[String],
        InterpreterFunctionType | Seq[parser.Statement]
    )
  ] = {
    Some(fn.params, fn.optionals, fn.argList, fn.function)
  }
}

case class NoneObj() extends DataObject {
  def typeName = "none"
}
case class UndefinedObj() extends DataObject {
  def typeName = "undefined"
}
case class NumberObj(val value: Double) extends DataObject {
  def typeName = "number"
}
case class BooleanObj(val value: Boolean) extends DataObject {
  def typeName = "boolean"
}
case class StringObj(val value: String) extends DataObject {
  def typeName = "string"
}
class ListObj(val value: scala.collection.mutable.ArrayBuffer[DataObject])
    extends DataObject {
  def typeName = "list"
}
object ListObj {
  def unapply(
      listObj: ListObj
  ): Option[scala.collection.mutable.ArrayBuffer[DataObject]] = {
    Some(listObj.value)
  }
}
class DictionaryObj(
    val value: scala.collection.mutable.HashMap[DataObject, DataObject]
) extends DataObject {
  def typeName = "dictionary"
}
object DictionaryObj {
  def unapply(
      dictionaryObj: DictionaryObj
  ): Option[scala.collection.mutable.HashMap[DataObject, DataObject]] = {
    Some(dictionaryObj.value)
  }
}
class IteratorObj(
    val next: FunctionObj,
    val hasNext: FunctionObj,
    val data: DictionaryObj
) extends DataObject {
  def typeName = "iterator"

  override def toString: String = {
    val nextStr = next.toString.take(20) + "..." // Truncate for brevity
    val hasNextStr = hasNext.toString.take(20) + "..."
    val dataStr = data.toString.take(20) + "..."
    s"IteratorObj(next: $nextStr, hasNext: $hasNextStr, data: $dataStr)"
  }
}
object IteratorObj {
  def unapply(
      iteratorObj: IteratorObj
  ): Option[(FunctionObj, FunctionObj, DictionaryObj)] = {
    Some((iteratorObj.next, iteratorObj.hasNext, iteratorObj.data))
  }
}
// TODO consider renaming it from Obj to something more expressive.

def toDataObject(value: Expression): DataObject =
  value match {
    case Bool(b)          => BooleanObj(b)
    case Number(value)    => NumberObj(value)
    case StdString(value) => StringObj(value)
    case x: ArrayLiteral =>
      ListObj(x.elements.map(toDataObject).to(ArrayBuffer))
    case x: parser.Dictionary => {
      val m = HashMap[DataObject, DataObject]()
      for (e <- x.entries) {
        m.put(toDataObject(e.key), toDataObject(e.value))
      }
      DictionaryObj(m)
    }
    case x: Function => {
      // TODO add optionals
      FunctionObj(
        x.args,
        Seq(),
        None,
        (params: Seq[DataObject]) => {
          val scope = _root_.`<empty>`.Scope()
          val newScope = _root_.`<empty>`.evalFunctionCall(
            x,
            params.map(x => toAstNode(x)),
            scope,
            _root_.`<empty>`.CallContext.Expression
          )
          newScope.result match {
            case None    => NONE // TODO
            case Some(x) => toDataObject(x)
          }
        }
      )
    }
    case v => assert(false, s"Value can not be converted to DataObject: $v")
  }

def toAstNode(data: DataObject): Expression =
  data match
    case NumberObj(value) =>
      Number(value)
    case BooleanObj(value) =>
      Bool(value)
    case StringObj(value) =>
      StdString(value)
    case DictionaryObj(value) =>
      Dictionary(value.map { case (k, v) =>
        DictionaryEntry(toAstNode(k), toAstNode(v))
      }.toSeq)
    case ListObj(value) =>
      parser.ArrayLiteral(value.map(toAstNode).toSeq)
    case NoneObj() =>
      parser.NoneValue()
    case x: IteratorObj => {
      toAstNode(stdlib.toListObj(x)) // TODO this is bad!
    }
    case x: ListObj =>
      ArrayLiteral(x.value.map(x => toAstNode(x)).toSeq) // TODO this is bad!
    case v => assert(false, s"Data object not convertable to AST node: $v")
