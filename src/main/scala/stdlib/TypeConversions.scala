package stdlib

import interpreterdata.*
import scala.collection.mutable.ArrayBuffer
/**
 * Generic utility method that converts any Data Object to a String Object if possible (throws an error otherwise)
 * Can be used outside the stdlib if necessary.
 */
def toStringObj(obj: DataObject): StringObj = {
  obj match {
    case x: NumberObj => StringObj(x.value.toString)
    case x: StringObj => x
    case x: BooleanObj => StringObj(x.value.toString)
    case x: FunctionObj => {
      val optionalsStr = if (x.optionals.nonEmpty) ", " + x.optionals.map(opt => opt._1 + "=" + opt._2.typeName).mkString(", ") else ""
      val argListStr = x.argList match {
        case Some(s) => s", $s..."
        case None => ""
      }
      StringObj("function(%s%s%s)" format (x.params.mkString(", "), optionalsStr, argListStr))
    }
    case x: DictionaryObj => StringObj(serializeJSONCompact(x, Seq()))
    case x: ListObj => StringObj(serializeJSONCompact(x, Seq()))
    case x: NoneObj => StringObj(x.typeName)
    case x: UndefinedObj => StringObj(x.typeName)
    case x: IteratorObj => StringObj(x.typeName)
  }
}

/**
 * Generic utility method that converts any Data Object to a Boolean Object if possible (throws an error otherwise)
 * Can be used outside the stdlib if necessary.
 */
def toBooleanObj(obj: DataObject): BooleanObj = obj match {
  case x: BooleanObj => x
  case x: NumberObj => BooleanObj(x.value != 0)
  case x: StringObj => BooleanObj(x.value.nonEmpty)
  case x: NoneObj => BooleanObj(false)
  case x: DictionaryObj => BooleanObj(x.value.nonEmpty)
  case x: ListObj => BooleanObj(x.value.nonEmpty)
  // Note: Possible while true loop here,
  // but if the programmer returns an iterator in the hastNext
  // function, they don't deserve any better.
  case x: IteratorObj => toBooleanObj(x.hasNext.function(Seq(x.data)))
  case x: UndefinedObj => throw IllegalArgumentException()
  case x: FunctionObj => BooleanObj(true)
}

/**
 * Generic utility method that converts any Data Object to an Iterator Object if possible (throws an error otherwise)
 * Can be used outside the stdlib if necessary.
 */
def toIteratorObj(obj: DataObject): IteratorObj = obj match {
  case x: IteratorObj => x
  case x: ListObj => {
    val next = FunctionObj(Seq("d"), Seq(), None, {
      case Seq(DictionaryObj(dict)) => {
        val indexKey = StringObj("index")
        val lstKey = StringObj("list")
        val curInd = dict.get(indexKey) match {
          case None => throw IllegalArgumentException()
          case Some(x) => x.asInstanceOf[NumberObj].value.toInt
        }
        val lst = dict.get(lstKey)match {
          case None => throw IllegalArgumentException()
          case Some(x) => x.asInstanceOf[ListObj].value
        }

        if (curInd >= lst.length) {
          throw IndexOutOfBoundsException()
        } else {
          val rv = lst(curInd)
          dict.addOne(indexKey, NumberObj(curInd + 1))
          rv
        }
      }
      case _ => throw IllegalArgumentException()
    })
    val hasnext = FunctionObj(Seq("d"), Seq(), None, {
      case Seq(DictionaryObj(dict)) => {
        val indexKey = StringObj("index")
        val lstKey = StringObj("list")
        val curInd = dict.get(indexKey) match {
          case None => throw IllegalArgumentException()
          case Some(x) => x.asInstanceOf[NumberObj].value.toInt
        }
        val lst = dict.get(lstKey) match {
          case None => throw IllegalArgumentException()
          case Some(x) => x.asInstanceOf[ListObj].value
        }

        if (curInd < lst.length) {
          TRUE
        } else {
          FALSE
        }
      }
      case _ => throw IllegalArgumentException()
    })

    val d = scala.collection.mutable.HashMap[DataObject, DataObject]()
    d.addOne(StringObj("index"), NumberObj(0))
    d.addOne(StringObj("list"), ListObj(x.value))

    IteratorObj(next, hasnext, DictionaryObj(d))
  }
  case x: DictionaryObj => throw NotImplementedError() //  TODO implement
  case _ => throw IllegalArgumentException()
}

def toNumberObj(obj: DataObject): NumberObj = obj match {
  case x: NumberObj => x
  case x: StringObj => throw IllegalArgumentException()
  case BooleanObj(x) => if (x == true) NumberObj(1) else NumberObj(0)
  case _ => throw IllegalArgumentException()
}

def toListObj(obj: DataObject): ListObj = obj match {
  case x: ListObj => x
  case it: IteratorObj => {
    var lst = new ArrayBuffer[DataObject]()
    while (it.hasNext.function(Seq(it.data)).asInstanceOf[BooleanObj].value) {
      lst += it.next.function(Seq(it.data))
    }
    ListObj(lst)
  }
  case dict: DictionaryObj => {
    ListObj(dict.value.toList.map((k, v) => {
      val b = new ArrayBuffer[DataObject]()
      b += k
      b += v
      ListObj(b)
    }).to(ArrayBuffer))
  }
  case _ => throw IllegalArgumentException()
}

private def escapeJsonString(str: String): String = {
  // Escape necessary characters for JSON
  str.flatMap {
    case '"' => "\\\"" // Escape double quotes
    case '\\' => "\\\\" // Escape backslashes
    case '\b' => "\\b" // Escape backspace
    case '\f' => "\\f" // Escape formfeed
    case '\n' => "\\n" // Escape newline
    case '\r' => "\\r" // Escape carriage return
    case '\t' => "\\t" // Escape tab
    case c if c.isControl => "\\u%04x".format(c.toInt) // Escape control characters
    case c => c.toString // Default case, no escaping needed
  }
}

private def serializeJSONCompact(obj: DataObject, references: Seq[Any]): String = {
  obj match {
    case NoneObj => s"\"${obj.typeName}\""
    case UndefinedObj => s"\"${obj.typeName}\""
    case IteratorObj => s"\"${obj.typeName}\""
    case FunctionObj => s"\"${obj.typeName}\""
    case BooleanObj(x) => x.toString
    case NumberObj(x) => {
      if (x == x.toInt) {
        x.toInt.toString
      } else {
        x.toString
      }
    }
    case StringObj(x) => s"\"${escapeJsonString(x)}\""
    case ListObj(lst) => {
      if (references.contains(lst)) {
        throw IllegalArgumentException("cannot serialize self referencing data structures")
      }
      
      "[" + (lst.map(e => serializeJSONCompact(e, references :+ lst)) mkString ", ") + "]"
    }
    case DictionaryObj(dict) => {
      if (references.contains(dict)) {
        throw IllegalArgumentException("cannot serialize self referencing data structures")
      }
      
      dict.map((k, v) => serializeJSONCompact(k, references :+ dict) + ": " + serializeJSONCompact(v, references :+ dict)) mkString ", "
    }
    case _ => throw UnsupportedOperationException()
  }
}
