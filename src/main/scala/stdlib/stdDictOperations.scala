package stdlib

import jsoniterator.JsonIterator
import interpreterdata.*
import scala.io.Source
import scala.collection.mutable


private def loadFunction(params: Seq[DataObject]): DataObject = params match {
  case Seq(StringObj(path), StringObj(format)) =>
    format.toLowerCase match {
      case "json" =>
        try {
          val source = Source.fromFile(path)
          val iterator = new JsonIterator(source)
          IteratorObj(
            FunctionObj(Seq("d"), Seq(), None, _ => {
              if (iterator.hasNext) {
                iterator.next() match {
                  case Right((key, value)) => convertToDataObject((key, value))
                  case Left(error) => throw new RuntimeException(error)
                }
              } else {
                throw new RuntimeException("No more elements")
              }
            }),
            FunctionObj(Seq("d"), Seq(), None, _ => BooleanObj(iterator.hasNext)),
            DictionaryObj(mutable.HashMap[DataObject, DataObject]())
          )
        } catch {
          case e: Exception => throw new RuntimeException(s"Error loading JSON file: ${e.getMessage}")
        }
      case _ =>
        throw new IllegalArgumentException(s"Unsupported format: $format")
    }
  case _ =>
    throw new IllegalArgumentException("load function requires two string arguments: path and format")
}

private def convertToDataObject(value: (String, Any)): DataObject = value match {
  case (key, null) => DictionaryObj(mutable.HashMap(StringObj(key) -> NoneObj()))
  case (key, b: Boolean) => DictionaryObj(mutable.HashMap(StringObj(key) -> BooleanObj(b)))
  case (key, n: BigDecimal) => DictionaryObj(mutable.HashMap(StringObj(key) -> NumberObj(n.toDouble)))
  case (key, s: String) => DictionaryObj(mutable.HashMap(StringObj(key) -> StringObj(s)))
  case (key, i: JsonIterator) =>
    DictionaryObj(mutable.HashMap(StringObj(key) -> IteratorObj(
      FunctionObj(Seq("d"), Seq(), None, _ => {
        if (i.hasNext) {
          i.next() match {
            case Right((k, v)) => convertToDataObject((k, v))
            case Left(error) => throw new RuntimeException(error)
          }
        } else {
          throw new RuntimeException("No more elements")
        }
      }),
      FunctionObj(Seq("d"), Seq(), None, _ => BooleanObj(i.hasNext)),
      DictionaryObj(mutable.HashMap[DataObject, DataObject]())
    )))
  case (_, v) => throw new IllegalArgumentException(s"Unsupported type: ${v.getClass.getSimpleName}")
}

