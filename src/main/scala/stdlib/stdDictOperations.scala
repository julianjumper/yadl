package stdlib

import interpreterdata.*
import jsoniterator.JsonIterator
import scala.io.Source
import java.nio.file.Paths
import scala.collection.mutable


private def loadFunction(params: Seq[DataObject]): DataObject = {
  if (params.length != 2 || !params(0).isInstanceOf[StringObj] || !params(1).isInstanceOf[StringObj]) {
    throw IllegalArgumentException("Load function expects two string arguments: path and format")
  }

  val path = params(0).asInstanceOf[StringObj].value
  val format = params(1).asInstanceOf[StringObj].value.toLowerCase

  val currentDir = System.getProperty("user.dir")
  val fullPath = Paths.get(currentDir, "test", "tests", path).toString
  val source = Source.fromFile(fullPath)

  format match {
    case "json" =>
      val jsonIterator = new JsonIterator(source)
      processJsonIterator(jsonIterator)
    case _ => throw IllegalArgumentException(s"Unsupported format: $format")
  }
}

private def processJsonIterator(iterator: JsonIterator): DataObject = {
  val result = mutable.HashMap[String, DataObject]()

  while (iterator.hasNext) {
    iterator.next() match {
      case Right((key, value)) =>
        val valueObj = convertJsonValue(value)
        result(key) = valueObj
      case Left(error) =>
        throw IllegalArgumentException(s"Error reading from iterator: $error")
    }
  }

  convertToProperStructure(result)
}

private def convertToProperStructure(map: mutable.HashMap[String, DataObject]): DataObject = {
  if (map.keys.forall(_.matches("""\[\d+\]"""))) {
    val sortedEntries = map.toSeq.sortBy { case (key, _) =>
      key.substring(1, key.length - 1).toInt
    }.map(_._2)
    ListObj(mutable.ArrayBuffer(sortedEntries: _*))
  } else {
    DictionaryObj(map.map { case (k, v) => StringObj(k) -> v })
  }
}

private def convertJsonValue(value: Any): DataObject = {
  value match {
    case null | None => NoneObj()
    case b: Boolean => BooleanObj(b)
    case n: BigDecimal => NumberObj(n.toDouble)
    case s: String => StringObj(s)
    case it: JsonIterator => processJsonIterator(it)
    case other =>
      throw IllegalArgumentException(s"Unsupported JSON value type: ${other.getClass}")
  }
}