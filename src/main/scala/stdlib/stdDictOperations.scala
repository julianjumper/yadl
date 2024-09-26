package stdlib

import scala.io.Source
import java.nio.file.Paths
import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.collection.immutable

import interpreterdata.*
import jsoniterator.JsonIterator
import fastparse._, NoWhitespace._
import parser.{
  identifierP,
  Identifier,
  Expression,
  valueP,
  DictionaryEntry,
  Dictionary,
  ArrayLiteral,
  StdString
}

private def loadFunction(params: Seq[DataObject]): DataObject = {
  if (
    params.length != 2 || !params(0).isInstanceOf[StringObj] || !params(1)
      .isInstanceOf[StringObj]
  ) {
    throw IllegalArgumentException(
      "Load function expects two string arguments: path and format"
    )
  }

  val path = params(0).asInstanceOf[StringObj].value
  val format = params(1).asInstanceOf[StringObj].value.toLowerCase

  val currentDir = System.getProperty("user.dir")
  val fullPath = Paths.get(currentDir, path).toString
  val source = Source.fromFile(fullPath)

  format match {
    case "json" =>
      val jsonIterator = new JsonIterator(source)
      processJsonIterator(jsonIterator)
    case "lines" =>
      val iter =
        source
          .LineIterator()
          .asInstanceOf[Iterator[String]]
          .map(StringObj(_).asInstanceOf[DataObject])
      ListObj(ArrayBuffer(iter.toArray*))
    case "csv" =>
      parseCSV(source.LineIterator())
    case _ => throw IllegalArgumentException(s"Unsupported format: $format")
  }
}

def valueParser[$: P]: P[Expression] = P(valueP(identifierP))

private def parseCSV(lineIter: Iterator[String]): DataObject = {
  val (local, header) = lineIter.duplicate // Thanks Java
  parseHeader(header) match
    case Some(value) => {
      val entry_iter: Iterator[Seq[(Expression, Expression)]] = local
        .drop(1)
        .map(line =>
          value.zip(
            line
              .split(",")
              .map(v =>
                parse(v, valueParser(using _)) match {
                  case Parsed.Success(Identifier(name), _) => StdString(name)
                  case Parsed.Success(v, _)                => v
                  case e: Parsed.Failure =>
                    scala.sys.error("failed to parse a value in a csv file")
                }
              )
          )
        )
      val dicts: Iterator[DataObject] = entry_iter
        .map((entry) =>
          Dictionary(
            entry
              .map((key, value) => DictionaryEntry(key, value))
              .toSeq
          )
        )
        .map(toDataObject)
      ListObj(ArrayBuffer(dicts.toArray*))
    }
    case None => {
      val entry_iter = local
        .map(line =>
          line
            .split(",")
            .map(v =>
              parse(v, valueParser(using _)) match {
                case Parsed.Success(Identifier(name), _) => StdString(name)
                case Parsed.Success(v, _)                => v
                case e: Parsed.Failure =>
                  scala.sys.error("failed to parse a value in a csv file")
              }
            )
            .map {
              case Identifier(name) =>
                StdString(name)
              case v => v
            }
        )
        .map(v => toDataObject(ArrayLiteral(v)))
      ListObj(ArrayBuffer(entry_iter.toArray*))
    }
}

private def parseHeader(lineIter: Iterator[String]): Option[Seq[Expression]] = {
  val iter = lineIter.filter(line =>
    line
      .split(",")
      .forall(elem =>
        elem.forall(char => Character.isAlphabetic(char) || char == '_') || elem
          .length() > 0 && elem(0) == '"' && elem(
          elem.length() - 1
        ) == '"'
      )
  )
  if (iter.hasNext) {
    val header = iter
      .map { line =>
        line
          .split(",")
          .map((v: String) =>
            parse(v, valueParser(using _)) match {
              case Parsed.Success(Identifier(name), _) => StdString(name)
              case Parsed.Success(v, _)                => v
              case e: Parsed.Failure =>
                scala.sys.error("failed to parse a value in a csv file")
            }
          )
      }
      .take(1)

    Some(header.next().toSeq)
  } else None
}

private def processJsonIterator(iterator: JsonIterator): DataObject = {
  val result = HashMap[String, DataObject]()

  while (iterator.hasNext) {
    iterator.next() match {
      case Right((key, value)) =>
        val valueObj = convertJsonExpression(value)
        result(key) = valueObj
      case Left(error) =>
        throw IllegalArgumentException(s"Error reading from iterator: $error")
    }
  }

  convertToProperStructure(result)
}

private def convertToProperStructure(
    map: HashMap[String, DataObject]
): DataObject = {
  if (map.keys.forall(_.matches("""\[\d+\]"""))) {
    val sortedEntries = map.toSeq
      .sortBy { case (key, _) =>
        key.substring(1, key.length - 1).toInt
      }
      .map(_._2)
    ListObj(ArrayBuffer(sortedEntries*))
  } else {
    DictionaryObj(map.map { case (k, v) => StringObj(k) -> v })
  }
}

private def convertJsonExpression(value: Any): DataObject = {
  value match {
    case null | None      => NoneObj()
    case b: Boolean       => BooleanObj(b)
    case n: BigDecimal    => NumberObj(n.toDouble)
    case s: String        => StringObj(s)
    case it: JsonIterator => processJsonIterator(it)
    case other =>
      throw IllegalArgumentException(
        s"Unsupported JSON value type: ${other.getClass}"
      )
  }
}
