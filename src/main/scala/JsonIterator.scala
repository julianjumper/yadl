package jsoniterator

import io.circe._
import io.circe.parser._
import scala.io.Source

class JsonIterator(input: Source) extends Iterator[Either[String, (String, Any)]] {
  private val jsonStream = input.getLines().mkString("\n")
  private val parsedJson = parse(jsonStream)
  private var currentIterator: Option[Iterator[(String, Json)]] = None

  init()

  private def init(): Unit = {
    parsedJson match {
      case Right(json) =>
        json.arrayOrObject(
          throw new IllegalArgumentException("JSON does not contain an object or an array"),
          array => currentIterator = Some(array.zipWithIndex.map { case (value, index) => (s"[$index]", value) }.iterator),  // Handle arrays
          obj => currentIterator = Some(obj.toIterable.iterator)
        )
      case Left(error) =>
        throw new IllegalArgumentException(s"Failed to parse JSON: ${error.message}")
    }
  }

  override def hasNext: Boolean = {
    currentIterator.exists(_.hasNext)
  }

  override def next(): Either[String, (String, Any)] = {
    currentIterator match {
      case Some(it) if it.hasNext =>
        val (key, value) = it.next()
        value.fold(
          jsonNull = Right(key, None),
          jsonBoolean = bool => Right(key, bool),
          jsonNumber = num => Right(key, num.toBigDecimal.getOrElse(BigDecimal(0))),
          jsonString = str => Right(key, str),
          jsonArray = arr => Right(key, new JsonIterator(Source.fromString(Json.fromValues(arr).noSpaces))),
          jsonObject = obj => Right(key, new JsonIterator(Source.fromString(Json.fromJsonObject(obj).noSpaces)))
        )
      case _ => Left("No more elements to iterate")
    }
  }
}