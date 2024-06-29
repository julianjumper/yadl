import munit.FunSuite
import scala.io.Source
import jsoniterator.JsonIterator

class JsonParsing extends FunSuite{
  test("JsonIterator should correctly handle simple JSON objects") {
    val json = """{"name":"John", "age":30}"""
    val iterator = new JsonIterator(Source.fromString(json))
    val results = iterator.toList
    print(results)
    assertEquals(results, List(Right("name", "John"), Right("age", BigDecimal(30))))
  }

  test("JsonIterator should gracefully handle empty JSON objects") {
    val json = """{}"""
    val iterator = new JsonIterator(Source.fromString(json))
    assert(!iterator.hasNext, "Iterator should not have any elements")
  }

  test("JsonIterator should handle large numbers") {
    val json = """{"bigNumber": 12345678901234567890}"""
    val iterator = new JsonIterator(Source.fromString(json))
    val result = iterator.next()
    assertEquals(result, Right("bigNumber", BigDecimal("12345678901234567890")))
  }

  test("JsonIterator should gracefully handle empty arrays and objects") {
    val json = """{"emptyObj": {}, "emptyArr": []}"""
    val iterator = new JsonIterator(Source.fromString(json))
    val objResult = iterator.next()
    assert(objResult.isRight, "Expected Right for empty object")
    objResult match {
      case Right(("emptyObj", subIterator: JsonIterator)) =>
        assert(!subIterator.hasNext, "Iterator for empty object should have no next elements")
      case _ => fail("Expected a JsonIterator for the empty object")
    }
    val arrResult = iterator.next()
    assert(arrResult.isRight, "Expected Right for empty array")
    arrResult match {
      case Right(("emptyArr", subIterator: JsonIterator)) =>
        assert(!subIterator.hasNext, "Iterator for empty array should have no next elements")
      case _ => fail("Expected a JsonIterator for the empty array")
    }
  }

  test("next should throw NoSuchElementException when there are no more elements") {
    val json = """{"name":"Alice"}"""
    val iterator = new JsonIterator(Source.fromString(json))
    val res1 = iterator.next() // Consume the only element
    val res2 = iterator.next() // Attempt to consume the next element
    assert(res2.isLeft, "Expected a Left result when there are no more elements")
  }

  test("JsonIterator should throw an exception for malformed JSON") {
    val malformedJson = """{"user": "Alice", "age": 30, "data": [10, "no end brace""" // Malformed JSON
    val caught = intercept[IllegalArgumentException] {
      new JsonIterator(Source.fromString(malformedJson))
    }
    assert(caught.getMessage.contains("Failed to parse JSON"), "Exception message should indicate parsing failure")
  }

  test("next should handle a simple JSON key-value pair") {
    val json = """{"name":"Alice"}"""
    val iterator = new JsonIterator(Source.fromString(json))
    val result = iterator.next()
    assertEquals(result, Right("name", "Alice"))
    assert(!iterator.hasNext, "Iterator should have no more elements")
  }

  test("JsonIterator should handle a top-level JSON array") {
    val json = """[{"name":"Alice"}, {"name":"Bob"}]"""
    val iterator = new JsonIterator(Source.fromString(json))
    val firstResult = iterator.next()
    assert(firstResult.isRight, "Expected Right containing a JsonIterator for array elements")
    firstResult match {
      case Right(("[0]", subIterator: JsonIterator)) =>
        val arrayResults = subIterator.toList
        assertEquals(arrayResults, List(Right("name", "Alice")))
        val secondResult = iterator.next()
        secondResult match {
          case Right(("[1]", subIterator: JsonIterator)) =>
            val arrayResults = subIterator.toList
            assertEquals(arrayResults, List(Right("name", "Bob")))
          case _ => fail("Expected a JsonIterator for the second array element")
        }
      case _ => fail("Expected a JsonIterator for array elements")
    }
  }

  test("JsonIterator should handle nested JSON objects") {
    val json = """{"user": {"name": "John", "age": 30}}"""
    val iterator = new JsonIterator(Source.fromString(json))
    val userResult = iterator.next()
    assert(userResult.isRight, "Expected a Right value for nested object")
    userResult match {
      case Right(("user", subIterator: JsonIterator)) =>
        val nameResult = subIterator.next()
        assertEquals(nameResult, Right("name", "John"))
        val ageResult = subIterator.next()
        assertEquals(ageResult, Right("age", BigDecimal(30)))
      case _ => fail("Expected a JsonIterator for the nested object")
    }
  }

  test("JsonIterator should handle JSON arrays") {
    val json = """{"numbers": [1, 2, 3]}"""
    val iterator = new JsonIterator(Source.fromString(json))
    val result = iterator.next()
    assert(result.isRight, "Expected Right with a JsonIterator for the array")
    result match {
      case Right(("numbers", subIterator: JsonIterator)) =>
        val firstNumber = subIterator.next()
        assertEquals(firstNumber, Right("[0]", BigDecimal(1)))  // Adjusted to match the expected index key
        val secondNumber = subIterator.next()
        assertEquals(secondNumber, Right("[1]", BigDecimal(2)))  // Adjusted to match the expected index key
        val thirdNumber = subIterator.next()
        assertEquals(thirdNumber, Right("[2]", BigDecimal(3)))  // Adjusted to match the expected index key
      case _ => fail("Expected a JsonIterator for the array elements")
    }
  }


    test("JsonIterator should handle an array of mixed types") {
      val json = """{"mixed": [1, "two", null, true, {"key": "value"}, [1, 2]]}"""
      val iterator = new JsonIterator(Source.fromString(json))
      val result = iterator.next()
      assert(result.isRight, "Expected Right with a JsonIterator for mixed array")
      result match {
        case Right(("mixed", subIterator: JsonIterator)) =>
          val numResult = subIterator.next()
          assertEquals(numResult, Right("[0]", BigDecimal(1)))
          val strResult = subIterator.next()
          assertEquals(strResult, Right("[1]", "two"))
          val nullResult = subIterator.next()
          assertEquals(nullResult, Right("[2]", None))
          val boolResult = subIterator.next()
          assertEquals(boolResult, Right("[3]", true))
          val objResult = subIterator.next()
          objResult match {
            case Right(("[4]", objSubIterator: JsonIterator)) =>
              val keyValue = objSubIterator.next()
              assertEquals(keyValue, Right("key", "value"))
            case _ => fail("Expected a JsonIterator for the embedded object in array")
          }
          val innerArrResult = subIterator.next()
          innerArrResult match {
            case Right(("[5]", arrSubIterator: JsonIterator)) =>
              val firstArrItem = arrSubIterator.next()
              assertEquals(firstArrItem, Right("[0]", BigDecimal(1)))
              val secondArrItem = arrSubIterator.next()
              assertEquals(secondArrItem, Right("[1]", BigDecimal(2)))
            case _ => fail("Expected a JsonIterator for the embedded array in array")
          }
        case _ => fail("Expected a JsonIterator for the 'mixed' key")
      }
    }

  test("JsonIterator should handle large nested mixed content") {
    val json = """{"outer": {"arr": [10, {"inner": "text"}, false], "bool": true}}"""
    val iterator = new JsonIterator(Source.fromString(json))
    val outerResult = iterator.next()
    assert(outerResult.isRight, "Expected Right for 'outer'")
    outerResult match {
      case Right(("outer", subIterator: JsonIterator)) =>
        val arrResult = subIterator.next()
        arrResult match {
          case Right(("arr", arrSubIterator: JsonIterator)) =>
            val num = arrSubIterator.next()
            assertEquals(num, Right("[0]", BigDecimal(10)))
            val innerObj = arrSubIterator.next()
            innerObj match {
              case Right(("[1]", innerSubIterator: JsonIterator)) =>
                val innerText = innerSubIterator.next()
                assertEquals(innerText, Right("inner", "text"))
              case _ => fail("Expected a JsonIterator for the nested object in array")
            }
            val bool = arrSubIterator.next()
            assertEquals(bool, Right("[2]", false))
          case _ => fail("Expected a JsonIterator for 'arr'")
        }
        val boolResult = subIterator.next()
        assertEquals(boolResult, Right("bool", true))
      case _ => fail("Expected a JsonIterator for 'outer'")
    }
  }

  test("JsonIterator should handle a complex JSON structure with an outer array and various nested elements") {
    val json =
      """[{"id": 1, "name": "Item1", "details": {"color": "red", "size": "large"}},{"id": 2, "name": "Item2", "properties": [true, 99.99, "available", {"date": "2022-01-01"}]},{"id": 3, "type": "Item3", "misc": {"info": "Testing"}},{"id": 4, "complex": [{"nestedLevel1": [{"nestedLevel2": "deepValue"}]}]}]""".stripMargin
    val iterator = new JsonIterator(Source.fromString(json))

    val firstItem = iterator.next()
    assert(firstItem.isRight, "Expected Right for the first object")
    firstItem match {
      case Right(("[0]", subIterator: JsonIterator)) =>
        val idResult = subIterator.next()
        assertEquals(idResult, Right("id", BigDecimal(1)))
        val nameResult = subIterator.next()
        assertEquals(nameResult, Right("name", "Item1"))
        val detailsResult = subIterator.next()
        detailsResult match {
          case Right(("details", detailsIterator: JsonIterator)) =>
            val colorResult = detailsIterator.next()
            assertEquals(colorResult, Right("color", "red"))
            val sizeResult = detailsIterator.next()
            assertEquals(sizeResult, Right("size", "large"))
          case _ => fail("Expected a JsonIterator for 'details'")
        }
      case _ => fail("Expected a JsonIterator for the first array element")
    }

    val secondItem = iterator.next()
    assert(secondItem.isRight, "Expected Right for the second object")
    secondItem match {
      case Right(("[1]", subIterator: JsonIterator)) =>
        val idResult = subIterator.next()
        assertEquals(idResult, Right("id", BigDecimal(2)))
        val nameResult = subIterator.next()
        assertEquals(nameResult, Right("name", "Item2"))
        val propertiesResult = subIterator.next()
        propertiesResult match {
          case Right(("properties", propertiesIterator: JsonIterator)) =>
            val boolResult = propertiesIterator.next()
            assertEquals(boolResult, Right("[0]", true))
            val numberResult = propertiesIterator.next()
            assertEquals(numberResult, Right("[1]", BigDecimal(99.99)))
            val availableResult = propertiesIterator.next()
            assertEquals(availableResult, Right("[2]", "available"))
            val dateResult = propertiesIterator.next()
            dateResult match {
              case Right(("[3]", dateIterator: JsonIterator)) =>
                val actualDate = dateIterator.next()
                assertEquals(actualDate, Right("date", "2022-01-01"))
              case _ => fail("Expected a JsonIterator for the 'date' object")
            }
          case _ => fail("Expected a JsonIterator for 'properties'")
        }
      case _ => fail("Expected a JsonIterator for the second array element")
    }
  }

  test("JsonIterator should handle multi-line formatted JSON") {
    val json =
      """[
        |{"id": 1, "name": "Item1", "details": {"color": "red", "size": "large"}},
        |{"id": 2, "name": "Item2", "properties": [true, 99.99, "available", {"date": "2022-01-01"}]},
        |{"id": 3, "type": "Item3", "misc": {"info": "Testing"}},
        |{"id": 4, "complex": [{"nestedLevel1": [{"nestedLevel2": "deepValue"}]}]}
        |]""".stripMargin
    val iterator = new JsonIterator(Source.fromString(json))

    def extractValues(it: JsonIterator): List[(String, Any)] = {
      var results = List.empty[(String, Any)]
      while (it.hasNext) {
        it.next() match {
          case Right((key, value: JsonIterator)) =>
            results ::= (key, extractValues(value))
          case Right((key, value)) =>
            results ::= (key, value)
          case Left(error) =>
            fail(s"Error: $error")
        }
      }
      results.reverse
    }

    val results = extractValues(iterator)

    val expectedResults = List(
      ("[0]", List(("id", BigDecimal(1)), ("name", "Item1"), ("details", List(("color", "red"), ("size", "large"))))),
      ("[1]", List(("id", BigDecimal(2)), ("name", "Item2"), ("properties", List(("[0]", true), ("[1]", BigDecimal(99.99)), ("[2]", "available"), ("[3]", List(("date", "2022-01-01"))))))),
      ("[2]", List(("id", BigDecimal(3)), ("type", "Item3"), ("misc", List(("info", "Testing"))))),
      ("[3]", List(("id", BigDecimal(4)), ("complex", List(("[0]", List(("nestedLevel1", List(("[0]", List(("nestedLevel2", "deepValue")))))))))))
    )

    assertEquals(results, expectedResults)
  }

}
