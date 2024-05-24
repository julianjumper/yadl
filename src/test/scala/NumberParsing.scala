import fastparse._
import fastparse.Parsed.Success
import fastparse.Parsed.Failure

class NumberParsing extends munit.FunSuite {

  test("case '0'") {
    val input = "0"
    val expected = Number(0)
    parse(input, numberP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '000000'") {
    val input = "000000"
    val expected = Number(0)
    parse(input, numberP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '1000'") {
    val input = "1000"
    val expected = Number(1000)
    parse(input, numberP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '0_0'") {
    val input = "0_0"
    val expected = Number(0)
    parse(input, numberP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '0.01'") {
    val input = "0.01"
    val expected = Number(0.01)
    parse(input, numberP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '.001234'") {
    val input = ".001234"
    val expected = Number(0.001234)
    parse(input, numberP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '0.00_00_01'") {
    val input = "0.00_00_01"
    val expected = Number(0.000001)
    parse(input, numberP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }
}
