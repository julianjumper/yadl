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

  test("case '0_.0'") {
    val input = "0_.0"
    parse(input, numberP(using _)) match {
      case Success(value, index) =>
        assert(false, "parsing succeded where it should have failed")
      case _: Failure =>
        assert(true, "expected failure")
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

  test("case '0b1101101.1101'") {
    val input = "0b1101101.1101"
    val expected = Number(109.13)
    parse(input, numberP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '0b.1101'") {
    val input = "0b.1101"
    val expected = Number(0.13)
    parse(input, numberP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '0b1101101.'") {
    val input = "0b1101101."
    val expected = Number(109.0)
    parse(input, numberP(using _)) match {
      case Success(_, _) =>
        assert(false, "parsing succeded where it should have failed")
      case _: Failure =>
        assert(true, "expected failure")
    }
  }

  test("case '0x10aaff.22ff'") {
    val input = "0x10aaff.22ff"
    val expected = Number(1092351.8959)
    parse(input, numberP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '0x10AAFF.22FF'") {
    val input = "0x10AAFF.22FF"
    val expected = Number(1092351.8959)
    parse(input, numberP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '0x10aazz.22zz'") {
    val input = "0x10aazz.22zz"
    val expected = Number(1092351.8959)
    parse(input, numberP(using _)) match {
      case Success(_, _) =>
        assert(false, "parsing succeded where it should have failed")
      case _: Failure =>
        assert(true, "expected failure")
    }
  }

  test("case '0x10AAZZ.22ZZ'") {
    val input = "0x10AAZZ.22ZZ"
    val expected = Number(1092351.8959)
    parse(input, numberP(using _)) match {
      case Success(_, _) =>
        assert(false, "parsing succeded where it should have failed")
      case _: Failure =>
        assert(true, "expected failure")
    }
  }
}
