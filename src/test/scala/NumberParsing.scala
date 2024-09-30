import fastparse._
import fastparse.Parsed.Success
import fastparse.Parsed.Failure

import parser.{numberP, YadlInt, YadlFloat}

class NumberParsing extends munit.FunSuite {

  test("case '0'") {
    val input = "0"
    val expected = YadlInt(0)
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
    val expected = YadlInt(0)
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
    val expected = YadlInt(1000)
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
    val expected = YadlInt(0)
    parse(input, numberP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '0_.0' (should fail)") {
    val input = "0_.0"
    parse(input, numberP(using _)) match {
      case Success(_, index) =>
        val tmp = index == input.length
        assert(!tmp, "parsing succeded where it should have failed")
      case _: Failure =>
        assert(true, "expected failure")
    }
  }

  test("case '0.01'") {
    val input = "0.01"
    val expected = YadlFloat(0.01)
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
    val expected = YadlFloat(0.001234)
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
    val expected = YadlFloat(0.000001)
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
    val expected = YadlFloat(109.8125)
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
    val expected = YadlFloat(0.8125)
    parse(input, numberP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '0b1101101.' (should fail)") {
    val input = "0b1101101."
    parse(input, numberP(using _)) match {
      case Success(_, index) =>
        val tmp = index == input.length
        assert(!tmp, "parsing succeded where it should have failed")
      case _: Failure =>
        assert(true, "expected failure")
    }
  }

  test("case '0x10aaff.22ff'") {
    val input = "0x10aaff.22ff"
    val expected = YadlFloat(1092351.1367034912)
    parse(input, numberP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '0x10AAFF.F2'") {
    val input = "0x10AAFF.F2"
    val expected = YadlFloat(1092351.9453125)
    parse(input, numberP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '0x.' (should fail)") {
    val input = "0x."
    parse(input, numberP(using _)) match {
      case Success(_, index) =>
        assertEquals(index, input.length, "input has not been parsed fully")
        assert(
          false,
          "unexpected success: number does not have any digits and thus should fail"
        )
      case _: Failure =>
        assert(true)
    }
  }
}
