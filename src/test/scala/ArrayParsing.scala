import fastparse._
import fastparse.Parsed.Success
import fastparse.Parsed.Failure

import parser.{
  ArrayLiteral,
  Number,
  arrayLiteralP,
  arrayAccessP,
  ArrayAccess,
  ArithmaticOp,
  ArithmaticOps,
  BinaryOp
}

class ArrayParsing extends munit.FunSuite {

  // Test cases for array literals
  test("empty array") {
    val input = "[]"
    val expected = ArrayLiteral(Seq())
    parse(input, arrayLiteralP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"array parsing for test case '$input' failed")
    }
  }

  test("single element array") {
    val input = "[1]"
    val expected = ArrayLiteral(Seq(Number(1)))
    parse(input, arrayLiteralP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"array parsing for test case '$input' failed")
    }
  }

  test("multi-element array") {
    val input = "[1, 2, 3]"
    val expected = ArrayLiteral(Seq(Number(1), Number(2), Number(3)))
    parse(input, arrayLiteralP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"array parsing for test case '$input' failed")
    }
  }

  test("nested arrays") {
    val input = "[[1, 2], [3, 4]]"
    val expected = ArrayLiteral(
      Seq(
        ArrayLiteral(Seq(Number(1), Number(2))),
        ArrayLiteral(Seq(Number(3), Number(4)))
      )
    )
    parse(input, arrayLiteralP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"array parsing for test case '$input' failed")
    }
  }

  // Test cases for array access
  test("simple array access") {
    val input = "arr [0]"
    val expected = ArrayAccess("arr", Number(0))
    parse(input, arrayAccessP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"array access parsing for test case '$input' failed")
    }
  }

  test("array access with expression index") {
    val input = "arr [1 + 1]"
    val expected = ArrayAccess(
      "arr",
      BinaryOp(Number(1), ArithmaticOp(ArithmaticOps.Add), Number(1))
    )
    parse(input, arrayAccessP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"array access parsing for test case '$input' failed")
    }
  }

  // Invalid array literal test cases
  test("invalid array literal - missing closing bracket") {
    val input = "[1, 2, 3"
    parse(input, arrayLiteralP(using _)) match {
      case Success(_, index) =>
        assert(false, f"parsing succeeded where it should have failed")
      case _: Failure =>
        assert(true, "expected failure")
    }
  }

  test("invalid array literal - extra comma") {
    val input = "[1, 2, 3,]"
    parse(input, arrayLiteralP(using _)) match {
      case Success(_, index) =>
        assert(false, f"parsing succeeded where it should have failed")
      case _: Failure =>
        assert(true, "expected failure")
    }
  }

  // Invalid array access test cases
  test("invalid array access - missing closing bracket") {
    val input = "arr[0"
    parse(input, arrayAccessP(using _)) match {
      case Success(_, index) =>
        assert(false, f"parsing succeeded where it should have failed")
      case _: Failure =>
        assert(true, "expected failure")
    }
  }

  test("invalid array access - non-integer index") {
    val input = "arr[foo]"
    parse(input, arrayAccessP(using _)) match {
      case Success(_, index) =>
        assert(false, f"parsing succeeded where it should have failed")
      case _: Failure =>
        assert(true, "expected failure")
    }
  }
}

