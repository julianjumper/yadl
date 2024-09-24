import fastparse._
import fastparse.Parsed.Success
import fastparse.Parsed.Failure

import parser.{binaryOpExpression, identifierP, Number}

def binary[$: P] =
  binaryOpExpression(identifierP, 0)

class OperatorPrecedence extends munit.FunSuite {

  test("case '3 + 4 * 5'") {
    val input = "3 + 4 * 5"
    val expected = 23.0
    parse(input, binary(using _)) match {
      case Success(result, index) =>
        val resultScope = evalValue(result, new Scope)
        val Some(Number(value)) = resultScope.result: @unchecked
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '(3 + 4) * 5'") {
    val input = "(3 + 4) * 5"
    val expected = 35.0
    parse(input, binary(using _)) match {
      case Success(result, index) =>
        val resultScope = evalValue(result, new Scope)
        val Some(Number(value)) = resultScope.result: @unchecked
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '3 + 4 * 5 ^ 6'") {
    val input = "3 + 4 * 5 ^ 6"
    val expected = scala.math.pow(5, 6) * 4 + 3
    parse(input, binary(using _)) match {
      case Success(result, index) =>
        val resultScope = evalValue(result, new Scope)
        val Some(Number(value)) = resultScope.result: @unchecked
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '3 + (4 * 5) ^ 6'") {
    val input = "3 + (4 * 5) ^ 6"
    val expected = scala.math.pow(20, 6) + 3
    parse(input, binary(using _)) match {
      case Success(result, index) =>
        val resultScope = evalValue(result, new Scope)
        val Some(Number(value)) = resultScope.result: @unchecked
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '(3 + 4) * 5 ^ 6'") {
    val input = "(3 + 4) * 5 ^ 6"
    val expected = scala.math.pow(5, 6) * 7
    parse(input, binary(using _)) match {
      case Success(result, index) =>
        val resultScope = evalValue(result, new Scope)
        val Some(Number(value)) = resultScope.result: @unchecked
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '(3 + 4 * 5) ^ 6'") {
    val input = "(3 + 4 * 5) ^ 6"
    val expected = scala.math.pow(23, 6)
    parse(input, binary(using _)) match {
      case Success(result, index) =>
        val resultScope = evalValue(result, new Scope)
        val Some(Number(value)) = resultScope.result: @unchecked
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '((3 + 4) * 5) ^ 6'") {
    val input = "((3 + 4) * 5) ^ 6"
    val expected = scala.math.pow(35, 6)
    parse(input, binary(using _)) match {
      case Success(result, index) =>
        val resultScope = evalValue(result, new Scope)
        val Some(Number(value)) = resultScope.result: @unchecked
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '1 - 2 / 3 + 4 * 5 ^ 6'") {
    val input = "1 - 2 / 3 + 4 * 5 ^ 6"
    val expected = 1 - 2.0 / 3.0 + 4 * scala.math.pow(5, 6)
    parse(input, binary(using _)) match {
      case Success(result, index) =>
        val resultScope = evalValue(result, new Scope)
        val Some(Number(value)) = resultScope.result: @unchecked
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '(1 - 2) / 3 + 4 * 5 ^ 6'") {
    val input = "(1 - 2) / 3 + 4 * 5 ^ 6"
    val expected = (-1 / 3.0) + 4 * scala.math.pow(5, 6)
    parse(input, binary(using _)) match {
      case Success(result, index) =>
        val resultScope = evalValue(result, new Scope)
        val Some(Number(value)) = resultScope.result: @unchecked
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '1 - 2 / (3 + 4) * 5 ^ 6'") {
    val input = "1 - 2 / (3 + 4) * 5 ^ 6"
    val expected = 1 - 2.0 / (3.0 + 4) * scala.math.pow(5, 6)
    parse(input, binary(using _)) match {
      case Success(result, index) =>
        val resultScope = evalValue(result, new Scope)
        val Some(Number(value)) = resultScope.result: @unchecked
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '(1 - 2) / (3 + 4) * 5 ^ 6'") {
    val input = "(1 - 2) / (3 + 4) * 5 ^ 6"
    val expected = (1 - 2.0) / (3.0 + 4) * scala.math.pow(5, 6)
    parse(input, binary(using _)) match {
      case Success(result, index) =>
        val resultScope = evalValue(result, new Scope)
        val Some(Number(value)) = resultScope.result: @unchecked
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '1 - (2 / 3 + 4) * 5 ^ 6'") {
    val input = "1 - (2 / 3 + 4) * 5 ^ 6"
    val expected = 1 - (2.0 / 3.0 + 4) * scala.math.pow(5, 6)
    parse(input, binary(using _)) match {
      case Success(result, index) =>
        val resultScope = evalValue(result, new Scope)
        val Some(Number(value)) = resultScope.result: @unchecked
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '1 - 2 / 3 + 4 * 5 ^ 6 + 7'") {
    val input = "1 - 2 / 3 + 4 * 5 ^ 6 + 7"
    val expected = 1 - 2.0 / 3.0 + 4 * scala.math.pow(5, 6) + 7
    parse(input, binary(using _)) match {
      case Success(result, index) =>
        val resultScope = evalValue(result, new Scope)
        val Some(Number(value)) = resultScope.result: @unchecked
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '1 - 2 / (3 + 4) * 5 ^ (6 + 7)'") {
    val input = "1 - 2 / (3 + 4) * 5 ^ (6 + 7)"
    val expected = 1 - 2.0 / (3.0 + 4) * scala.math.pow(5, 6 + 7)
    parse(input, binary(using _)) match {
      case Success(result, index) =>
        val resultScope = evalValue(result, new Scope)
        val Some(Number(value)) = resultScope.result: @unchecked
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }

  test("case '1 - 2 / 3 + 4 * 5 ^ 6 + 7 * 8'") {
    val input = "1 - 2 / 3 + 4 * 5 ^ 6 + 7 * 8"
    val expected = 1 - 2.0 / 3.0 + 4 * scala.math.pow(5, 6) + 7 * 8
    parse(input, binary(using _)) match {
      case Success(result, index) =>
        val resultScope = evalValue(result, new Scope)
        val Some(Number(value)) = resultScope.result: @unchecked
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"number parsing for test case '$input' failed")
    }
  }
}
