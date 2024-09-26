import fastparse._
import fastparse.Parsed.Success
import fastparse.Parsed.Failure

import parser.{
  functionCallExpression,
  identifierP,
  expression,
  FunctionCall,
  Function,
  Identifier,
  Wrapped,
  Return
}

def funCall[$: P] =
  functionCallExpression(identifierP)

// def expr[$: P] =
//   expression(identifierP)

class FunctionCalls extends munit.FunSuite {

  test("case 'print((() => x)())'") {
    val input = "print((() => x)())"
    val expected =
      FunctionCall(
        Identifier("print"),
        Seq(
          FunctionCall(
            Wrapped(Function(Seq(), Seq(Return(Identifier("x"))))),
            Seq()
          )
        )
      )
    parse(input, funCall(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case f: Failure =>
        val msg = f.trace().longAggregateMsg
        assert(
          false,
          f"function call parsing for test case '$input' failed:\n    $msg"
        )
    }
  }

  test("case 'f(f(x))'") {
    val input = "f(f(x))"
    val expected =
      FunctionCall(
        Identifier("f"),
        Seq(
          FunctionCall(
            Identifier("f"),
            Seq(Identifier("x"))
          )
        )
      )
    parse(input, funCall(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case f: Failure =>
        val msg = f.trace().longAggregateMsg
        assert(
          false,
          f"function call parsing for test case '$input' failed:\n    $msg"
        )
    }
  }

  test("case 'f()()'") {
    val input = "f()()"
    val expected =
      FunctionCall(
        FunctionCall(
          Identifier("f"),
          Seq()
        ),
        Seq()
      )
    parse(input, funCall(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case f: Failure =>
        val msg = f.trace().longAggregateMsg
        assert(
          false,
          f"function call parsing for test case '$input' failed:\n    $msg"
        )
    }
  }

  test("case '(() => x)()'") {
    val input = "(() => x)()"
    val expected =
      FunctionCall(
        Wrapped(Function(Seq(), Seq(Return(Identifier("x"))))),
        Seq()
      )
    parse(input, funCall(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case f: Failure =>
        val msg = f.trace().longAggregateMsg
        assert(
          false,
          f"function call parsing for test case '$input' failed:\n    $msg"
        )
    }
  }

  test("case '((() => x)())'") {
    val input = "((() => x)())"
    val expected =
      Wrapped(
        FunctionCall(
          Wrapped(Function(Seq(), Seq(Return(Identifier("x"))))),
          Seq()
        )
      )
    parse(input, expr(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case f: Failure =>
        val msg = f.trace().longAggregateMsg
        assert(
          false,
          f"function call parsing for test case '$input' failed:\n    $msg"
        )
    }
  }

  test("case '(() => x)(y)' - expression") {
    val input = "(() => x)(y)"
    val expected =
      FunctionCall(
        Wrapped(Function(Seq(), Seq(Return(Identifier("x"))))),
        Seq(Identifier("y"))
      )
    parse(input, expr(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case f: Failure =>
        val msg = f.trace().longAggregateMsg
        assert(
          false,
          f"function call parsing for test case '$input' failed:\n    $msg"
        )
    }
  }

  test("case '(() => x)(y)' - functionCallP") {
    val input = "(() => x)(y)"
    val expected =
      FunctionCall(
        Wrapped(Function(Seq(), Seq(Return(Identifier("x"))))),
        Seq(Identifier("y"))
      )
    parse(input, funCall(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case f: Failure =>
        val msg = f.trace().longAggregateMsg
        assert(
          false,
          f"function call parsing for test case '$input' failed:\n    $msg"
        )
    }
  }

  test("case '(x)(y)'") {
    val input = "(x)(y)"
    val expected =
      FunctionCall(
        Wrapped(Identifier("x")),
        Seq(Identifier("y"))
      )
    parse(input, funCall(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case f: Failure =>
        val msg = f.trace().longAggregateMsg
        assert(
          false,
          f"function call parsing for test case '$input' failed:\n    $msg"
        )
    }
  }
}
