import fastparse._
import fastparse.Parsed.Success
import fastparse.Parsed.Failure

import parser.{stringP, StdString, FormatString, Identifier}

class StringParsing extends munit.FunSuite {

  test("simple String 1") {
    val input = "'a'"
    val expected = StdString("a")
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("simple String 2") {
    val input = "\"try_this$_&*\""
    val expected = StdString("try_this$_&*")
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("simple String 3") {
    val input = "\"Hallo Welt\""
    val expected = StdString("Hallo Welt")
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("simple String 4") {
    val input = "'Hello World!'"
    val expected = StdString("Hello World!")
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("simple String 5") {
    val input = "\"Simple String\""
    val expected = StdString("Simple String")
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("simple String 6") {
    val input = "'Another simple string'"
    val expected = StdString("Another simple string")
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("simple String 7") {
    val input = "'String with single quotes'"
    val expected = StdString("String with single quotes")
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

// Weitere 40 korrekte Eingaben
  test("simple String 8") {
    val input = "\"Simple String\""
    val expected = StdString("Simple String")
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("simple String 9") {
    val input = "'Another simple string'"
    val expected = StdString("Another simple string")
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("simple String 10") {
    val input = "'String with single quotes'"
    val expected = StdString("String with single quotes")
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("String with escape Sequence 1") {
    val input = "'abcd123454_\\nnewline'"
    val expected = StdString("abcd123454_\nnewline")
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' should not Fail")
    }
  }

  test("String with escape Sequence 2") {
    val input = "'testEscape_Tab:\\tEscapeBackslash:\\\\'"
    val expected = StdString("testEscape_Tab:\tEscapeBackslash:\\")
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("String with escape Sequence 3") {
    val input = "\"\\\"Quoted\\\"\""
    val expected = StdString("\"Quoted\"")
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("String with escape Sequence 4") {
    val input = "'abcd123454_\\nnewline'"
    val expected = StdString("abcd123454_\nnewline")
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("String with escape Sequence 5") {
    val input = "\"String with \\\"escaped quotes\\\"\""
    val expected = StdString("String with \"escaped quotes\"")
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("String with escape Sequence 6") {
    val input = "\"String with \\n newline\""
    val expected = StdString("String with \n newline")
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("String with escape Sequence 7") {
    val input = "\"String with \\\"escaped quotes\\\"\""
    val expected = StdString("String with \"escaped quotes\"")
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("String with escape Sequence 8") {
    val input = "\"String with \\n newline\""
    val expected = StdString("String with \n newline")
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("String with escape Sequence 9") {
    val input = "\"Tab character \\t in string\""
    val expected = StdString("Tab character \t in string")
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("String with escape Sequence 10") {
    val input = "'String with escaped backslash \\\\'"
    val expected = StdString("String with escaped backslash \\")
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("String with escape Sequence 11") {
    val input = "\"String with \\r carriage return\""
    val expected = StdString("String with \r carriage return")
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("String with escape Sequence 12") {
    val input = "\"String with hex \\x41 character\""
    val expected = StdString("String with hex \\x41 character")
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("String with escape Sequence 13") {
    val input = "\"String with \\b backspace\""
    val expected = StdString("String with \b backspace")
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("String with escape Sequence 14") {
    val input = "'String with multiple \\nescaped \\ncharacters'"
    val expected = StdString("String with multiple \nescaped \ncharacters")
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

// Weitere 10 korrekte Eingaben
  test("String with escape Sequence 15") {
    val input = "\"Tab character \\t in string\""
    val expected = StdString("Tab character \t in string")
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("String with escape Sequence 16") {
    val input = "'String with escaped backslash \\\\'"
    val expected = StdString("String with escaped backslash \\")
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("String with escape Sequence 17") {
    val input = "\"String with \\r carriage return\""
    val expected = StdString("String with \r carriage return")
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("String with escape Sequence 18") {
    val input = "\"String with hex \\x41 character\""
    val expected = StdString("String with hex \\x41 character")
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("String with escape Sequence 19") {
    val input = "\"String with \\b backspace\""
    val expected = StdString("String with \b backspace")
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("String with escape Sequence 20") {
    val input = "'String with multiple \\nescaped \\ncharacters'"
    val expected = StdString("String with multiple \nescaped \ncharacters")
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("Mulitline String 1") {
    val input = "\'\'\'Multilinecode\\nnewLine\'\'\'"
    val expected = StdString("Multilinecode\nnewLine")
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("Mulitline String 2") {
    val input = "\"\"\"Multilinecode\\nnewLine\\tTab\"\"\""
    val expected = StdString("Multilinecode\nnewLine\tTab")
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("Mulitline String 3") {
    val input = "'''this is a multiline string!!\nok\n'''"
    val expected = StdString("this is a multiline string!!\nok\n")
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("Multiline String 4") {
    val input = "'''Multi-line\nstring'''"
    val expected = StdString("Multi-line\nstring")
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("Mulitline String 5") {
    val input = "'''Multi-line\nstring'''"
    val expected = StdString("Multi-line\nstring")
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("Multiline String 6") {
    val input = "'''Multi-line string with indentation\n  and extra spaces'''"
    val expected =
      StdString("Multi-line string with indentation\n  and extra spaces")
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("Mulitline String 7") {
    val input = "'''Multi-line string with indentation\n  and extra spaces'''"
    val expected =
      StdString("Multi-line string with indentation\n  and extra spaces")
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("format String 1") {
    val input = "f\"Moin {name}!\""
    val expected = FormatString(
      List(StdString("Moin "), Identifier("name"), StdString("!"))
    )
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("format String 2") {
    val input = "f\"String with {variable}\""
    val expected =
      FormatString(List(StdString("String with "), Identifier("variable")))
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("format String 3") {
    val input = "f\"{greeting}, {name}!\""
    val expected = FormatString(
      List(
        StdString(""),
        Identifier("greeting"),
        StdString(", "),
        Identifier("name"),
        StdString("!")
      )
    )
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("format String 4") {
    val input = "f\"String with {variable}\""
    val expected =
      FormatString(List(StdString("String with "), Identifier("variable")))
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("format String 5") {
    val input = "f\"{greeting}, {name}!\""
    val expected = FormatString(
      List(
        StdString(""),
        Identifier("greeting"),
        StdString(", "),
        Identifier("name"),
        StdString("!")
      )
    )
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(false, f"string parsing for test case '$input' failed")
    }
  }

  test("false String 1") {
    val input = "\"Unclosed string"
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assert(false, f"string parsing for test case '$input' failed")
      case _: Failure =>
        assert(true, f"string parsing for test case '$input' should Fail")
    }
  }

  test("false String 2") {
    val input = "f\"Unclosed formatted string"
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assert(false, f"string parsing for test case '$input' failed")
      case _: Failure =>
        assert(true, f"string parsing for test case '$input' should Fail")
    }
  }

  test("false String 3") {
    val input = "'''Unclosed multiline string"
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assertNotEquals(index, input.length, "input has not been parsed fully")
      case _: Failure =>
        assert(true, f"string parsing for test case '$input' should Fail")
    }
  }

  test("false String 4") {
    val input = "invalid\"String"
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assert(false, f"string parsing for test case '$input' failed")
      case _: Failure =>
        assert(true, f"string parsing for test case '$input' should Fail")
    }
  }

  test("false String 5") {
    val input = "f\"String with {escaped \\{braces\\}}\""
    val expected =
      FormatString(List(StdString("String with {escaped \\{braces\\}}")))
    try {
      parse(input, stringP(using _))
    } catch {
      case _: java.lang.AssertionError =>
        assert(true, "shout throw an assertion")
      case assertion => throw assertion
    }
  }
}
