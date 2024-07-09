import fastparse._
import fastparse.Parsed.Success
import fastparse.Parsed.Failure

import parser.{stringP, StdString, FormatString, Identifier}

class StringParsing extends munit.FunSuite {

  test("case 0") {
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

  test("case 1") {
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

  test("case 2") {
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

  test("case 3") {
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

  test("case 4") {
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

  test("case 5") {
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

  test("case 6") {
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

  test("case 7") {
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

  test("case 8") {
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

  test("case 9") {
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

  test("case 10") {
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

// Weitere 45 korrekte Eingaben
  test("case 11") {
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

  test("case 12") {
    val input = "\"Unclosed string"
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assert(false, f"string parsing for test case '$input' failed")
      case _: Failure =>
        assert(true, f"string parsing for test case '$input' should Fail")
    }
  }

  test("case 13") {
    val input = "f\"Unclosed formatted string"
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assert(false, f"string parsing for test case '$input' failed")
      case _: Failure =>
        assert(true, f"string parsing for test case '$input' should Fail")
    }
  }

  test("case 14") {
    val input = "'''Unclosed multiline string"
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        print(value)
        assert(false, f"string parsing for test case '$input' failed")
      case _: Failure =>
        assert(true, f"string parsing for test case '$input' should Fail")
    }
  }

  test("case 15") {
    val input = "invalid\"String"
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assert(false, f"string parsing for test case '$input' failed")
      case _: Failure =>
        assert(true, f"string parsing for test case '$input' should Fail")
    }
  }

// Weitere 45 fehlerhafte Eingaben
  test("case 16") {
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

  test("case 17") {
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

  test("case 18") {
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

  test("case 19") {
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

  test("case 20") {
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

  test("case 21") {
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

  test("case 22") {
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

  test("case 23") {
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
  test("case 26") {
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

  test("case 27") {
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

  test("case 28") {
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

  test("case 29") {
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

  test("case 30") {
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

  test("case 31") {
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

  test("case 32") {
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

  test("case 33") {
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
  test("case 36") {
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

  test("case 37") {
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

  test("case 38") {
    val input = "f\"String with {escaped \\{braces\\}}\""
    val expected =
      FormatString(List(StdString("String with {escaped \\{braces\\}}")))
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assert(false, f"string parsing for test case '$input' failed")
      case _ =>
        assert(true, f"string parsing for test case '$input' should Fail")
    }
  }

  test("case 39") {
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

  test("case 40") {
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

  test("case 41") {
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

  test("case 42") {
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

  test("case 43") {
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
  test("case 46") {
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

  test("case 47") {
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

  test("case 48") {
    val input = "f\"String with {escaped \\{braces\\}}\""
    val expected =
      FormatString(List(StdString("String with {escaped \\{braces\\}}")))
    parse(input, stringP(using _)) match {
      case Success(value, index) =>
        assert(false, f"string parsing for test case '$input' failed")
      case _ =>
        assert(true, f"string parsing for test case '$input' should Fail")
  }

  test("case 49") {
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

  test("case 50") {
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

  test("case 51") {
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

  test("case 52") {
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

  test("case 53") {
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

}
}