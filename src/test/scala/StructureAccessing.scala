import fastparse._
import fastparse.Parsed.Success
import fastparse.Parsed.Failure

class StructureAccessing extends munit.FunSuite {
  test("case 'hello[5]'") {
    val input = "hello[5]"
    val expected = StructureAccess(Identifier("hello"), Number(5))
    parse(input, structureAccess(using _)) match {
      case Success(value, index) =>
        assertEquals(value, expected)
        assertEquals(index, input.length, "input has not been parsed fully")
      case f: Failure =>
        val msg = f.trace().longAggregateMsg
        assert(
          false,
          f"structure access parsing for test case '$input' failed\n  msg: $msg"
        )
    }
  }
}
