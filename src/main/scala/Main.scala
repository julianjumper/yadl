import fastparse._, NoWhitespace._
import java.io.*
import java.{util => ju}

// Some operators should be calculated before other operators.
// eg. 4 - 4 * 4 => 4*4 gets calculated before 4-4.
// So the "precedence" of * is higher than of -. This is handled here.
def precedence(op: ArithmaticOps) = op match {
  case ArithmaticOps.Add  => 4
  case ArithmaticOps.Sub  => 4
  case ArithmaticOps.Mul  => 5
  case ArithmaticOps.Div  => 5
  case ArithmaticOps.Expo => 6
}

def precedence(op: BooleanOps) = op match {
  case BooleanOps.And => 2
  case BooleanOps.Or  => 1
  case BooleanOps.Not => 3
}

def precedenceOf(value: Value): Int = value match {
  case BinaryOp(_, ArithmaticOp(op), _) => precedence(op)
  case BinaryOp(_, BooleanOp(op), _)    => precedence(op)
  case BinaryOp(_, CompareOp(_), _)     => 0
  case Wrapped(v)                       => 10 + precedenceOf(v)
  case _                                => 100000
}

// Thank you Java (-_-)
def readFileContent(filepath: String): String =
  var file = new File(filepath);

  var fis = FileInputStream(file);
  var baos = ByteArrayOutputStream()
  var buffer = Array.ofDim[scala.Byte](1024);
  var bytesRead = 0;

  bytesRead = fis.read(buffer)
  while (bytesRead != -1) {
    baos.write(buffer, 0, bytesRead)
    bytesRead = fis.read(buffer)
  }
  baos.toString

object Main {
  def main(args: Array[String]): Unit =
    // read each given file
    for (f <- args)
      // read file
      val content = readFileContent(f)
      // parse this file using the starting rule `fileP`
      val result = parse(content, fileP(using _)): @unchecked
      result match {
        case Parsed.Success(stmt_seq, _) =>
          // parsing successful, ready to be interpreted
          // This is done by folding over the sequence (seq) of statements (stmt) that we got from the parser.
          // The first statement is interpreted and new variables are stored in an empty Scope.
          // This Scope gets passed to the next statement and so on. So the Scope gets updated with each statement.
          val context = stmt_seq.foldLeft(new Scope)(
            evalStatement // interpret current statement
          )
        case Parsed.Failure(v, s, s2) =>
          // parsing was not successful.
          val fail = Parsed.Failure(v, s, s2)
          val trace = fail.trace().longAggregateMsg
          println(trace)
      }
}
