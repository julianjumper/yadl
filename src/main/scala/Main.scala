import fastparse._, NoWhitespace._
import java.io.*
import java.{util => ju}

import parser.{fileP}

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
          scala.sys.error(trace)
      }
}
