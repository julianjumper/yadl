import fastparse._, NoWhitespace._
import java.io.*
import java.{util => ju}

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
def readFileContent(filepath: String) =
  var file = new File(filepath);

  var fis = FileInputStream(file);
  var baos = ByteArrayOutputStream()
  var buffer = Array.ofDim[scala.Byte](1024);
  var bytesRead = 0;

  bytesRead = fis.read(buffer)
  while (bytesRead != -1) {
    baos.write(buffer, 0, bytesRead);
    bytesRead = fis.read(buffer)
  }
  baos.toString();

object Main {
  def main(args: Array[String]): Unit =
    for (f <- args)
      val content = readFileContent(f)
      val result = parse(content, fileP(using _)): @unchecked
      result match {
        case Parsed.Success(v, _) =>
          val context = v.foldLeft(new HashMap[String, Value])((acc, st) =>
            evalStatement(st, acc)
          )
        case Parsed.Failure(v, s, s2) =>
          val fail = Parsed.Failure(v, s, s2)
          val trace = fail.trace().longAggregateMsg
          println(trace)
      }
}
