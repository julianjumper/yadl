package stdlib

import interpreterdata._

/**
 * Temporary example implementation of a basic add function.
 */
private def sum2(params: Seq[DataObject]): DataObject = params match {
  case Seq(NumberObj(a), NumberObj(b)) => NumberObj(a + b)
  case _ => throw IllegalArgumentException()
}