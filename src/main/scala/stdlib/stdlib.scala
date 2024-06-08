package stdlib

import interpreterdata._

private type HashMap[K, V] = scala.collection.mutable.HashMap[K, V]

/**
 * TODO: we should find a way to add new functions without having to modify this file.
 *    But since the interface of the stdlib function wont change we can leave it like this for now.
 * This is THE way to supply prebuild functions to the interpreter.
 * @return A map of all function names and their corresponding Function Objects.
 */
def stdlib: HashMap[String, FunctionObj] = {
  new HashMap[String, FunctionObj]
    .addOne("sum2", FunctionObj(Seq("a", "b"), Seq(), None, sum2))
    
    .addOne("filter", FunctionObj(Seq("iterable", "filterFunction"), Seq(), None, filterBuiltIn))
    
    .addOne("string", FunctionObj(Seq("object"), Seq(), None, {
      case Seq(x) => toStringObj(x)
      case _ => throw IllegalArgumentException()
    }))
    .addOne("bool", FunctionObj(Seq("object"), Seq(), None, {
      case Seq(x) => toBooleanObj(x)
      case _ => throw IllegalArgumentException()
    }))
    .addOne("type", FunctionObj(Seq("object"), Seq(), None, (params: Seq[DataObject]) => params match {
      case Seq(x) => StringObj(x.typeName)
      case _ => throw IllegalArgumentException()
    }))
}
