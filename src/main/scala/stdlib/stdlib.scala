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
    .addOne("number", FunctionObj(Seq("object"), Seq(), None, (params: Seq[DataObject]) => params match {
      case Seq(x) => toNumberObj(x)
      case _ => throw IllegalArgumentException()
    }))
    .addOne("string", FunctionObj(Seq("object"), Seq(), None, (params: Seq[DataObject]) => params match {
      case Seq(x) => toStringObj(x)
      case _ => throw IllegalArgumentException()
    }))
    .addOne("list", FunctionObj(Seq("object"), Seq(), None, (params: Seq[DataObject]) => params match {
      case Seq(x) => toListObj(x)
      case _ => throw IllegalArgumentException()
    }))
    // Cannot be called correctly right now.
//    .addOne("print2", FunctionObj(Seq(), Seq(("end", StringObj("\n")), ("separator", StringObj(", "))), Some("objectsToPrint"), (params: Seq[DataObject]) => params match {
//      case Seq(StringObj(end), StringObj(separator), ListObj(printables)) => {
//        val str = (printables.map(p => toStringObj(p).value) mkString separator) + end
//        System.out.print(str)
//        NONE
//      }
//      case _ => throw IllegalArgumentException()
//    }))
    .addOne("print3", FunctionObj(Seq("obj"), Seq(), None, (params: Seq[DataObject]) =>
      params match {
      case Seq(x) => {
        val str = toStringObj(x).value + "\n"
        System.out.print(str)
        NONE
      }
      case _ => throw IllegalArgumentException()
    }))
    .addOne("check_all", FunctionObj(Seq("iterable", "checkFunction"), Seq(), None, check_allBuiltIn))
    .addOne("check_any", FunctionObj(Seq("iterable", "checkFunction"), Seq(), None, check_anyBuiltIn))
    .addOne("check_none", FunctionObj(Seq("iterable", "checkFunction"), Seq(), None, check_noneBuiltIn))
}
