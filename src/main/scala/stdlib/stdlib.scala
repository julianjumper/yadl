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
    .addOne("load", FunctionObj(Seq("path", "format"), Seq(), None, loadFunction))
    .addOne("check_all", FunctionObj(Seq("iterable", "checkFunction"), Seq(), None, check_allBuiltIn))
    .addOne("check_any", FunctionObj(Seq("iterable", "checkFunction"), Seq(), None, check_anyBuiltIn))
    .addOne("check_none", FunctionObj(Seq("iterable", "checkFunction"), Seq(), None, check_noneBuiltIn))
    .addOne("first",FunctionObj(Seq("iterable", "fn", "default"), Seq(), None, firstBuiltIn))
    .addOne("last",FunctionObj(Seq("iterable", "fn", "default"), Seq(), None, lastBuiltIn))
    .addOne("count",FunctionObj(Seq("iterable", "target"),Seq(),None, countBuiltIn))
    .addOne("zip",FunctionObj(Seq("iterable1", "iterable2"),Seq(),None,zipBuiltIn))
    .addOne("do", FunctionObj(Seq("iterable", "function"), Seq(), None, doBuiltIn))
    .addOne("len", FunctionObj(Seq("iterable"), Seq(), None, lenBuiltIn))
    .addOne("groupBy", FunctionObj(Seq("iterable", "groupByFunction"), Seq(), None, groupByBuiltIn))
    .addOne("reduce", FunctionObj(Seq("iterable", "reduceFunction"), Seq(), None, reduceBuiltIn))
    .addOne("map", FunctionObj(Seq("iterable", "mapFunction"), Seq(), None, mapBuiltIn))
  
}
