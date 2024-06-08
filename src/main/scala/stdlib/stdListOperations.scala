package stdlib

import interpreterdata.*

import scala.collection.mutable

private def filterBuiltIn(params: Seq[DataObject]): DataObject = {
  if (params.length != 2 || !params(1).isInstanceOf[FunctionObj]) {
    throw IllegalArgumentException()
  }
  
  val filterFn = params(1).asInstanceOf[FunctionObj]
  
  // We can cheat and just don't use the the data attribute of the
  // iterator, since it can't be accessed by the language in the
  // first place.
  val d = DictionaryObj(mutable.HashMap[DataObject, DataObject]())

  val it = toIteratorObj(params(0))
  var buffer: DataObject = NONE
  var hasBuffer = false

  

  val hasnext = FunctionObj(Seq("d"), Seq(), None, (d: Seq[DataObject]) => {
    if (hasBuffer) {
      TRUE
    } 
    while (it.hasNext.function(Seq(it.data)).asInstanceOf[BooleanObj].value) {
      val n = it.next.function(Seq(it.data))
      if (filterFn.function(Seq(n)).asInstanceOf[BooleanObj].value) {
        buffer = n
        hasBuffer = true
        TRUE
      }
    }
    FALSE
  })

  val next = FunctionObj(Seq("d"), Seq(), None, (d: Seq[DataObject]) => {
    if (hasBuffer) {
      hasBuffer = false
      buffer
    } else if (hasnext.function(d).asInstanceOf[BooleanObj].value) {
        hasBuffer = false
        buffer
    }
    else {
      throw IllegalArgumentException()
    }
  })
  
  IteratorObj(next, hasnext, d)
}
