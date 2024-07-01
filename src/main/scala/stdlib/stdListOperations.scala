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

private def groupByBuiltIn(params: Seq[DataObject]): DataObject = {
  if (params.length != 2 || !params(1).isInstanceOf[FunctionObj]) {
    throw IllegalArgumentException()
  }

  val groupByFn = params(1).asInstanceOf[FunctionObj]
  val it = toIteratorObj(params(0))
  val result = new DictionaryObj(scala.collection.mutable.HashMap[DataObject, DataObject]())

  while (it.hasNext.function(Seq(it.data)).asInstanceOf[BooleanObj].value) {
    val item = it.next.function(Seq(it.data))
    val key = groupByFn.function(Seq(item))

    if (!result.value.contains(key)) {
      result.value(key) = new ListObj(scala.collection.mutable.ArrayBuffer[DataObject]())
    }
    result.value(key).asInstanceOf[ListObj].value.append(item)
  }

  result
}

private def reduceBuiltIn(params: Seq[DataObject]): DataObject = {
  if (params.length != 2 || !params(1).isInstanceOf[FunctionObj]) {
    throw IllegalArgumentException()
  }

  val reduceFn = params(1).asInstanceOf[FunctionObj]
  val it = toIteratorObj(params(0))

  if (!it.hasNext.function(Seq(it.data)).asInstanceOf[BooleanObj].value) {
    throw IllegalArgumentException("Cannot reduce an empty iterable")
  }

  var accumulator = it.next.function(Seq(it.data))

  while (it.hasNext.function(Seq(it.data)).asInstanceOf[BooleanObj].value) {
    val item = it.next.function(Seq(it.data))
    accumulator = reduceFn.function(Seq(accumulator, item))
  }

  accumulator
}