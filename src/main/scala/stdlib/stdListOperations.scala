package stdlib

import interpreterdata.*

import scala.collection.mutable
import scala.util.boundary, boundary.break


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
    
    boundary {
      while (it.hasNext.function(Seq(it.data)).asInstanceOf[BooleanObj].value) {
        val n = it.next.function(Seq(it.data))

        if (filterFn.function(Seq(n)).asInstanceOf[BooleanObj].value) {
          buffer = n
          hasBuffer = true
          break(TRUE)
        }
      }
      FALSE
    }
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


private def check_allBuiltIn(params: Seq[DataObject]): BooleanObj = {
  if (params.length != 2 || !params(1).isInstanceOf[FunctionObj]) {
    throw IllegalArgumentException()
  }

  val d = DictionaryObj(mutable.HashMap[DataObject, DataObject]())

  val checkFn = params(1).asInstanceOf[FunctionObj]
  val it = toIteratorObj(params(0))

  while (it.hasNext.function(Seq(it.data)).asInstanceOf[BooleanObj].value) {
    val nextElement = it.next.function(Seq(it.data))
    val checkResult = checkFn.function(Seq(nextElement)).asInstanceOf[BooleanObj].value
    if (!checkResult) {
      return FALSE
    }
  }
  TRUE
}


private def check_anyBuiltIn(params: Seq[DataObject]): BooleanObj = {
  if (params.length != 2 || !params(1).isInstanceOf[FunctionObj]) {
    throw IllegalArgumentException()
  }

  val d = DictionaryObj(mutable.HashMap[DataObject, DataObject]())

  val checkFn = params(1).asInstanceOf[FunctionObj]
  val it = toIteratorObj(params(0))

  while (it.hasNext.function(Seq(it.data)).asInstanceOf[BooleanObj].value) {
    val nextElement = it.next.function(Seq(it.data))
    val checkResult = checkFn.function(Seq(nextElement)).asInstanceOf[BooleanObj].value
    if (checkResult) {
      return TRUE
    }
  }
  FALSE
}


private def check_noneBuiltIn(params: Seq[DataObject]): BooleanObj = {
  if (params.length != 2 || !params(1).isInstanceOf[FunctionObj]) {
    throw IllegalArgumentException()
  }

  val d = DictionaryObj(mutable.HashMap[DataObject, DataObject]())

  val checkFn = params(1).asInstanceOf[FunctionObj]
  val it = toIteratorObj(params(0))

  while (it.hasNext.function(Seq(it.data)).asInstanceOf[BooleanObj].value) {
    val nextElement = it.next.function(Seq(it.data))
    val checkResult = checkFn.function(Seq(nextElement)).asInstanceOf[BooleanObj].value
    if (checkResult) {
      return FALSE
    }
  }
  TRUE
}

private def firstBuiltIn(params: Seq[DataObject]): DataObject = {
  if (params.length < 1 || params.length > 3 || !params(1).isInstanceOf[FunctionObj]) {
    throw new IllegalArgumentException
  }

  val it = toIteratorObj(params(0)).asInstanceOf[IteratorObj]
  val fn = if (params.length > 1 && params(1).isInstanceOf[FunctionObj]) Some(params(1).asInstanceOf[FunctionObj]) else None
  val default = if (params.length == 3) Some(params(2)) else None

  while (it.hasNext.function(Seq(it.data)).asInstanceOf[BooleanObj].value) {
    val nextElement = it.next.function(Seq(it.data))
    if (fn.isEmpty || fn.get.function(Seq(nextElement)).asInstanceOf[BooleanObj].value) {
      return nextElement
    }
  }

  default.getOrElse(throw new NoSuchElementException("No element satisfies the condition"))
}

private def lastBuiltIn(params: Seq[DataObject]): DataObject = {
  if (params.length < 1 || params.length > 3 || !params(1).isInstanceOf[FunctionObj]) {
    throw new IllegalArgumentException
  }

  val it = toIteratorObj(params(0)).asInstanceOf[IteratorObj]
  val fn = if (params.length > 1 && params(1).isInstanceOf[FunctionObj]) Some(params(1).asInstanceOf[FunctionObj]) else None
  val default = if (params.length == 3) Some(params(2)) else None

  var lastElement: DataObject = default.getOrElse(UndefinedObj())
  var found = false

  while (it.hasNext.function(Seq(it.data)).asInstanceOf[BooleanObj].value) {
    val nextElement = it.next.function(Seq(it.data))
    if (fn.isEmpty || fn.get.function(Seq(nextElement)).asInstanceOf[BooleanObj].value) {
      lastElement = nextElement
      found = true
    }
  }

  if (found) {
    lastElement
  } else {
    default.getOrElse(throw new NoSuchElementException("No element satisfies the condition"))
  }
}

  
private def countBuiltIn(params: Seq[DataObject]): NumberObj = {
  if (params.length != 2 || !params(1).isInstanceOf[FunctionObj]) {
    throw IllegalArgumentException()
  }

  val d = DictionaryObj(mutable.HashMap[DataObject, DataObject]())

  val checkFn = params(1).asInstanceOf[FunctionObj]
  val it = toIteratorObj(params(0))
  var count = 0

  while (it.hasNext.function(Seq(it.data)).asInstanceOf[BooleanObj].value) {
    val nextElement = it.next.function(Seq(it.data))
    val checkResult = checkFn.function(Seq(nextElement)).asInstanceOf[BooleanObj].value
    if (checkResult) {
      count = count + 1
    }
  }
  val result = NumberObj(count)
  result
}

private def doBuiltIn(params: Seq[DataObject]): IteratorObj = {
  if (params.length != 2 || !params(1).isInstanceOf[FunctionObj]) {
    throw new IllegalArgumentException()
  }

  val fn = params(1).asInstanceOf[FunctionObj]
  val it = toIteratorObj(params(0)).asInstanceOf[IteratorObj]

  // Create a buffer to store the results
  val results = mutable.Buffer[DataObject]()

  while (it.hasNext.function(Seq(it.data)).asInstanceOf[BooleanObj].value) {
    val nextElement = it.next.function(Seq(it.data))
    val result = fn.function(Seq(nextElement))
    results += result
  }

  // Create a new iterator for the results
  val newIt = results.iterator

  // Functions to wrap the new iterator
  val hasNextFn = new FunctionObj(Seq(), Seq(), None, _ => BooleanObj(newIt.hasNext))
  val nextFn = new FunctionObj(Seq(), Seq(), None, _ => if (newIt.hasNext) newIt.next() else NoneObj())
  val data = DictionaryObj(mutable.HashMap())

  // Return the new IteratorObj
  new IteratorObj(nextFn, hasNextFn, data)
}
  

/*private def zipBuiltIn(params: Seq[DataObject]): IteratorObj = {
  // Check if there are exactly two parameters and both are iterators
  if (params.length != 2) {
    throw new IllegalArgumentException("LALALA")
  }

  // Extract the iterators
  val it1 = toIteratorObj(params(0)).asInstanceOf[IteratorObj]
  val it2 = toIteratorObj(params(1)).asInstanceOf[IteratorObj]

  // Initialize an empty ArrayBuffer to store the zipped results
  val zippedResults = mutable.ArrayBuffer.empty[List[DataObject]]

  // Iterate until either iterator is empty
  while (it1.hasNext.function(Seq(it1.data)).asInstanceOf[BooleanObj].value &&
         it2.hasNext.function(Seq(it2.data)).asInstanceOf[BooleanObj].value) {
    // Collect elements from each iterator into a List
    val tuple = List(it1.next.function(Seq(it1.data)), it2.next.function(Seq(it2.data)))
    zippedResults += tuple
  }

  // Convert zippedResults to an iterator
  val newIt = zippedResults.iterator

  // Functions to wrap the new iterator
  val hasNextFn = new FunctionObj(Seq(), Seq(), None, _ => BooleanObj(newIt.hasNext))
  val nextFn = new FunctionObj(Seq(), Seq(), None, _ => {
    if (newIt.hasNext) {
      val nextTuple = newIt.next()
      ListObj(mutable.ArrayBuffer(nextTuple: _*)) // Convert List to ArrayBuffer
    } else {
      NoneObj()
    }
  })
  val data = DictionaryObj(mutable.HashMap())

  // Return the new IteratorObj
  new IteratorObj(nextFn, hasNextFn, data)
}
*/
private def zipBuiltIn(params: Seq[DataObject]): IteratorObj = {
  // Check if there are exactly two parameters and both are iterators
  if (params.length != 2) {
    throw new IllegalArgumentException("zipBuiltIn expects exactly two parameters")
  }

  // Extract the iterators
  val it1 = toIteratorObj(params(0)).asInstanceOf[IteratorObj]
  val it2 = toIteratorObj(params(1)).asInstanceOf[IteratorObj]

  // Initialize an empty ArrayBuffer to store the zipped results
  val zippedResults = mutable.ArrayBuffer.empty[mutable.ArrayBuffer[DataObject]]

  // Iterate until either iterator is empty
  while (it1.hasNext.function(Seq(it1.data)).asInstanceOf[BooleanObj].value &&
         it2.hasNext.function(Seq(it2.data)).asInstanceOf[BooleanObj].value) {
    // Collect elements from each iterator into an ArrayBuffer
    val tuple = mutable.ArrayBuffer(
      it1.next.function(Seq(it1.data)),
      it2.next.function(Seq(it2.data))
    )
    zippedResults += tuple
  }

  // Convert zippedResults to an iterator
  val newIt = zippedResults.iterator

  // Functions to wrap the new iterator
  val hasNextFn = new FunctionObj(Seq(), Seq(), None, _ => BooleanObj(newIt.hasNext))
  val nextFn = new FunctionObj(Seq(), Seq(), None, _ => {
    if (newIt.hasNext) {
      val nextTuple = newIt.next()
      ListObj(nextTuple) // Wrap nextTuple in ListObj (which expects ArrayBuffer)
    } else {
      NoneObj()
    }
  })
  val data = DictionaryObj(mutable.HashMap())

  // Return the new IteratorObj
  new IteratorObj(nextFn, hasNextFn, data)
}

private def lenBuiltIn(params: Seq[DataObject]): NumberObj = {
  if (params.length > 1) {
    throw IllegalArgumentException()
  }

  val d = DictionaryObj(mutable.HashMap[DataObject, DataObject]())
  val it = toIteratorObj(params(0))
  var len = 0

  while (it.hasNext.function(Seq(it.data)).asInstanceOf[BooleanObj].value) {
    val nextElement = it.next.function(Seq(it.data))
    len = len + 1
  }
  val result = NumberObj(len)
  result
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

private def mapBuiltIn(params: Seq[DataObject]): IteratorObj = {
  if (params.length != 2 || !params(1).isInstanceOf[FunctionObj]) {
    throw new IllegalArgumentException("map function expects an iterable and a function")
  }

  val mapFn = params(1).asInstanceOf[FunctionObj]
  val it = toIteratorObj(params(0)).asInstanceOf[IteratorObj]

  val mappedResults = mutable.Buffer[DataObject]()

  while (it.hasNext.function(Seq(it.data)).asInstanceOf[BooleanObj].value) {
    val nextElement = it.next.function(Seq(it.data))
    val mappedElement = mapFn.function(Seq(nextElement))
    mappedResults += mappedElement
  }

  val newIt = mappedResults.iterator


  val hasNextFn = new FunctionObj(Seq(), Seq(), None, _ => BooleanObj(newIt.hasNext))
  val nextFn = new FunctionObj(Seq(), Seq(), None, _ => if (newIt.hasNext) newIt.next() else NoneObj())
  val data = DictionaryObj(mutable.HashMap())

  new IteratorObj(nextFn, hasNextFn, data)
}