package stdlib

import interpreterdata.*

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

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

private def flatMapBuiltIn(params: Seq[DataObject]): DataObject = {
  if (params.length != 2 || !params(1).isInstanceOf[FunctionObj]) {
    throw IllegalArgumentException()
  }

  val flatMapFn = params(1).asInstanceOf[FunctionObj]

  // We can cheat and just don't use the the data attribute of the
  // iterator, since it can't be accessed by the language in the
  // first place.
  val d = DictionaryObj(mutable.HashMap[DataObject, DataObject]())

  val it = toIteratorObj(params(0)) //Function parameter + data
  var buffer: ListObj = NONE
  var hasBuffer = false

  var arrSize = 0

  val hasnext = FunctionObj(Seq("d"), Seq(), None, (d: Seq[DataObject]) => {
    if (arrSize < buffer.size()) {
      TRUE
    }
    while (it.hasNext.function(Seq(it.data)).asInstanceOf[BooleanObj].value) {
      val n = it.next.function(Seq(it.data))
      buffer = flatMapFn.function(Seq(n)).asInstanceOf[ListObj]
      arrSize = 0
      TRUE
      }
    FALSE
  })

  val next = FunctionObj(Seq("d"), Seq(), None, (d: Seq[DataObject]) => {
    if (arrSize < buffer.size()) {
      val tmp = buffer[arrSize]
      arrSize += 1
      tmp
    } else if (hasnext.function(d).asInstanceOf[BooleanObj].value) {
      val tmp = buffer[arrSize]
      arrSize += 1
      tmp
    }
    else {
      throw IllegalArgumentException()
    }
  })

  IteratorObj(next, hasnext, d)
}

private def mapBuiltIn(params: Seq[DataObject]): DataObject = {
  if (params.length != 2 || !params(1).isInstanceOf[FunctionObj]) {
    throw IllegalArgumentException()
  }

  val mapFn = params(1).asInstanceOf[FunctionObj]

  // We can cheat and just don't use the the data attribute of the
  // iterator, since it can't be accessed by the language in the
  // first place.
  val d = DictionaryObj(mutable.HashMap[DataObject, DataObject]())

  val it = toIteratorObj(params(0)) //Function parameter + data
  var buffer: DataObject = NONE
  var hasBuffer = false


  val hasnext = FunctionObj(Seq("d"), Seq(), None, (d: Seq[DataObject]) => {
    if (hasBuffer) {
      TRUE
    }
    while (it.hasNext.function(Seq(it.data)).asInstanceOf[BooleanObj].value) {
      val n = it.next.function(Seq(it.data))
      buffer = n
      TRUE
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

private def sortBuiltIn(params: Seq[DataObject]): DataObject = {
  if (params.length != 2 || !params(1).isInstanceOf[FunctionObj]) {
    throw IllegalArgumentException()
  }

  val sortFn = params(1).asInstanceOf[FunctionObj]

  // We can cheat and just don't use the the data attribute of the
  // iterator, since it can't be accessed by the language in the
  // first place.
  val d = DictionaryObj(mutable.HashMap[DataObject, DataObject]())

  val it = toIteratorObj(params(0)) //Function parameter + data

  val res = ArrayBuffer[DataObject]()
  while(it.hasNext.function(Seq(it.data)).asInstanceOf[BooleanObj].value)
  {
    res += it.next.function(Seq(it.data))
  }
  res.sortWith(sortFn)

}

private def countBuiltIn(params: Seq[DataObject]): NumberObj = { //TODO: Has to be done
  if (params.length != 2 || !params(1).isInstanceOf[FunctionObj]) {
    throw IllegalArgumentException()
  }

  val obj = params(1).asInstanceOf[DataObject]

  // We can cheat and just don't use the the data attribute of the
  // iterator, since it can't be accessed by the language in the
  // first place.
  val d = DictionaryObj(mutable.HashMap[DataObject, DataObject]())

  val it = toIteratorObj(params(0)) //Function parameter + data

}