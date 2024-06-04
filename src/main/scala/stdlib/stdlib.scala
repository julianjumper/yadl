package stdlib

import parser.Value

type HashMap[K, V] = scala.collection.mutable.HashMap[K, V]

case class BuiltinFunction(n_args: Int, fn: Seq[Value] => Value)

def getStdlibDictionary(): HashMap[String, BuiltinFunction] = {
  var m: HashMap[String, BuiltinFunction] = new HashMap
  m.addOne("sum2", BuiltinFunction(2, sum2))
  m.addOne("sum3", BuiltinFunction(3, sum3))
  m
}

def sum2(params: Seq[Value]): Value = {
  params match {
   case Seq(a, b) => a
   case _ => throw IllegalArgumentException()
  }
}


def sum3(params: Seq[Value]): Value = {
  params match {
   case Seq(a, b, c) => a
   case _ => throw IllegalArgumentException()
  }
}

