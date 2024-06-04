package stdlib

import parser.{Value, Number}

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
   case Seq(Number(a), Number(b)) => Number(a+b)
   case _ => throw IllegalArgumentException()
  }
}


def sum3(params: Seq[Value]): Value = {
  params match {
   case Seq(Number(a), Number(b), Number(c)) => Number(a+b+c)
   case _ => throw IllegalArgumentException()
  }
}

