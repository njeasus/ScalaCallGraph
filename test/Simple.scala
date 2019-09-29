package dataflow.test

class Simple {
//  var y: Int = 0
//  val x: Int = 0
//  val list = List(1,2,3)
//
//  def modifier(): Unit = {
//    Static.x = 3
//    y = 3
//    Static.method()
//  }

  def mapInt(f: (Int) => String, i: Int): String = {
    f(i)
  }

  def higherOrder(): Unit = {
    val x = 1
    var res = 0
    mapInt({
      i: Int =>
        res = 3
        (i + x).toString
    }, 0)
    res = 4
  }

//  def map[A, B](f: (A) => B, l: Seq[A]): Seq[B] = {
//    var res = Seq[B]()
//    for(i <- 0 to l.length) {
//      res = res :+ f(l(i))
//    }
//    res
//  }

//  def ho(): Unit = {
//    var writtenVar = 0
//    map({ i: Int => writtenVar = i + 1 }, list)
//  }

//  def f(z: Int, in: X): Unit = {
//    in.called2()
//    y += x * z
//    in.z = y
//    in.called()
//    modifier()
//  }
}

class X() {
  var z: Int = 0

  def called(): X = new X
  def called2(): Unit = {}
}

object Static {
  var x = 0
  def method(): Unit = {
    println("Printed line")
  }
}