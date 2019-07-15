package basic

object FindFirst {

  def main(args: Array[String]): Unit = {


//    val x: Int = List(1,2,3,4,5) match {
//      case Cons(x, Cons(2, Cons(4, _))) => x
//      case Nil => 42
//      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => {println(" executing..."); x + y}
//      case Cons(h, t) => h + sum(t)
//      case _ => 101
//    }
//    println(x)

    println(add(10)(55))
  }

  def curry[A, B, C] (f: (A, B) => C) : A => (B => C) = {

    f.curried

  }



  // 普通加法函数
  def add(n1: Int, n2: Int) : Int = {
    n1 + n2
  }

  def add2(n1: Int): (Int, Int) => Int = {
//    (n2: Int, n3: Double)=> n1 + n2
    _ + _
  }

  () => 5



  // 函数的柯里化
  def add(n1: Int) : Int => Int = {
    n2: Int => n1 + n2
  }

}
