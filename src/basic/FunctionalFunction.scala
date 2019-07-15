package basic

object FunctionalFunction {

  def main(args : Array[String]) : Unit = {
    def abstractFunc(arg: Int, f: Int => Int) : Int = {
      f(arg)
    }

    def absFunc(arg: Int): Int = {
      Math.abs(arg)
    }

    def sqrtFunc(arg: Int): Int = {
      Math.sqrt(arg).toInt
    }

    println(abstractFunc(9, absFunc))
    println(abstractFunc(9, sqrtFunc))
  }

  def setHead[A](h: A, l: List[A]) : List[A] = l match {
    case Nil => sys.error("null")
    case Cons(_, t) => Cons(h, t)
  }


  // (a, b) => a < b //它的原型如下
  /**
    * `apply`方法
    */
  val lessThan: (Int, Int) => Boolean = new Function2[Int, Int, Boolean] {
    override def apply(a: Int, b: Int): Boolean = a < b
  }



  /**
    * 删除前n个元素
    *
    * @param list 被操作的list
    * @param n    前n个元素
    * @tparam A   泛型
    * @return     删除了前n个元素的结果
    */
  def drop[A](list: List[A], n: Int) : List[A] = {
    if (n <= 0) list
    else list match {
        case Nil => Nil
        case Cons(_, t) => drop(t, n - 1)
      }
  }

  /**
    * 两个数组相互连接
    *
    * @param l1   前面的列表
    * @param l2   后面的列表
    * @tparam A   泛型
    * @return     连接后的列表
    */
  def append[A](l1: List[A], l2: List[A]) : List[A] = l1 match {
    case Nil => l2
    case Cons(h, t) => Cons(h, append(t, l2))
  }

  /**
    * 删除最后一个元素
    *
    * @param l    list
    * @tparam A   泛型
    * @return     删除了最后一个元素的数组
    */
  def init[A](l: List[A]) : List[A] = {

    l match {
      case Nil => sys.error("list can not be empty")
      case Cons(head, Nil) => Nil  // 如果是最后一个元素, 返回Nil, 即删除最后一个元素
      case Cons(head, tail) => Cons(head, init(tail))
    }
  }

  val c: (Int, Int) => Boolean = new Function2[Int, Int, Boolean] {
    def apply(n1: Int, n2: Int): Boolean = n1 < n2
  }
  c.apply(8, 9)
}
