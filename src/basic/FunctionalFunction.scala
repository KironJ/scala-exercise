package basic

/**
  * 第二三章的练习题
  *
  * 参考了:
  *  - https://www.playscala.cn/article/view?_id=10-5a24b7db1b00000a00658aee
  *  - http://miximixi.me/index.php/archives/1077
  */
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



    /**
      * 删除符合判定的元素
      *
      * @param list 列表
      * @param f    判定函数
      * @tparam A   泛型
      * @return     删除后的结果
      */
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
      l match {
        case Cons(h,t) => if(f(h)) dropWhile(t, f) else Cons(h, dropWhile(t, f))
        case _ => l
      }

    /**
      * 函数的柯里化, 可以帮助类型推导.
      * @param list 列表
      * @param f    判定函数
      * @tparam A   泛型
      * @return     删除后的结果
      */
    def dropWhile2[A](list: List[A])(f: A => Boolean) : List[A] = list match {
      case Cons(head, tail) if f(head) => dropWhile2(tail)(f)
      case _ => list
    }


/*    val l1 = List(1 to 5)
    println("dropWhile: " + dropWhile(l1, (v: Int) => v < 3)) //???? 会报错, 怀疑是List(x to y)方法导致的
    println("dropWhile删除符合判定的元素: " + dropWhile(l1, (v: Int) => v < 3))*/
    println("dropWhile: " + dropWhile(List(1,2,3,3,2,4,8), (v: Int) => v < 3))      // 这里必须注明v的类型
    println("dropWhile2: " + dropWhile2(List(1,2,3,3,2,4,8))(v => v < 3))           // 这里可以推导, 前面的参数组已经明确了泛化类型为Int, 所以
    // 参数类型会从左传递到右边第二个参数组.

    // 练习3.9 使用foldRight计算list的长度.
    def length[A](as: List[A]): Int = {
      foldRiht(as, 0)((_, y) => y+1)
    }
    println("计算长度: " + length(List(1,2,3,4,5,6,7,8,88)))

    // 练习3.10, 将foldRight转成尾递归, 避免栈溢出.
    @annotation.tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Cons(h, Nil) => f(z, h)                  // 这里注意, (h,Nil)不能放在下面
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
    println("尾递归实现foldLeft: " + foldLeft(List(1,2,3,4,5), 1000)(_+_))

    // 练习3.14 用foldLeft或foldRight实现append.
    def appendViaFoldRight[A](l1: List[A], l2: List[A]): List[A] = {
      foldRiht(l1, l2)(Cons(_, _))
    }

//    def appendViaFoldLeft[A](l1: List[A], l2: List[A]): List[A] = {
//      foldLeft(l2, l1)()
//    }

//    3.15 不会
    // 3.16 对一个整数列表的每个元素加一
    def listPlusOne(list: List[Int]) : List[Int] = list match {
      case Cons(h, Nil) => Cons(h + 1, Nil)
      case Cons(h, t) => Cons(h + 1, listPlusOne(t))
    }
    println("listPlusOne: " + listPlusOne(List(1,2,4,8,10)))

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
    *   这个方法无法做到常量级开销, 因为这个单向链表的删除一定会遍历整个列表, 哪怕只是删除最后一个元素.
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


  // 基于list的递归并泛化为高阶函数
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(h, t) => h * product(t)
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(h, t) => h + sum(t)
  }
  // 上面两个是非常相似的函数, 不同的是, 一个是加法, 另一个是乘法.

  def foldRiht[A, B](as: List[A], z:B)(f: (A, B) => B): B =   // 这里把函数f参数独立出来, 可以让类型系统能够推导出f的输入类型.
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRiht(xs, z)(f))
  }

  // dropWhile(List(1 to 5), (v: Int) => v < 3)
  def sum2(list: List[Int]): Int = foldRiht(list, 0)(_+_)
  def sum3(list: List[Int]): Int = foldRiht(list, 0)((x, y) => x + y) // 等同于上面的简写

  def product2(list: List[Double]): Double = foldRiht(list, 1.0)(_*_)
}



























