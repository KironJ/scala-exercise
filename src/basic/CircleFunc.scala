package basic

object CircleFunc {

  def main(args: Array[String]) : Unit = {

    // 使用递归实现阶乘计算
    @annotation.tailrec
    def go(n: Int, acc: Int = 1) : Int = {
      if (n <= 0) {
        acc
      } else {
        println(n + "   " + acc)
        go(n - 1, acc * n)
      }
    }
//    workspace\spark-example-project\src\main\scala\xyz\kironj\scala\exercise
    lazy val result = go(8)
    println("llallalalal")
    print(result)
  }
}
