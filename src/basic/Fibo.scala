package basic

object Fibo {

  def main (args : Array[String]) : Unit = {

//    @annotation.tailrec
    def fibo (n : Int) : Int = {

      if (n == 1) {
        0
      } else if (n == 2) {
        1
      } else {
        fibo(n -1) + fibo(n-2)
      }
    }

    for (i <- 1 to 10) {
      println(i + "  " + fibo(i))
    }

  }
}
