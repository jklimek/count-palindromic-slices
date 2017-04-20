/**
  * Created by klemens on 20/04/2017.
  */

object Solution {
  def solution(s: String): Int = {
    var palindromesCount = 0
    val strSize = s.length
    for (i <- 1 until strSize) {
      println(i)
    }

    println(s)
    palindromesCount
  }

  def main(args: Array[String]): Unit = {
    var testingString = "banana"

    println(solution(testingString)) // should return 4
  }
}