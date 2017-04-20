/**
  * Created by klemens on 20/04/2017.
  */

object Solution {
  def solution(s: String): Int = {
    var palindromesCount = 0
    val string = "$" + s + "%" // trick for intuitive iteration (string(1) == first letter)
    val origStrSize = s.length
    val results = Array.ofDim[Int](2, origStrSize+1)

    // Two runs, 1st for odd 2nd for even-length palindromes
    for (j <- 0 until 2) {
      var i = 1
      var radius = 0
      results(j)(0) = 0
      while (i <= origStrSize) {
        while (string(i - radius - 1) == string(i + j + radius)) {
          radius += 1
        }
        results(j)(i) = radius
        palindromesCount += radius
        var k = 1
        while ( (results(j)(i - k) != radius - k) && (k < radius)) {
          results(j)(i + k) = math.min(results(j)(i - k), radius - k)
          palindromesCount += results(j)(i + k)
          k += 1
        }
        radius = math.max(radius - k, 0)
        i += k
      }
    }
    println(results.deep.mkString("\n"))
    println(s)
    palindromesCount
  }

  def main(args: Array[String]): Unit = {
    var testingString = "aaabababa"
    println(solution(testingString))
  }
}