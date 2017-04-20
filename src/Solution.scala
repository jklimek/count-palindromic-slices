/**
  * Created by klemens on 20/04/2017.
  */

object Solution {
  def solution(s: String): Int = {
    var palindromesCount = 0
    var string = "$" + s + "%" // trick for proper iteration (string(1) == first letter)
    val origStrSize = s.length
    val modifiedStrSize = string.length
    var results = Array.ofDim[Int](2, origStrSize+1)

    // Two runs, 1st for odd 2nd for even-length palindromes
    for (j <- 0 until 2) {
      var i = 1
      var radius = 0
      results(j)(0) = 0
      while (i <= origStrSize) {
        var t1 = i - radius - 1
        while (string(i - radius - 1) == string(i + j + radius)) {
          radius += 1
        }
        results(j)(i) = radius
        palindromesCount += radius
        var k = 1
        while ( (results(j)(i - k) != radius - k) && (k < radius)) {
          results(j)(i + k) = math.min(results(j)(i - k), radius - k)
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
    var testingString = "baababa"
    println(solution(testingString))
  }
}