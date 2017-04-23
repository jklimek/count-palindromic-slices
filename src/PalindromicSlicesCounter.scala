/**
  * Created by klemens on 20/04/2017.
  */

object PalindromicSlicesCounter {
  def solution(s: String): Int = {
    var palindromesCount = 0
    val string = "$" + s + "%" // for intuitive iteration (string(1) == first letter)
    val origStrSize = s.length
    val results = Array.ofDim[Int](2, origStrSize+1)

    // Two runs, 1st for odd 2nd for even-length palindromes
    for (j <- 0 until 2) {
      var i = 1
      var radius = 0
      results(j)(0) = 0
      while (i <= origStrSize) {
        // Measure palindrome radius
        while (string(i - radius - 1) == string(i + j + radius)) {
          radius += 1
        }
        results(j)(i) = radius
        palindromesCount += radius

        // Manacher's algorithm
        var k = 1
        while ( (results(j)(i - k) != radius - k) && (k < radius) ) {
          results(j)(i + k) = math.min(results(j)(i - k), radius - k)
          palindromesCount += results(j)(i + k)
          k += 1
        }
        radius = math.max(radius - k, 0)
        i += k // This skipping keeps computation at O(n)
        if (palindromesCount > 100000000) return -1
      }
    }
    palindromesCount
  }

  def main(args: Array[String]): Unit = {
    val testStringsArray = Array(
      "",
      "a"*14142, // maximal uniform string passing (slices < 10000000) test
      "a"*14143, // just a little too much
      randomAlpha(20000),
      randomAlpha(100),
      "kobylamamalybok",
      "abababa"
    )
    if (testStringsArray.map(checkStringCorrectness).forall(a => a) ) {
      testStringsArray.map(solution).foreach(println)
    } else {
      println("Invalid string")
    }
  }

  def randomAlpha(length: Int): String = {
    val chars = 'a' to 'z'
    randomStringFromCharList(length, chars)
  }

  def randomStringFromCharList(length: Int, chars: Seq[Char]): String = {
    val sb = new StringBuilder
    for (i <- 1 to length) {
      val randomNum = util.Random.nextInt(chars.length)
      sb.append(chars(randomNum))
    }
    sb.toString
  }

  def checkStringCorrectness(s: String): Boolean = {
    if (s.length > 20000) return false
    if (s.matches("[^a-z]")) return false

    true
  }
}