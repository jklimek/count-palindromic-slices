/**
  * Created by klemens on 20/04/2017.
  */

object PalindromicSlicesCounter {

  // Function counting all substring palindromes for given string s
  def solution(s: String): Int = {
    var palindromesCount = 0
    val string = "$" + s + "%" // for intuitive iteration (string(1) == first letter)
    val origStrSize = s.length
    val results = Array.ofDim[Int](2, origStrSize + 1)

    // Two runs, 1st for even 2nd for odd-length palindromes
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
        while ((results(j)(i - k) != radius - k) && (k < radius)) {
          results(j)(i + k) = math.min(results(j)(i - k), radius - k) // Setting radius on the right hand side
          palindromesCount += results(j)(i + k)
          k += 1
        }
        radius = math.max(radius - k, 0)
        i += k // Skip computation for k-1 next chars
        if (palindromesCount > 100000000) return -1
      }
    }
    //    println(results.deep.mkString("\n"))
    palindromesCount
  }

  def main(args: Array[String]): Unit = {
    val testStringsArray = Array(
      "",
      "a" * 14142, // maximal uniform string passing (slices < 10000000) test (radius 7071)
      "a" * 14143, // just a little too much
      randomLowercaseAlpha(20000),
      randomLowercaseAlpha(20001), // string too long
      randomLowercaseAlpha(100),
      "kobylamamalybok",
      "r78ffffgq343qgv0bvZZZ",
      "dabaacaabaa"
    )

    testStringsArray.map(printSolution)
  }

  // Function generates random lowercase a-z string with given length
  def randomLowercaseAlpha(length: Int): String = {
    val chars = 'a' to 'z'
    randomStringFromCharList(length, chars)
  }

  // Function generates random string for given char sequence
  def randomStringFromCharList(length: Int, chars: Seq[Char]): String = {
    val sb = new StringBuilder
    for (i <- 1 to length) {
      val randomNum = util.Random.nextInt(chars.length)
      sb.append(chars(randomNum))
    }
    sb.toString
  }

  // Function checks if string is suitable for the algorithm
  def checkStringCorrectness(s: String): Boolean = {
    if (s.length > 20000) return false
    if (s.matches(".*[^a-z].*")) return false
    true
  }

  // Function just for printing results
  def printSolution(s: String): Any = {
    println("s:\t" + s)
    if (checkStringCorrectness(s)) {
      println("slices:\t" + solution(s) + "\n")
    } else {
      println("e:\tInvalid string\n")
    }
  }
}