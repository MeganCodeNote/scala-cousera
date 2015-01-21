package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    // assert c and r are non-negative, and c <= r
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def isBalance(numLeftParens: Int, numRightParens: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty) numLeftParens == numRightParens
      else if (numLeftParens < numRightParens) false
      else chars.head match {
        case '(' => isBalance(numLeftParens + 1, numRightParens, chars.tail)
        case ')' => isBalance(numLeftParens, numRightParens + 1, chars.tail)
        case _ => isBalance(numLeftParens, numRightParens, chars.tail)
      }
    }

    isBalance(0, 0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0) 0
    else if (money == 0) 1
    else if (coins.isEmpty) 0
    else (0 to money / coins.head)
      .map(money - _ * coins.head)
      .map(countChange(_, coins.tail))
      .reduceLeft(_ + _)
  }
}
