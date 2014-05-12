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
    //The numbers at the edge of the triangle are all 1
    if ((c == 0) || (c == r)) 1
    //and each number inside the triangle is the sum of the two numbers above it
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance(chars: List[Char], openParens: Int): Boolean = {
      if (chars.isEmpty) {
        openParens == 0
      } else {
        val head = chars.head
        val count = head match {
          case '(' => openParens + 1
          case ')' => openParens - 1
          case _ => openParens
        }

        //if first paren is a closing paren we will have negative count, no need to call balance if we are negative
        if (count >= 0) balance(chars.tail, count) else false
      }
    }
    balance(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    //formula is described here http://en.wikipedia.org/wiki/Partition_%28number_theory%29
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
