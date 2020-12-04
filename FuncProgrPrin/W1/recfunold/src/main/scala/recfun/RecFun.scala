package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
    //println(balance("(just an) example".toList ))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if ( c == 0 && r ==0 ) 1
    else if ( c==0 && r > 0) 1
    else if ( c > 0 && r == c) 1
    else  pascal( c-1, r - 1) + pascal(c,r-1)

  }

  /**
   * Exercise 2
   */
 def balance(chars: List[Char]): Boolean = {
   def loop(chars: List[Char], open: Int): Boolean = {
     chars match {
       case Nil => open == 0
       case '(' :: xs => loop(xs, open + 1)
       case ')' :: xs => (open > 0) && loop(xs, open - 1)
       case any => loop(chars.tail, open)
     }
   }
   loop(chars, 0)
 }



  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChangeCombinations(money: Int, coins: List[Int]): Int =
      if (money == 0)
        1
      else if (money < 0 || coins.isEmpty)
        0
      else
      // total combinations = using the first coin only + using all other coins
        countChangeCombinations(money - coins.head, coins) + countChangeCombinations(money, coins.tail)

    countChangeCombinations(money, coins)
  }
}
