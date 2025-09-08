package recfun

import common._

import scala.annotation.tailrec

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
    if (c == 0) {
      1;
    }

    else if (c == r) {
      1;
    }

    else pascal(c - 1, r - 1) + pascal(c, r - 1);
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    @tailrec
    def checkBalance(chars: List[Char], openCount: Int): Boolean = {
      if (chars.isEmpty) {
        openCount == 0;
      }

      else if (chars.head == '(') {
        checkBalance(chars.tail, openCount + 1);
      }

      else if (chars.head == ')') {
        if (openCount == 0) {
          false;
        }

        else checkBalance(chars.tail, openCount - 1);
      }

      else checkBalance(chars.tail, openCount);
    }

    checkBalance(chars, 0);
  }


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) {
      1;
    }
    else if (money < 0) {
      0;
    }
    else if (coins.isEmpty) {
      0
    };

    else {
      val withFirst = countChange(money - coins.head, coins);
      val withoutFirst = countChange(money, coins.tail);

      withFirst + withoutFirst;

    }
  }

}
