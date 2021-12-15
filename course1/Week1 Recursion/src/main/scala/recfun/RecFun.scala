package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if r == 0 || c == 0 || c == r then 1 else pascal(c, r - 1) + pascal(c - 1, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    if chars.isEmpty then true else balance_helper(chars, 0)

  @tailrec
  def balance_helper(chars: List[Char], pre_quo: Int): Boolean =
    if pre_quo < 0 then false
    else if chars.isEmpty then pre_quo == 0
    else
      val next_quo = if chars.head == '(' then pre_quo + 1 else if chars.head == ')' then pre_quo - 1 else pre_quo
      balance_helper(chars.tail, next_quo)


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if money == 0 || coins.isEmpty then 0 else countChange_helper(money, coins)

  def countChange_helper(money: Int, coins: List[Int]): Int =
      if money == 0 then 1 else
      if money < 0 || coins.isEmpty then 0 else
      countChange_helper(money - coins.head, coins) + countChange_helper(money, coins.tail)