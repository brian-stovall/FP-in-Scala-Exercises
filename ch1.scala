/* Listing of exercise solutions for the book 'Functional Programming in
 Scala' - Chapter 1*/

object ChapterOne {
  /** Returns the nth integer of the Fibonacci sequence*/
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(itr: Int, nMinusTwo: Int, nMinusOne: Int): Int =
      if (itr == n) nMinusOne+nMinusTwo
      else go(itr + 1, nMinusOne, nMinusOne+nMinusTwo)

  if (n == 0) 0
  else if (n == 1) 1
  else go(2, 0, 1)
  }


  def isSorted[A] (as: Array[A], gt: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def go (iter: Int): Boolean = {
      if (iter == as. length - 1) true
      else if (!gt ( as(iter + 1), as(iter))) false
      else go(iter + 1)
    }
    go (0)
  }

  def main(args: Array[String]): Unit =
    println(isSorted(Array(1, 2, 6, 5, 8), (x : Int, y: Int) => x > y))
}
