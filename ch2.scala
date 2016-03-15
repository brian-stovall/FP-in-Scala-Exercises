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

  /** Returns true if an array of type A is sorted in ascending order,
  false otherwise. */
  def isSorted[A] (as: Array[A], gt: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def go (iter: Int): Boolean = {
      if (iter == as. length - 1) true
      else if (!gt ( as(iter + 1), as(iter))) false
      else go(iter + 1)
    }
    go (0)
  }


  /** Returns a partially applied f1 from an f2 */
  def partial1 [A,B,C] (a: A, f: (A,B) => C) : B => C = {
    (b: B) => {f (a, b)}
  }

  def curry[A,B,C] (f: (A, B) => C): A => (B => C) = {
    (a: A) => { (b: B) => {f(a, b)} }
  }

  def uncurry[A,B,C] (f: A => B => C): (A, B) => C = {
    (a: A, b: B) => {f(a)(b)}
  }

  def compose[A,B,C] (f: B => C, g: A => B): A => C = {
    (a: A) => {f(g(a))}
  }

  def main(args: Array[String]): Unit = {
    println (isSorted(Array(1, 2, 6, 5, 8), (x : Int, y: Int) => x > y))
    def addTwo = partial1 (2, (x: Int, y: Int) => {x + y})
    println (addTwo(3))
    println (addTwo(10))
    def makeAdder = curry((x: Int, y: Int) => {x + y})
    def addThree = makeAdder(3)
    println(addThree(4))
    println(addThree(-6))
    def plainAdder = uncurry(makeAdder)
    println(plainAdder(3, 4))
    def addSevenDivideTwo = compose( (n: Int) => {n/2}, (n: Int) => {n+7})
    println(addSevenDivideTwo(3))
  }
}
