package datastructures

sealed trait MyList[+A]

case object Nil extends MyList[Nothing]
case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {
  def sum(ints: MyList[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def tail[A](l: MyList[A]): MyList[A] =
    l match {
    case Nil => Nil
    case Cons(_, t) => t
    }

  def drop[A](l: MyList[A], n: Int): MyList[A] = {
    @annotation.tailrec
    def go[A](l: MyList[A], n: Int): MyList[A] =
      if (n == 0) l
      else go(MyList.tail(l), n-1)

    go(l, n)
  }

  def product(ds: MyList[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val example = Cons(1, Cons(2, Cons(3, Nil)))
  val example2 = MyList(1,2,3)
  val total = sum(example)
}
