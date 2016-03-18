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
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => t
    }

  def head[A](l: MyList[A]): A =
    l match {
      case Nil => sys.error("head of empty list")
      case Cons(h, _) => h
    }

  def setHead[A](l: MyList[A], head: A): MyList[A] =
    l match {
      case Nil => Cons(head, Nil)
      case Cons(h, t) => Cons(head, Cons(h,t))
    }

  def drop[A](l: MyList[A], n: Int): MyList[A] = {
    @annotation.tailrec
    def go(l: MyList[A], n: Int): MyList[A] =
      if (n == 0) l
      else go(MyList.tail(l), n-1)

    go(l, n)
  }

  def dropWhile[A] (l: MyList[A], f: A => Boolean): MyList[A] = {
    @annotation.tailrec
    def go(l: MyList[A]): MyList[A] =
      if (!f(MyList.head(l))) l
      else go(MyList.tail(l))
    go(l)
  }

   def init[A] (l: List[A]): List[A] =
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(h,t) => Cons(h, init(t))
    }

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(x,xs) => f(x, foldRight(xs, z)(f))
    }

  def length[A](l: MyList[A]): Int = {
    foldRight(l, 0)((_, b: Int) => {b + 1})
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
