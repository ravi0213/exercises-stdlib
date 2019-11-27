package list

sealed trait CustomList[+A]
case object Nil extends CustomList[Nothing]
case class Cons[+A](head: A, tail: CustomList[A]) extends CustomList[A]
case class X(x: String)
class B(b: String)
case class Y(y: String) extends B("<" + y + ">")
case class Z(z: String) extends B("@" + z + "@")

object CustomList {
  def sum(ints: CustomList[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }
  def join(list: CustomList[X]): String = list match {
    case Nil => ""
    case Cons(x, Nil) => x.x
    case Cons(x, xs) => x.x + ", " + join(xs)
  }

  def tail[A](list: CustomList[A]): CustomList[A] = list match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](a: A, list: CustomList[A]): CustomList[A] = list match {
    case Nil => CustomList(a)
    case Cons(_, xs) => Cons(a, xs)
  }

  def drop[A](list: CustomList[A], n: Int): CustomList[A] = list match {
    case Nil => Nil
    case Cons(_,xs) => if(n > 0) drop(xs, n-1) else list
  }

  def dropWhile[A](list: CustomList[A])(f: A => Boolean): CustomList[A] = list match {
    case Nil => Nil
    case Cons(x, xs) => if(f(x)) list else dropWhile(xs)(f);
  }

  def init[A](list: CustomList[A]): CustomList[A] = list match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A,B](as: CustomList[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def foldLeft[A,B](as: CustomList[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def loop(node: CustomList[A], res: B): B = node match {
      case Nil => res
      case Cons(x, xs) => loop(xs, f(res, x))
    }
    loop(as, z)
  }

  def length[A](list: CustomList[A]): Int = {
    //foldRight(list, 0)((_, y) => y+1)
    foldLeft(list, 0)((x, _) => x+1)
  }

  def reverse[A](as: CustomList[A]): CustomList[A] = {
    foldLeft(as, Nil: CustomList[A])((x, y) => Cons(y, x))
  }

  def append[A](as: CustomList[A], add: A): CustomList[A] = {
    reverse(Cons(add, reverse(as)))
  }

  def appendAll[A](lists: CustomList[CustomList[A]]): CustomList[A] = {
    reverse(foldLeft(lists, Nil: CustomList[A])((res, x ) => foldLeft(x, res)((innerRes, elm) => Cons(elm, innerRes))))
  }

  def addOne(as: CustomList[Int]): CustomList[Int] = {
    reverse(foldLeft(as, Nil: CustomList[Int])((res, x) => Cons(x+1, res)))
  }

  def doubleToString(as: CustomList[Double]): CustomList[String] = {
    reverse(foldLeft(as, Nil: CustomList[String])((res, x) => Cons(x.toString, res)))
  }

  def map[A, B](as: CustomList[A])(f: A => B): CustomList[B] = {
    reverse(foldLeft(as, Nil: CustomList[B])((res, cur) => Cons(f(cur), res)))
  }

  def filter[A](as: CustomList[A])(f: A => Boolean): CustomList[A] = {
    reverse(foldLeft(as, Nil: CustomList[A])((res, cur) => if(f(cur)) Cons(cur, res) else res))
  }

  def flatMap[A,B](as: CustomList[A])(f: A => CustomList[B]): CustomList[B] = {
    reverse(foldLeft(as, Nil: CustomList[B])(
      (res, cur) => foldLeft(reverse(f(cur)), res)((innerRes, elm) => Cons(elm, innerRes))
    ))
  }

  def apply[A](as: A*): CustomList[A] = {
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }
}
