package list

sealed trait CustomList[+A]
case object Nil extends CustomList[Nothing]
case class Cons[A](head: A, tail: CustomList[A]) extends CustomList[A]
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

  def apply[A](as: A*): CustomList[A] = {
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }
}
