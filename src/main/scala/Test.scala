object Test {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(ind: Int): Boolean = {
      if(ind >= as.length) true
      else if(ordered(as(ind-1), as(ind))) loop(ind+1)
      else false
    }
    loop(1)
  }

  def main(args: Array[String]): Unit = {
    println("Hello scala")
    val comparator = (x: Int, y: Int) => x <= y
    println("isSorted [1,3,4,5]: " + isSorted(Array(1,3,4,5), comparator))
    println("isSorted [1,3,7,5]: " + isSorted(Array(1,3,7,5), comparator))

    import list._
    val ex1: CustomList[Int] = Nil
    val ex2 = Cons(1, Cons(2, ex1))
    println("sum: " + CustomList.sum(ex2) + " , class: " + ex1.getClass)
    println("type: " + 1.getClass)



    val ex3 = Cons(X("Ravi"), Cons(X("Kumar"), Nil))
    println("join: " + CustomList.join(ex3))

    val x = CustomList(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _))))  => x + y
      case Cons(h, t) => h + CustomList.sum(t)
      case _ => 101
    }
    println("x: " + x)

  }
}
