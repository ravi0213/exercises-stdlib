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

    println("remove and sum [4, 1, 2, 3]: " + CustomList.sum(CustomList.tail(CustomList(4, 1, 2, 3))))
    println("replace and sum [4, 1, 2, 3]: " + CustomList.sum(CustomList.setHead(6, CustomList(4, 1, 2, 3))))
    println("remove 1: " + CustomList.drop(CustomList(4, 1, 2, 3), 1))
    println("remove 5: " + CustomList.dropWhile(CustomList(4, 2, 2, 3))(x => x%2 == 1))

    println("init: " + CustomList.init(CustomList(1, 2, 3, 4)))

    println("length: " + CustomList.length(CustomList(1, 2, 3, 4)))

    println("reverse: " + CustomList.reverse(CustomList(1, 2, 3, 4)))

    println("append: " + CustomList.append(CustomList(1, 2, 3, 4), 6))

    println("appendAll: " + CustomList.appendAll(CustomList(CustomList(1, 2), CustomList(3, 4))))

    println("addOne: " + CustomList.addOne(CustomList(1, 2)))

    println("doubleToString: " + CustomList.doubleToString(CustomList(1.0, 2.0)))

    println("map: " + CustomList.map(CustomList('a', 'b', 'c'))(x => x.toInt))

    println("filter: " + CustomList.filter(CustomList(1,2,3,4))(x => x%2 == 0))

    println("flatmap: " + CustomList.flatMap(CustomList(1,2,3))(i => CustomList(i, i)))

  }
}
