object Cats {
  def main(args: Array[String]): Unit = {
    println("Hello cats")

    case class User(name: String, age: Int)
    val userBase = List(User("Ravi", 25), User("A", 28), User("B", 24))

    val above24 = for( user <- userBase if user.name equals "Ravi" ) yield user.name

    println(above24)
  }
}
