package part1recap

object TypeClasses {

  case class Person(name: String, age: Int)

  // Part 1 - type class definition
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  // Part 2 - create implicit type class instances
  implicit object StringSerializer extends JSONSerializer[String] {
    override def toJson(value: String): String = "\"" + value + "\""
  }

  implicit object IntSerializer extends JSONSerializer[Int] {
    override def toJson(value: Int): String = value.toString
  }

  implicit object PersonSerializer extends JSONSerializer[Person] {
    override def toJson(value: Person): String =
      s"""
        | { "name": ${value.name}, "age": ${value.age} }
        |""".stripMargin.trim
  }

  // Part 3 - offer an API
  def convertListToJSON[T](list: List[T])(implicit serializer: JSONSerializer[T]): String =
    list.map(serializer.toJson).mkString("[", ",", "]")

  def toJson[T](value: T)(implicit serializer: JSONSerializer[T]): String =
    serializer.toJson(value)

  // Part 4 - extending the existing types via extension methods
  object JSONSyntax {
    implicit class JSONSerializable[T](value: T)(implicit serializer: JSONSerializer[T]) {
      def wibble: String = serializer.toJson(value)
      def toJson: String = serializer.toJson(value)
    }
  }

  def main(args: Array[String]): Unit = {
    // Part 2
    println(s"""String as json: ${StringSerializer.toJson("A json string")}""")
    println(s"""   Int as json: ${IntSerializer.toJson(5)}""")
    println(s"""Person as json: ${PersonSerializer.toJson(Person("Graeme", 51))}""")

    // Part 3
    println(s"""People as json: ${convertListToJSON(List(Person("Alice", 23), Person("Xavier", 45)))}""")
    println(s"""Person as json: ${toJson(Person("Wendell", 8))}""")

    // Part 4
    import JSONSyntax._
    val bob = Person("Bob", 36)
    println(s"""Person as json: ${bob.wibble}""")
    println(s"""Person as json: ${bob.toJson}""")
  }
}
