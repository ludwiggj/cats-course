package part1recap

object Implicits {

  // Implicit class
  case class Person(name: String) {
    def greet: String = s"Hi, my name is $name!"
  }

  implicit class ImpersonableString(name: String) {
    def greet: String = Person(name).greet
  }

  // implicit arguments and values
  def increment(x: Int)(implicit amount: Int): Int = x + amount
  def multiply(x: Int)(implicit times: Int): Int = x * times

  // more complex example
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  def listToJson[T](list: List[T])(implicit serializer: JSONSerializer[T]): String =
    list.map(value => serializer.toJson(value)).mkString("[", ",", "]")

  case class Cat(name: String)

  case class Dog(name: String, age: Int)

  object OneArgSerializer {
    // implicit methods
    implicit def oneArgCaseClassSerializer[T <: Product]: JSONSerializer[T] = (value: T) =>
      s"""{"${value.productElementName(0)}" : "${value.productElement(0)}"}"""
  }

  object TwoArgSerializer {
    implicit def twoArgCaseClassSerializer[T <: Product]: JSONSerializer[T] = (value: T) =>
      s"""
         |{"${value.productElementName(0)}": "${value.productElement(0)}", "${value.productElementName(1)}": "${value.productElement(1)}"}
         |""".stripMargin.trim
  }

  // can be used for implicit conversions (DISCOURAGED)

  def main(args: Array[String]): Unit = {
    println("Peter".greet) // new ImpersonableString("Peter").greet

    // importing implicit conversions in scope
    import scala.concurrent.duration._
    val oneSec = 1.second
    println(oneSec)

    // implicit arguments and values
    implicit val defaultAmount: Int = 10

    println(s"2 add 10 is ${increment(2)}") // implicit argument 10 is passed by the compiler

    println(s"2 times 10 is ${multiply(2)}") // implicit argument 10 is passed by the compiler

    implicit val personSerializer: JSONSerializer[Person] = (person: Person) =>
      s"""{"name" : "${person.name}"}"""

    // implicit argument is used to PROVE THE EXISTENCE of a type
    println(s"""People: ${listToJson(List(Person("Alice"), Person("Bob")))}""")

    object CatOneArg {
      // implicit methods are used to PROVE THE EXISTENCE of a type
      import OneArgSerializer.oneArgCaseClassSerializer

      def showCats(): Unit = println(s"""Cats: ${listToJson(List(Cat("Tom"), Cat("Garfield")))}""")
      // in the background: val catsToJson = listToJson(List(Cat("Tom"), Cat("Garfield")))(oneArgCaseClassSerializer[Cat])
    }

    object CatTwoArgs {
      import TwoArgSerializer.twoArgCaseClassSerializer

      def showCats(): Unit = println(s"""Cats: ${listToJson(List(Cat("Tom"), Cat("Garfield")))}""")
    }

    object DogOneArg {
      import OneArgSerializer.oneArgCaseClassSerializer

      def showDogs(): Unit = println(s"""Dogs: ${listToJson(List(Dog("Butch", 5), Dog("Fido", 12)))}""")
    }

    object DogTwoArgs {
      import TwoArgSerializer.twoArgCaseClassSerializer

      def showDogs(): Unit = println(s"""Dogs: ${listToJson(List(Dog("Butch", 5), Dog("Fido", 12)))}""")
    }

    DogOneArg.showDogs()
    CatOneArg.showCats()
    DogTwoArgs.showDogs()
    CatTwoArgs.showCats()
  }
}