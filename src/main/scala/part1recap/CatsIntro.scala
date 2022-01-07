package part1recap

object CatsIntro {

  //noinspection ComparingUnrelatedTypes
  val aComparison: Boolean = 2 == "Two" // This compiles. but will always be false

  // Hence Eq

  // Part 1 - type class import

  import cats.Eq

  // Part 2 - import type class instances for the type you need
  import cats.instances.int._

  // Part 3 - use the type class API
  val intEquality: Eq[Int] = Eq[Int]

  // Summoner pattern
  // object Eq {
  //   final def apply[A](implicit ev: Eq[A]): Eq[A] = ev
  // }

  // Part 4 - use extension methods, these are in syntax packages

  import cats.syntax.eq._ // Eq extension methods

  def main(args: Array[String]): Unit = {
    println(intEquality.eqv(2, 3))
    println(intEquality.eqv(2, 2))

    // Doesn't compile
    // println(intEquality.eqv(2, "two"))

    println(2 === 3) // === is an extension method, depends on presence of appropriate type class instance
    println(2 =!= 3)
    // println(2 =!= "3") // Doesn't compile

    // i.e. extension methods are only visible in the presence of the right type class instance(s)

    // Part 5 - extending the type class operations to composite types e.g. lists
    import cats.instances.list._
    println(List(2) === List(3))

    // Part 6 - defining type class instance for custom type
    case class ToyCar(name: String, price: Double)

    implicit val toyCarEq: Eq[ToyCar] = Eq.instance[ToyCar] {
      (tc1, tc2) => tc1.price == tc2.price
    }

    println(ToyCar("Ferrari", 29.99) === ToyCar("Ford", 29.99))

  }
}
