package part2abstractMath

object Semigroups {

  // Semigroups combine elements of the same type
  import cats.Semigroup
  import cats.instances.int._

  val naturalIntSemigroup: Semigroup[Int] = Semigroup[Int]

  import cats.instances.string._

  val naturalStringSemigroup: Semigroup[String] = Semigroup[String]

  def reduceInts(list: List[Int]): Int = list.reduce(naturalIntSemigroup.combine)
  def reduceStrings(list: List[String]): String = list.reduce(naturalStringSemigroup.combine)
  def reduceThings[T](list: List[T])(implicit s: Semigroup[T]): T = list.reduce(s.combine)

  case class Expense(id: Long, amount: Double)

  implicit val expenseSemigroup: Semigroup[Expense] = Semigroup.instance(
    (a, b) => Expense(Math.max(a.id, b.id), a.amount + b.amount)
  )

  def main(args: Array[String]): Unit = {
    println(naturalIntSemigroup.combine(2, 46))                // 48 (default combine is addition)
    println(naturalStringSemigroup.combine("I love ", "cats")) // I love cats (default combine is concatenation)

    val ints = (1 to 10).toList
    val strings = List("I ", "am ", "starting ", "to ", "like ", "semigroups")

    // Specific APIs
    println(reduceInts(ints))       // 55
    println(reduceStrings(strings)) // I am starting to like semigroups

    // Generic APIs
    println(reduceThings(ints))    // compiler injects implicit Semigroup[Int]
    println(reduceThings(strings)) // compiler injects implicit Semigroup[String]

    import cats.instances.option._
    // compiler will produce an implicit Semigroup[Option[Int]]
    // compiler will produce an implicit Semigroup[Option[String]]
    // same for any type with an implicit Semigroup

    val numberOptions: List[Option[Int]] = ints.map(i => Option(i))
    println(reduceThings(numberOptions)) // Some(55)

    val stringOptions: List[Option[String]] = strings.map(i => Option(i))
    println(reduceThings(stringOptions)) // Some(I am starting to like semigroups)

    val expenses = List(Expense(2, 34.9), Expense(1, 15.0), Expense(3, 41.7))
    println(reduceThings(expenses)) // Expense(3,91.6)

    import cats.syntax.semigroup._ // |+|
    println(1 |+| 2)                               // 3
    println("Hello " |+| "there")                  // Hello there
    println(Expense(2, 54) |+| Expense(1, 12.8))   // Expense(2, 66.8)

    def reduceThings2[T](list: List[T])(implicit s: Semigroup[T]): T = list.reduce(_ |+| _)
    println(reduceThings2(expenses))

    def reduceThings3[T: Semigroup](list: List[T]): T = list.reduce(_ |+| _)
    println(reduceThings3(expenses))
  }
}
