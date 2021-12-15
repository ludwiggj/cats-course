package part2abstractMath

object Monoids {

  import cats.instances.int._
  import cats.syntax.semigroup._ // |+| method

  private val numbers = (1 to 1000).toList

  // |+| is associative
  private val sumLeft = numbers.foldLeft(0)(_ |+| _)
  private val sumRight = numbers.foldRight(0)(_ |+| _)

  // define a general API for a fold
  // def combineFold[T](list: List[T])(implicit semigroup: Semigroup[T]): T =
  //  list.foldLeft(/* What goes here? */)(_ |+| _)

  // Monoids
  import cats.Monoid
  private val intMonoid = Monoid[Int]

  import cats.instances.string._ // brings Monoid[String] into scope
  private val stringMonoid = Monoid[String]

  import cats.instances.option._ // construct implicit Monoid[Option[T]]

  private val optionIntMonoid = Monoid[Option[Int]]
  private val emptyOption = optionIntMonoid.empty

  // define a general API for a fold, using monoid
  def combineFold[T](list: List[T])(implicit monoid: Monoid[T]): T =
    list.foldLeft(monoid.empty)(_ |+| _)

  // Monoid for shopping carts
  case class ShoppingCart(items: List[String], total: Double)

  implicit val shoppingCartMonoid: Monoid[ShoppingCart] = Monoid.instance[ShoppingCart](
    ShoppingCart(List(), 0),
    (s1, s2) => ShoppingCart(s1.items ++ s2.items, s1.total + s2.total)
  )

  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart = combineFold(shoppingCarts)

  def main(args: Array[String]): Unit = {
    println(sumLeft)
    println(sumRight)

    println(intMonoid.combine(1, 2)) // 3
    println(intMonoid.empty)         // 0

    println(stringMonoid.combine("Rock the ", "JVM")) // Rock the JVM
    println(s">${stringMonoid.empty}<")               // ><

    println(emptyOption)                                           // None
    println(optionIntMonoid.combine(Option(2), Option.empty[Int])) // Some(2)
    println(optionIntMonoid.combine(Option(2), Option(5)))         // Some(7)

    // Gets |+| extension method via Semigroup
    import cats.syntax.monoid._ // Either this or cats.syntax.semigroup._
    println(Option(3) |+| Option(5))                               // Some(8)

    println(combineFold(List(2, 4, 6, 8)))                               // 20
    println(combineFold(List("2", "4", "6", "8")))                       // 2468
    println(combineFold(List(Option(2), Option.empty[Int], Option(6))))  // Some(8)

    // Now combine a list of phonebooks
    val phonebooks = List(
      Map(
        "Alice" -> 235,
        "Bob" -> 647
      ),
      Map(
        "Charlie" -> 372,
        "Daniel" -> 889
      ),
      Map(
        "Tina" -> 123
      )
    )

    import cats.instances.map._      // provides Monoid[Map]
    println(combineFold(phonebooks))

    val shoppingCart1 = ShoppingCart(List("Comix", "CDs"), 213.2)
    val shoppingCart2 = ShoppingCart(List("Food"), 132.9)

    println(combineFold(List(shoppingCart1, shoppingCart2))) // ShoppingCart(List("Comix", "CDs", "Food"), 346.1)
  }
}
