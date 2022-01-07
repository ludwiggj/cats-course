package part3datamanipulation

import cats.Eval
import cats.data.IndexedStateT

object FunctionalState {

  import cats.data.State

  val countAndSay: State[Int, String] = State(currentCount => (currentCount + 1, s"Counted $currentCount"))

  // iterative
  var a = 10

  a += 1
  val firstComputation = s"Added 1, obtained $a"

  a *= 5
  val secondComputation = s"Multiplied by 5, obtained $a"

  // pure FP with states
  val firstTransformation: State[Int, String] = State((s: Int) => {
    val newS = s + 1
    (newS, s"Added 1, obtained $newS")
  })
  val secondTransformation: State[Int, String] = State((s: Int) => {
    val newS = s * 5
    (newS, s"Multiplied by 5, obtained $newS")
  })

  val compositeTransformation: IndexedStateT[Eval, Int, Int, (String, String)] = firstTransformation.flatMap { firstResult =>
    secondTransformation.map(secondResult => (firstResult, secondResult))
  }

  // Why do we use state? Let's try and compose functions
  val f1: Int => (Int, String) = (s: Int) => {
    val newS = s + 1
    (newS, s"Added 1, obtained $newS")
  }
  val f2: Int => (Int, String) = (s: Int) => {
    val newS = s * 5
    (newS, s"Multiplied by 5, obtained $newS")
  }
  val f3: Int => (Int, String) = (s: Int) => {
    val newS = s - 12
    (newS, s"Subtracted 12, obtained $newS")
  }

  // Each composition...
  val f1f2: Int => (String, (Int, String)) = f1.andThen {
    case (state, res) => (res, f2(state))
  }

  // ... gets more complicated!
  val f1f2f3: Int => (String, String, (Int, String)) = f1.andThen {
    case (state, res1) => (res1, f2(state))
  }.andThen {
    case (res1, (state, res2)) => (res1, res2, f3(state))
  }

  // TODO: online store
  case class ShoppingCart(items: List[String], total: Double)

  def addToCart(item: String, price: Double): State[ShoppingCart, Double] =
    State { cart =>
      (
        ShoppingCart(cart.items :+ item, cart.total + price),
        cart.total + price
      )
    }

  val myCartWithDiscount: State[ShoppingCart, Double] = for {
    _ <- addToCart("Fender guitar", 500)
    _ <- addToCart("Elixir strings", 19)
    total <- addToCart("Electric cable", 8)
  } yield total * 0.9

  // TODO
  // returns a State data structure that, when run, will not change the state but will issue the value f(a)
  def inspect[A, B](f: A => B): State[A, B] = State { a =>
    (a, f(a))
  }

  // returns a State data structure that, when run, returns the value of that state and makes no changes
  def get[A]: State[A, A] = State { a =>
    (a, a)
  }

  // returns a State data structure that, when run, returns Unit and sets the state to that value
  def set[A](value: A): State[A, Unit] = State { _ =>
    (value, ())
  }

  // returns a State data structure that, when run, returns Unit and sets the state to f(state)
  def modify[A](f: A => A): State[A, Unit] = State { a =>
    (f(a), ())
  }

  // above methods available via following import
  // import cats.data.State._

  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 10)
    b <- get[Int]
    _ <- modify[Int](_ + 43)
    c <- inspect[Int, Int](_ * 2)
  } yield (a, b, c)

  def main(args: Array[String]): Unit = {
    println(countAndSay.run(10).value)
    println(firstComputation)
    println(secondComputation)
    println(compositeTransformation.run(10).value)
    println(f1f2(10))
    println(f1f2f3(10))
    println(myCartWithDiscount.run(ShoppingCart(List(), 0)).value)
    println(program.run(5).value)
  }
}
