package part5alien

import cats.data.Reader

object Kleislis {

  def func1: Int => Option[String] = x => if (x % 2 == 0) Some(s"$x is even") else None
  def func2: Int => Option[Int] = x => Some(x * 3)

  // val func3 = func2 andThen func1

  def plainFunc1: Int => String = x => if (x % 2 == 0) s"$x is even" else "fail"
  def plainFunc2: Int => Int = x => x * 3

  val plainFunc3: Int => String = plainFunc2 andThen plainFunc1

  import cats.data.Kleisli
  import cats.instances.option._ // Flatmap[Option]

  val func1K: Kleisli[Option, Int, String] = Kleisli(func1)
  val func2K: Kleisli[Option, Int, Int] = Kleisli(func2)
  val func3K: Kleisli[Option, Int, String] = func2K andThen func1K

  val multiply = func2K.map(_ * 2) // Functior[Option], Option(...).map(_ * 2)

  val chain = func2K.flatMap(a => func1K.map(b => s"[$b]" * a))

  val chainFor = for {
    a <- func2K
    b <- func1K
  } yield s"[$b]" * a

  // TODO
  import cats.Id
  type InterestingKleisli[A, B] = Kleisli[Id, A, B] // wrapper over A => Id[B]
  // type InterestingKleisli[A, B] = Kleisli[Id, A, B] === Reader

  // hint
  //val times2 = Kleisli[Id, Int, Int](_ * 2)
  //val plus4 = Kleisli[Id, Int, Int](_ + 4)

  val times2 = Reader[Int, Int](_ * 2)
  val plus4 = Reader[Int, Int](_ + 4)

  val composed = times2.flatMap(t2 => plus4.map(p4 => t2 + p4))

  val composedFor = for {
    t2 <- times2
    p4 <- plus4
  } yield t2 + p4

  def main(args: Array[String]): Unit = {
    println(plainFunc3(2))
    println(plainFunc3(3))

    println(func3K.run(2))
    println(func3K.run(3))

    println(chain.run(2))
    println(chain.run(3))

    println(chainFor.run(2))
    println(chainFor.run(3))

    println(multiply.run(2))
    println(multiply.run(3))

    println(composed.run(2))
    println(composed.run(3))

    println(composedFor.run(2))
    println(composedFor.run(3))
  }
}
