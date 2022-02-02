package part4typeclasses

import cats.{Functor, Semigroupal}

object S3_WeakerApplicatives {
  trait MyApplicative[W[_]] extends Functor[W] with Semigroupal[W] {
    def pure[A](a: A): W[A] // fundamental method

    override def product[A, B](wa: W[A], wb: W[B]): W[(A, B)] = {
      val functionWrapper: W[B => (A, B)] = map(wa)(a => (b: B) => (a, b))
      ap(functionWrapper)(wb)
    }

    def ap[A, B](wf: W[A => B])(wa: W[A]): W[B]
  }

  trait MyApply[W[_]] extends Functor[W] with Semigroupal[W] {
    override def product[A, B](wa: W[A], wb: W[B]): W[(A, B)] = {
      val functionWrapper: W[B => (A, B)] = map(wa)(a => (b: B) => (a, b))
      ap(functionWrapper)(wb)
    }

    def ap[A, B](wf: W[A => B])(wa: W[A]): W[B] // fundamental method

    // TODO
    def mapN[A, B, C](tuple: (W[A], W[B]))(f: (A, B) => C): W[C] = {
      val (wa, wb) = tuple
      val tupleWrapper: W[(A, B)] = product(wa, wb)
      map(tupleWrapper) { case (a, b) => f(a, b) }
    }
  }

  trait MyApplicative2[W[_]] extends MyApply[W] {
    def pure[A](a: A): W[A] // fundamental method
  }

  import cats.Applicative
  import cats.Apply
  import cats.instances.option._ // implicit Apply[Option]

  val applyOption: Apply[Option] = Apply[Option]

  // Note: Rarely used in this way
  val funcApp: Option[Int] = applyOption.ap(Some((x: Int) => x + 1))(Some(4))

  // Much more common is the use of extension methods to extract and combine tuples
  import cats.syntax.apply._ // apply extension methods

  val tupleOfOptions: (Option[Int], Option[Int], Option[Int]) = (Option(1), Option(2), Option(3))
  val optionOfTuple: Option[(Int, Int, Int)] = tupleOfOptions.tupled
  val sumOption: Option[Int] = tupleOfOptions.mapN(_ + _ + _)

  def main(args: Array[String]): Unit = {
    println(funcApp)
    println(optionOfTuple)
    println(sumOption)
  }

  // Cats TC type hierarchy:
  /*
      ____________             __________       ______________
     |           |            |         |      |             |
     | Semigroup |            | Functor |      | Semigroupal |
     |  combine  |            |   map   |      |   product   |
     |___________|            |_________|      |_____________|
          ^                       ^                    ^
          |                       |____________________|
          |                                  |
      ____________                     ______________
     |           |                    |             |
     |  Monoid   |                    |    Apply    |
     |  empty    |                    |     ap      |
     |___________|                    |_____________| 
                                            ^
                                            |
                                       ______________
                                      |             |
                                      | Applicative |
                                      |    pure     |
                                      |_____________|
                                            ^
                                            |
                                         ________
                                        |       |
                                        | Monad |
                                        |_______|
  */

}