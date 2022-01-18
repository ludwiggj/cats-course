package part4typeclasses

import cats.{Applicative, Apply}

object S4_WeakerMonads {
  trait MyFlatMap[M[_]] extends Apply[M] {
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    // TODO
    // hint: Apply extends Functor
    // Therefore: MyFlatMap can extend Apply
    override def ap[A, B](wf: M[A => B])(wa: M[A]): M[B] = {
      flatMap(wa)(a => map(wf)(f => f(a)))
    //         |  |        /   \     \/
    //         |  |    M[A=>B] A=>B  B
    //         |  |    \_____   ____/
    //       M[A] A =>      M[B]
    }
  }

  trait MyMonad[M[_]] extends Applicative[M] with MyFlatMap[M] {
    override def map[A, B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma)(a => pure(f(a)))
  }

  import cats.Monad
  import cats.FlatMap
  import cats.syntax.flatMap._ // flatMap extension method
  import cats.syntax.functor._ // map extension method

  // can now use for comprehensions on whatever type has a FlatMap instance in scope i.e. Monad instance isn't required

  def getPairs[M[_]: FlatMap](numbers: M[Int], chars: M[Char]): M[(Int, Char)] = for {
    n <- numbers
    c <- chars
  } yield (n, c)

  def getPairsGeneral[M[_]: FlatMap, A, B](numbers: M[A], chars: M[B]): M[(A, B)] = for {
    n <- numbers
    c <- chars
  } yield (n, c)

  // NOTE: Very similar to previous exercise with Monads,
  //       Except in getPairs method the constraint FlatMap is weaker than Monad
  def getPairsFor[M[_]: Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] = {
    for {
      a <- ma
      b <- mb
    } yield (a, b)
  }

  def main(args: Array[String]): Unit = {

  }

  // Cats TC type hierarchy:
  /*
      ____________             __________       ______________
     |           |            |         |      |             |
     | Semigroup |            | Functor |      | Semigroupal |
     |           |            |   map   |      |   product   |
     |___________|            |_________|      |_____________|
          ^                       ^                    ^
          |                       |____________________|
          |                                  |
      ____________                     ______________
     |           |                    |             |
     |  Monoid   |                    |    Apply    |
     |           |                    |     ap      |
     |___________|                    |_____________|
                                            ^
                                            |
                                 _________________________
                                |                        |
                         ______________           ______________
                        |             |          |             |
                        |   FlatMap   |          | Applicative |
                        |   flatMap   |          |    pure     |
                        |_____________|          |_____________|
                               ^                        ^
                               |________________________|
                                            |
                                         ________
                                        |       |
                                        | Monad |
                                        |_______|
  */
}
