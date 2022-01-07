package part4typeclasses

import cats.Monad

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object S1_Semigroupals {

  // Semigroupal - tuples elements i.e. combines values into a tuple
  //               it doesn't actually combine the values (which is what a Semigroup does)
  //               i.e. Semigroupal = tupling
  //                    Semigroup   = combining
  trait MySemigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  import cats.Semigroupal
  import cats.instances.option._ // implicit Semigroupal[Option]

  val optionSemigroupal: Semigroupal[Option] = Semigroupal[Option]
  val aTupledOption: Option[(Int, String)] = optionSemigroupal.product(Some(123), Some("a string")) // Don't have to unwrap / wrap items
  val aNoneTupled: Option[(Int, Nothing)] = optionSemigroupal.product(Some(123), None)

  import cats.instances.future._

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  val aTupledFuture: Future[(String, Int)] = Semigroupal[Future].product(Future("meaning of life"), Future(42))

  import cats.instances.list._ // Monad[List], as monad is semigroupal (see below for details)

  val aTupledList: Seq[(Int, String)] = Semigroupal[List].product(List(1, 2), List("a", "b")) // cartesian product (monad rather than a zip)

  // TODO
  def productWithMonads[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A, B)] =
    monad.flatMap(fa)(a => monad.flatMap(fb)(b => monad.pure(a, b)))

  def productWithMonads2[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A, B)] =
    monad.flatMap(fa)(a => monad.map(fb)(b => (a, b)))

  def productWithMonadsFor[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A, B)] = {
    import cats.syntax.functor._ // for map
    import cats.syntax.flatMap._ // for flatMap

    for {
      a <- fa
      b <- fb
    } yield (a, b)
  }

  // monads extend semigroupals
  // product is implemented in terms of map/flatMap
  trait MyMonad[M[_]] extends MySemigroupal[M] {
    def pure[A](value: A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    // implement map method in MyMonad - monads are functors!
    def map[A, B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma)(a => pure(f(a)))

    // For Semigroupal
    def product[A, B](fa: M[A], fb: M[B]): M[(A, B)] =
      flatMap(fa)(a => map(fb)(b => (a, b)))
  }

  // But, monads impose the sequencing of computations

  // What if we don't want to impose the sequencing i.e. follow the monad laws? e.g. validated

  // Some semigroupals are useful without being monads
  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]

  val validatedSemigroupal: Semigroupal[ErrorsOr] = Semigroupal[ErrorsOr] // requires implicit Semigroup[List[_]]

  val invalidCombination: ErrorsOr[(Nothing, Nothing)] = validatedSemigroupal.product(
    Validated.invalid(List("something wrong", "something else wrong")),
    Validated.invalid(List("this isn't right"))
  )

  // if combine with monadic type e.g. either get a different result

  type EitherErrorsOr[T] = Either[List[String], T]
  import cats.instances.either._ // implicit Monad[Either]
  val eitherSemigroupal: Semigroupal[EitherErrorsOr] = Semigroupal[EitherErrorsOr]

  val eitherCombination: EitherErrorsOr[(Nothing, Nothing)] = eitherSemigroupal.product(       // in terms of map/flatMap
    Left(List("something wrong", "something else wrong")),
    Left(List("this isn't right"))                         // this error is lost as flatMap short circuits
  )

  // Associativity law:
  //   m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g)
  //   Not true for validated

  // TODO: define a Semigroupal[List] which does a zip
  def zipListSemigroupal: Semigroupal[List] = new Semigroupal[List] {
    override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] = fa.zip(fb)
  }

  def main(args: Array[String]): Unit = {
    println(aTupledOption)
    println(aNoneTupled)
    aTupledFuture.foreach(println)
    println(aTupledList)
    println(productWithMonads(List(1, 2), List("a", "b")))
    println(productWithMonads2(List(1, 2), List("a", "b")))
    println(productWithMonadsFor(List(1, 2), List("a", "b")))
    println(invalidCombination)
    println(eitherCombination)
    println(zipListSemigroupal.product(List(1, 2), List("a", "b")))
    sys.exit(0)
  }

  // Cats TC type hierarchy:
 /*
     ____________             __________       ______________
    |           |            |         |      |             |
    | Semigroup |            | Functor |      | Semigroupal |
    |___________|            |_________|      |_____________|
         ^                       ^                    ^
         |                       |____________________|
         |                                  |
    _____________                       ________
    |           |                      |       |
    |  Monoid   |                      | Monad |
    |___________|                      |_______|
 */
}
