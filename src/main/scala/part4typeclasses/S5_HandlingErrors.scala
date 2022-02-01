package part4typeclasses

import cats.{Applicative, Monad}

import java.util.concurrent.Executors
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Try

object S5_HandlingErrors {

  // E can be anything that means an error, not necessarily a JVM exception type
  trait MyApplicativeError[M[_], E] extends Applicative[M] {
    // pure method from Applicative

    // abstract method
    def raiseError[A](e: E): M[A]
    def handleErrorWith[A](ma: M[A])(func: E => M[A]): M[A]

    // handleError can be expressed in terms of handleErrorWith
    def handleError[A](ma: M[A])(func: E => A): M[A] = handleErrorWith(ma)(e => pure(func(e)))
  }

  trait MyMonadError[M[_], E] extends MyApplicativeError[M, E] with Monad[M]  {
    // M[A] somehow encapsulates E; various implementations e.g. Either, Try, Future
    def ensure[A](ma: M[A])(error: E)(predicate: A => Boolean): M[A]
  }

  import cats.MonadError          // Most monads also have a MonadError available
                                  // Compiler can compute instances of MonadError if there is a Monad in scope

  import cats.instances.either._  // implicit MonadError for any kind of either that will store an exception
                                  // i.e. makes implicit catsStdInstancesForEither available (see below),
                                  // from package cats.instances.EitherInstances

  type ErrorOr[A] = Either[String, A]
  val monadErrorEither = MonadError[ErrorOr, String] // Error type of MonadError must match error
                                                     //  type of ErrorOr i.e. String

  // Cats provides an implicit called catsStdInstancesForEither to above declaration i.e.
  //   val monadErrorEither = MonadError[ErrorOr, String](catsStdInstancesForEither)

  // Type signature of catsStdInstancesForEither is:
  //
  // implicit def catsStdInstancesForEither[A]
  //    : MonadError[Either[A, *], A] with Traverse[Either[A, *]] with Align[Either[A, *]]
  //
  // This explains why error type must be consistent

  // MonadError has access to all Monad methods
  val success: Either[String, Int] = monadErrorEither.pure(23)                           // Right(23)
  val failure: Either[String, Int] = monadErrorEither.raiseError[Int]("Something wrong") // Left("Something wrong")
  val badness: Either[String, Int] = monadErrorEither.raiseError[Int]("Badness")         // Left("badness")

  // recover from error - map failures to values
  def handledError(errorOr: ErrorOr[Int]): ErrorOr[Int] = monadErrorEither.handleError(errorOr) {
    case "Badness" => 44 // Int
    case _ => 89
  }

  // recover from error - map failures to values or failures
  def handledError2(errorOr: ErrorOr[Int]): ErrorOr[Int] = monadErrorEither.handleErrorWith(errorOr) {
    case "Badness" => monadErrorEither.pure(99) // ErrorOr[Int]
    case "Oh dear" => Left("Something else")    // Has enough type hints to figure this out
    case _ => failure
  }

  // These are equivalent to Try & Future's recover & recoverWith methods

  // Ensure is typically used as an extension method
  def filtered(errorOr: ErrorOr[Int]): ErrorOr[Int] = monadErrorEither.ensure(errorOr)("Number too small")(_ > 100)

  val bigSuccess: Either[String, Int] = monadErrorEither.pure(123)

  // Try and Future
  import cats.instances.try_._ // implicit MonadError[Try, Throwable]

  val exception = new RuntimeException("Really bad")

  // Store exception in purely functional way
  val tryException: Try[Int] = MonadError[Try, Throwable].raiseError(exception) // failure

  // Cats provides an implicit called catsStdInstancesForTry to above declaration i.e.
  //   val tryException = MonadError[Try, Throwable](catsStdInstancesForTry)

  // Type signature of catsStdInstancesForTry is:
  //
  // implicit def catsStdInstancesForTry
  //    : MonadError[Try, Throwable] with CoflatMap[Try] with Traverse[Try] with Monad[Try]
  //
  // This explains why error type must be Throwable

  import cats.instances.future._
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  val futureException: Future[Int] = MonadError[Future, Throwable].raiseError(exception) // future that completes with a failure

  // Cats provides an implicit called catsStdInstancesForFuture to above declaration i.e.
  //   val futureException = MonadError[Future, Throwable](catsStdInstancesForFuture)

  // Type signature of catsStdInstancesForFuture is:
  //
  // implicit def catsStdInstancesForFuture(
  //    implicit ec: ExecutionContext
  // ): MonadError[Future, Throwable] with CoflatMap[Future] with Monad[Future]
  //
  // This explains why error type must be Throwable

  // Applicatives - applicativeError
  import cats.data.Validated
  import cats.instances.list._ // implicit Semigroup[List[String]]

  type ErrorsOr[T] = Validated[List[String], T]
  import cats.ApplicativeError

  // implicit is catsDataApplicativeErrorForValidated,
  // where:
  // implicit def catsDataApplicativeErrorForValidated[E](implicit E: Semigroup[E]): ApplicativeError[Validated[E, *], E]

  // So given an implicit Semigroup[List[String]] (from above), we now have:
  //
  // implicit def catsDataApplicativeErrorForValidated[List[String]]: ApplicativeError[Validated[List[String], *], List[String]]
  //
  // implicit def catsDataApplicativeErrorForValidated[List[String]]: ApplicativeError[ErrorsOr, List[String]]
  val applErrorVal = ApplicativeError[ErrorsOr, List[String]]

  // ApplicativeError has access to:
  //   pure
  //   raiseError
  //   handleError
  //   handleErrorWith

  // Question: What is the difference between ApplicativeError and MonadError?
  // Answer:   ensure! (see above)

  // Extension methods - ApplicativeError
  import cats.syntax.applicative._      // pure
  import cats.syntax.applicativeError._ // raiseError, handleError, handleErrorWith

  // pure requires an implicit F: Applicative[F]
  // fulfilled in this case by catsDataApplicativeErrorForValidated, as the
  // ApplicativeError it produces extends Applicative
  val extendedSuccess: ErrorsOr[Int] = 42.pure[ErrorsOr] // requires implicit ApplicativeError[ErrorsOr, List[String]]

  // raiseError requires wrapper type and value type, it returns wrapper type of value type
  // implicit is catsDataApplicativeErrorForValidated
  val extendedError: ErrorsOr[Int] = List("Badness").raiseError[ErrorsOr, Int] // requires ApplicativeError with List[String] as error type

  // recover is equivalent of handleError
  // implicit is catsDataApplicativeErrorForValidated
  val recoveredError: ErrorsOr[Int] = extendedError.recover {
    case _ => 43
  }

  // recoverWith is equivalent of handleErrorWith
  // implicit is catsDataApplicativeErrorForValidated
  val recoveredWithError: ErrorsOr[Int] = extendedError.recoverWith {
    case _ => 43.pure
  }

  // Extension methods - MonadError
  import cats.syntax.monadError._      // ensure

  val testedSuccess: ErrorOr[Int] = success.ensure("Something bad")(_ > 100)

  def main(args: Array[String]): Unit = {
    println(success)
    println(failure)

    println(handledError(success)) // Right(23)
    println(handledError(failure)) // Left("Something wrong") becomes Right(89)
    println(handledError(badness)) // Left("badness") becomes Right(44)

    println(handledError2(success)) // Right(23)
    println(handledError2(failure)) // Left("Something wrong")
    println(handledError2(badness)) // Left("badness") becomes Right(99)

    println(filtered(success))    // Left("Number too small")
    println(filtered(bigSuccess)) // Right(123)
    println(filtered(failure))    // Left("Something wrong")
    println(filtered(badness))    // Left("badness")

    println(tryException)        // Failure(java.lang.RuntimeException: Really bad)
    // println(Await.result(futureException, 1.second)) // Throws exception
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
                                 _________________________
                                |                        |
                         ______________           ______________
                        |             |          |             |
                        |   FlatMap   |          | Applicative |
                        |   flatMap   |          |    pure     |
                        |_____________|          |_____________|
                               ^                      ^  ^
                               |______________________|  |__________
                                            |                      |
                                        __________         ___________________
                                       |         |        |                  |
                                       |  Monad  |        | ApplicativeError |
                                       |         |        |    raiseError    |
                                       |         |        |  handleErrorWith |
                                       |_________|        |__________________|
                                            ^                     ^
                                            |_____________________|
                                                      |
                                                _______________
                                               |              |
                                               |  MonadError  |
                                               |    ensure    |
                                               |______________|
  */
}
