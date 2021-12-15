package part2abstractMath

import java.util.concurrent.Executors
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}

object Monads {
  // lists
  val numbersList = List(1, 2, 3)
  val charsList = List('a', 'b', 'c')

  // Create all combinations
  val listCombo: Seq[(Int, Char)] = for {
    n <- numbersList
    c <- charsList
  } yield (n, c)

  // options
  val numberOption: Option[Int] = Option(2)
  val charOption: Option[Char] = Option('d')

  val optionCombo: Option[(Int, Char)] = for {
    n <- numberOption
    c <- charOption
  } yield (n, c)

  // futures
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  val numberFuture: Future[Int] = Future(42)
  val charFuture: Future[Char] = Future('Z')

  val futureCombo: Future[(Int, Char)] = for {
    n <- numberFuture
    c <- charFuture
  } yield (n, c)

  // Pattern
  // - wrapping value into a monadic (M) value
  // - flatMap mechanism

  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    // implement map method in MyMonad - monads are functors!
    def map[A, B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma)(a => pure(f(a)))
  }

  import cats.Monad
  import cats.instances.option._ // implicit Monad[Option]

  val optionMonad: Monad[Option] = Monad[Option]
  val anOption: Option[Int] = optionMonad.pure(4)
  val aTransformedOption: Option[Int] = optionMonad.flatMap(anOption)(x => if (x % 3 == 0) Some(x + 1) else None)

  import cats.instances.list._
  val listMonad: Monad[List] = Monad[List]
  val aList: List[Int] = listMonad.pure(3)
  val aTransformedList: List[Int] = listMonad.flatMap(aList)(x => List(x, x + 1))

  // Use a Monad[Future]
  import cats.instances.future._
  val futureMonad: Monad[Future] = Monad[Future]
  val aFuture: Future[Int] = futureMonad.pure(2040)
  val aTransformedFuture: Future[(Int, Int)] = futureMonad.flatMap(aFuture)(x => Future((x, x + 1)))

  // Specialised API
  def getPairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] =
    numbers.flatMap(n => chars.map(c => (n, c)))

  def getPairsOption(number: Option[Int], char: Option[Char]): Option[(Int, Char)] =
    number.flatMap(n => char.map(c => (n, c)))

  def getPairsFuture(number: Future[Int], char: Future[Char]): Future[(Int, Char)] =
    number.flatMap(n => char.map(c => (n, c)))

  // Generalised API
  def getPairs[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] =
    monad.flatMap(ma)(a => monad.map(mb)(b => (a, b)))

  // extension methods
  import cats.syntax.applicative._ // pure
  val oneOption: Option[Int] = 1.pure[Option] // implicit Monad[Option] is used, Some(1)
  val oneList: List[Int] = 1.pure[List]

  import cats.syntax.flatMap._
  val oneOptionTransformed: Option[Int] = oneOption.flatMap(x => (x + 1).pure[Option])

  import cats.syntax.functor._
  val oneOptionMapped: Option[Int] = optionMonad.map(oneOption)(_ + 2)
  val oneOptionMapped2: Option[Int] = oneOption.map(_ + 2)

  // for comprehensions
  val composedOptionFor = for {
    one <- 1.pure[Option]
    two <- 2.pure[Option]
  } yield one + two

  // Shorter version of getPairs
  def getPairs2[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] =
    ma.flatMap(a => mb.map(b => (a, b)))

  def getPairsFor[M[_]: Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] = {
    for {
      a <- ma
      b <- mb
    } yield (a, b)
  }

  def main(args: Array[String]): Unit = {
    println(listCombo)
    println(optionCombo)
    println(Await.result(futureCombo, 1.seconds))

    println(anOption)
    println(aTransformedOption)

    println(aList)
    println(aTransformedList)

    println(Await.result(aFuture, 1.seconds))
    println(Await.result(aTransformedFuture, 1.seconds))

    println(getPairsList(numbersList, charsList))
    println(getPairsOption(numberOption, charOption))
    println(Await.result(getPairsFuture(numberFuture, charFuture), 1.second))

    println(getPairs(numbersList, charsList))
    println(getPairs(numberOption, charOption))
    println(Await.result(getPairs(numberFuture, charFuture), 1.second))

    getPairs(numberFuture, charFuture).foreach(println)

    println(oneOption)
    println(oneList)

    println(oneOptionTransformed)
    println(oneOptionMapped)
    println(oneOptionMapped2)
    println(composedOptionFor)

    println(getPairs2(numbersList, charsList))
    println(getPairs2(numberOption, charOption))
    getPairs2(numberFuture, charFuture).foreach(println)

    println(getPairsFor(numbersList, charsList))
    println(getPairsFor(numberOption, charOption))
    getPairsFor(numberFuture, charFuture).foreach(println)

    System.exit(0)
  }
}
