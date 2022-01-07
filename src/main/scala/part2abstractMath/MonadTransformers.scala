package part2abstractMath

import java.util.concurrent.Executors
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}

// Monad Transformers are not typeclasses
// They are higher-kinded types for convenience over nested monadic values
object MonadTransformers {
  //noinspection NotImplementedCode,ScalaUnusedSymbol
  def sumAllOptions(values: List[Option[Int]]): Int = ???

  // option transformer
  import cats.data.OptionT
  import cats.instances.list._ // implicit OptionT[List]

  val listOfNumberOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2))) // wrapper over List[Option[Int]]
  val listOfCharOptions: OptionT[List, Char] = OptionT(List(Option('a'), Option('b'), Option.empty[Char]))

  val listOfTuples: OptionT[List, (Int, Char)] = for { // can access map/flatMap with implicit Monad[List] in scope
    char <- listOfCharOptions                          // don't need to unwrap options
    number <- listOfNumberOptions
  } yield (number, char)

  // either transformer - transformer for monadic values of Either instances
  import cats.data.EitherT
  // apply factory method
  val listOfEithers: EitherT[List, String, Int] = EitherT(List(Left("something wrong"), Right(43), Right(2)))
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  import cats.syntax.either._
  import cats.instances.future._ // for functor

  val futureOfEithers: EitherT[Future, String, Int] = EitherT(Future[Either[String, Int]](Right(45)))
  val futureOfEithersBetter: EitherT[Future, String, Int] = EitherT(Future(45.asRight[String]))
  val futureOfEithersEvenBetter: EitherT[Future, String, Int] = EitherT.right(Future(45)) // wrap over Future(Right(45))

  // We have a multi-machine cluster for your business which will receive a traffic surge following a media appearance
  // We measure bandwidth in units
  // We want to allocate two of our servers to cope with the traffic spike
  // We know the current capacity for each server and we know we'll hold the traffic if the sum of bandwidths > 250

  private val server1 = "server1.rockthejvm.com"
  private val server2 = "server2.rockthejvm.com"
  private val server3 = "server3.rockthejvm.com"

  val invalidServer = "server1.rockthejvmz.com"

  val bandwidths = Map(
    server1 -> 50,
    server2 -> 300,
    server3 -> 170
  )

  type AsyncResponse[T] = EitherT[Future, String, T] // wrapper over Future[Either[String, T]]

  def getBandwidth(server: String): AsyncResponse[Int] = bandwidths.get(server) match {
//    case None => EitherT(Future(s"Server $server unreachable".asLeft[Int]))
//    case Some(b) => EitherT(Future(b.asRight[String]))

    // alternative left and right methods
    case None => EitherT.left(Future(s"Server $server unreachable"))
    case Some(b) => EitherT.right(Future(b))
  }

  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = {
    for {
      s1Bandwidth <- getBandwidth(s1)
      s2Bandwidth <- getBandwidth(s2)
    } yield s1Bandwidth + s2Bandwidth > 250
    // wrapper over Future[Either[String, Boolean]]
  }

  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] =
    canWithstandSurge(s1, s2).map(
      ok => if (ok) {
        s"Servers $s1, $s2 can withstand surge"
      } else {
        s"Servers $s1, $s2 cannot withstand surge"
      }
    )

  def generateBetterTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] = {
    // change (i.e. transform) the deeply nested either, without having to unwrap the outside monadic type (future in this case)
    canWithstandSurge(s1, s2).transform {
      case Left(reason) => Left(s"Servers $s1 and $s2 CANNOT cope with the incoming spike: $reason")
      case Right(false) => Left(s"Servers $s1 and $s2 CANNOT cope with the incoming spike: not enough total bandwidth")
      case Right(true) => Right(s"Servers $s1 and $s2 CAN cope with the incoming spike")
      // ^^^^^^^^^^^^^                        ^^^^^^^^^^^^^
      // Future[Either[String, Boolean]] ---> Future[Either[String, String]]
    }
  }

  def main(args: Array[String]): Unit = {
    println(listOfTuples.value)

    println(Await.result(canWithstandSurge(server1, server2).value, 1.second))
    println(Await.result(canWithstandSurge(server1, server3).value, 1.second))
    println(Await.result(canWithstandSurge(invalidServer, server3).value, 1.second))

    println(Await.result(generateTrafficSpikeReport(server1, server2).value, 1.second))
    println(Await.result(generateTrafficSpikeReport(server1, server3).value, 1.second))
    println(Await.result(generateTrafficSpikeReport(invalidServer, server3).value, 1.second))

    println(Await.result(generateBetterTrafficSpikeReport(server1, server2).value, 1.second))
    println(Await.result(generateBetterTrafficSpikeReport(server1, server3).value, 1.second))
    println(Await.result(generateBetterTrafficSpikeReport(invalidServer, server3).value, 1.second))

    System.exit(0)
  }
}
