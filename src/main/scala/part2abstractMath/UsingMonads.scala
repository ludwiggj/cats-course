package part2abstractMath

import part2abstractMath.UsingMonads.ServiceLayer.{EitherHttpService, OptionHttpService, config, getResponse}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object UsingMonads {

  import cats.Monad
  import cats.instances.list._
  import cats.instances.option._

  val monadList: Monad[List] = Monad[List]
  val aSimpleList: List[Int] = monadList.pure(2)
  val anExtendedList: List[Int] = monadList.flatMap(aSimpleList)(x => List(x, x + 1))

  // either is also a monad
  val aManualEither: Either[String, Int] = Right(42)
  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T] = Either[Throwable, T]

  import cats.instances.either._

  val loadingMonad: Monad[LoadingOr] = Monad[LoadingOr]

  val anEither: LoadingOr[Int] = loadingMonad.pure(45)
  val aChangedLoading: LoadingOr[Int] = loadingMonad.flatMap(anEither)(n => if (n % 2 == 0) Right(n + 1) else Left("Oh no!"))

  // an online store
  case class OrderStatus(orderId: Long, status: String)

  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] = Right(OrderStatus(orderId, "Ready to ship"))

  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if (orderStatus.orderId > 1000) Left("Not available yet") else Right("Amsterdam, NL")

  val orderId = 457L

  val orderLocation: LoadingOr[String] = loadingMonad.flatMap(getOrderStatus(orderId))(orderStatus => trackLocation(orderStatus))

  // extension methods required to support for comprehension
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  val orderLocationBetter: LoadingOr[String] = getOrderStatus(orderId).flatMap(trackLocation)

  val orderLocationFor: LoadingOr[String] = for {
    orderStatus <- getOrderStatus(orderId)
    location <- trackLocation(orderStatus)
  } yield location

  // Service layer API of web app

  // Requirements:

  // - if the host and port are found in the configuration map, then we'll return an M containing a connection with
  //   those values, otherwise the method will fail, according to the logic of the type M
  //   (for try it will return a failure, for option it will return none, for future it will be a failed future, for
  //   either it will return a left)

  // - the issueRequest method returns a M containing the string "request (payload) has been accepted", if the payload
  //   is less than 20 characters, otherwise the method will fail, according to the logic of the type M

  // - To do, provide a real implementation using Try, Option, Future, Either
  object ServiceLayer {
    case class Connection(host: String, port: String)

    val config = Map(
      "host" -> "localhost",
      "port" -> "4040"
    )

    trait HttpService[M[_]] {
      def getConnection(cfg: Map[String, String]): M[Connection]

      def issueRequest(conn: Connection, payload: String): M[String]
    }

    def getResponse[M[_]: Monad](service: HttpService[M], payload: String): M[String] =
      for {
        conn <- service.getConnection(config)
        response <- service.issueRequest(conn, payload)
      } yield response

    object TryHttpService extends HttpService[Try] {
      override def getConnection(cfg: Map[String, String]): Try[Connection] =
        for {
          host <- cfg.get("host").toRight(new Exception("Host not found")).toTry
          port <- cfg.get("port").toRight(new Exception("Port not found")).toTry
        } yield Connection(host, port)

      override def issueRequest(conn: Connection, payload: String): Try[String] = {
        if (payload.length < 20)
          Success(s"request ($payload) has been accepted")
        else
          Failure(new Exception(s"payload ($payload) length ${payload.length} should be less than 20 characters"))
      }
    }

    object OptionHttpService extends HttpService[Option] {
      override def getConnection(cfg: Map[String, String]): Option[Connection] =
        for {
          host <- cfg.get("host")
          port <- cfg.get("port")
        } yield Connection(host, port)

      override def issueRequest(conn: Connection, payload: String): Option[String] =
        if (payload.length < 20)
          Some(s"request ($payload) has been accepted")
        else
          None
    }

    implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

    object FutureHttpService extends HttpService[Future] {
      override def getConnection(cfg: Map[String, String]): Future[Connection] = {
        val futureHost = Future(cfg.get("host"))
        val futurePort = Future(cfg.get("port"))
        (for {
          host <- futureHost
          port <- futurePort
        } yield (host, port)).map {
          case (Some(h), Some(p)) => Connection(h, p)
          case _ => throw new Exception("port and/or host could not be located")
        }
      }

      override def issueRequest(conn: Connection, payload: String): Future[String] =
        if (payload.length < 20)
          Future(s"request ($payload) has been accepted")
        else
          Future.failed(new Exception(s"payload ($payload) length ${payload.length} should be less than 20 characters"))
    }

    object EitherHttpService extends HttpService[ErrorOr] {
      override def getConnection(cfg: Map[String, String]): ErrorOr[Connection] =
        for {
          host <- cfg.get("host").toRight(new RuntimeException("Host not found"))
          port <- cfg.get("port").toRight(new RuntimeException("Port not found"))
        } yield Connection(host, port)

      def getConnectionAlt(cfg: Map[String, String]): ErrorOr[Connection] = {
        if (!cfg.contains("host") || !cfg.contains("port")) {
          Left(new RuntimeException("Connection could not be established: invalid configuration"))
        } else {
          Right(Connection(cfg("host"), cfg("port")))
        }
      }

      override def issueRequest(conn: Connection, payload: String): ErrorOr[String] =
        if (payload.length < 20)
          Right(s"request ($payload) has been accepted")
        else
          Left(new RuntimeException(s"payload ($payload) length ${payload.length} should be less than 20 characters"))
    }
  }

  def main(args: Array[String]): Unit = {
    println(anEither)
    println(aChangedLoading)

    println(orderLocation)
    println(orderLocationBetter)
    println(orderLocationFor)

    val responseOptionSuccess = OptionHttpService.getConnection(ServiceLayer.config).flatMap {
      conn => OptionHttpService.issueRequest(conn, "Hello, HTTP service")
    }
    println(responseOptionSuccess)

    val responseOptionFailure = OptionHttpService.getConnection(ServiceLayer.config).flatMap {
      conn => OptionHttpService.issueRequest(conn, "Hello from the Option HTTP service")
    }
    println(responseOptionFailure)

    val responseOptionFor = for {
      conn <- OptionHttpService.getConnection(ServiceLayer.config)
      response <- OptionHttpService.issueRequest(conn, "Hello, HTTP service")
    } yield response
    println(responseOptionFor)

    val errorOrResponse = for {
      conn <- EitherHttpService.getConnection(config)
      response <- EitherHttpService.issueRequest(conn, "Hi, ErrorOr service")
    } yield response
    println(errorOrResponse)

    val errorOrResponseFailure = for {
      conn <- EitherHttpService.getConnection(config)
      response <- EitherHttpService.issueRequest(conn, "Hi, from the ErrorOr HTTP service")
    } yield response
    println(errorOrResponseFailure)

    println(getResponse(OptionHttpService, "Hello, HTTP service"))
    println(getResponse(OptionHttpService, "Hello from the Option HTTP service"))
    println(getResponse(EitherHttpService, "Hi, ErrorOr service"))
    println(getResponse(EitherHttpService, "Hi, from the ErrorOr HTTP service"))
  }
}
