package part4typeclasses

import cats.{Applicative, Eval, Foldable, Functor, Monad}

import java.util.concurrent.Executors
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}

object S7_Traversing {
  implicit val ex: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val servers: List[String] = List("server-ci.rockthejvm.com", "server-staging.rockthejvm.com", "prod.rockthejvm.com")

  def getBandwidth(hostname: String): Future[Int] = Future(hostname.length * 80)

  // we have
  //   - a List[String]
  //   - a func String => Future[Int]
  // we want a Future[List[Int]]

  // following works but we have to create, wrap and unwrap futures
  val allBandwidths: Future[List[Int]] = servers.foldLeft(Future(List.empty[Int]))(
    (acc, server) => acc.flatMap(l => getBandwidth(server).map(bw => l :+ bw))
  )

  val allBandwidthsTraverse: Future[List[Int]] = Future.traverse(servers)(getBandwidth)
  val allBandwidthsSequence: Future[List[Int]] = Future.sequence(servers.map(getBandwidth))

  // TODO 1
  import cats.syntax.applicative._ // for pure
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def listTraverse[F[_]: Monad, A, B](list: List[A])(func: A => F[B]): F[List[B]] = {
    list.foldLeft(List.empty[B].pure[F])(
      (flb, a) => for {
        lb <- flb
        b <- func(a)
      } yield lb :+ b
    )
  }

  import cats.syntax.apply._ // for mapN

  def listWeakerTraverse[F[_]: Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] = {
    list.foldLeft(List.empty[B].pure[F])(
      (flb, a) => (flb, func(a)).mapN(_ :+ _)
    )
  }

  def listSequence[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] =
    listWeakerTraverse[F, F[A], A](list)(identity)

  def listSequenceExpanded[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] = {
    list.foldLeft(List.empty[A].pure[F])(
      (fla, fa) => {
        (fla, fa).mapN((la, a) => {
          la :+ a
        })
      }
    )
  }

  import cats.instances.option._

  def filterAsOption(list: List[Int])(predicate: Int => Boolean): Option[List[Int]] = {
    listTraverse[Option, Int, Int](list)(n => Some(n).filter(predicate))
  }

  import cats.data.Validated
  import cats.instances.list._ // Semigroup[List] => Applicative[ErrorsOr]

  type ErrorsOr[T] = Validated[List[String], T]

  def filterAsValidated(list: List[Int])(predicate: Int => Boolean): ErrorsOr[List[Int]] =
    listWeakerTraverse[ErrorsOr, Int, Int](list) { n =>
      if (predicate(n)) Validated.valid(n)
      else Validated.invalid(List(s"predicate for $n failed"))
    }

  // Generalise traverse to a container
  // Extends Foldable as traverse uses foldLeft
  // It doesn't make any other assumptions e.g. existence of an empty container -
  // implementation of traverse left to individual containers
  trait MyTraverse[L[_]] extends Foldable[L] with Functor[L] {
    def traverse[F[_]: Applicative, A, B](container: L[A])(func: A => F[B]): F[L[B]]
    def sequence[F[_]: Applicative, A](container: L[F[A]]): F[L[A]] = traverse(container)(identity)

    type Identity[T] = T

    // Cats can summon an Applicative[Identity] which is required for traverse (via catsInstancesForId)
    def map[A, B](wa: L[A])(f: A => B): L[B] =
      traverse[Identity, A, B](wa)(f)

    // This is the same idea as cats.Id
    import cats.Id // Fake wrapper, Applicative[Id] is available for traverse (via catsInstancesForId)
    def map2[A, B](wa: L[A])(f: A => B): L[B] =
      traverse[Id, A, B](wa)(f)
  }

  import cats.Traverse
  import cats.instances.future._ // Applicative[Future]

  val allBandwidthsCats: Future[List[Int]] = Traverse[List].traverse(servers)(getBandwidth)

  // extension methods
  import cats.syntax.traverse._ // sequence + traverse
  val allBandwidthsCats2: Future[List[Int]] = servers.traverse(getBandwidth)

  def main(args: Array[String]): Unit = {
    println(Await.result(allBandwidths, 1.seconds))
    println(Await.result(allBandwidthsTraverse, 1.seconds))
    println(Await.result(allBandwidthsSequence, 1.seconds))

    import cats.instances.future._

    println(Await.result(listTraverse(servers)(getBandwidth), 1.seconds))
    println(Await.result(listWeakerTraverse(servers)(getBandwidth), 1.seconds))
    println(Await.result(listSequence(servers.map(getBandwidth)), 1.seconds))

    import cats.instances.vector._

    // Vector(4, 5, 6, 8, 10, 12, 12, 15, 18)
    println((Vector(1, 2, 3), Vector(4, 5, 6)).mapN(_ * _)) // mapN produces cross-product

    // listSequenceExpanded includes (F[List[A]], F[A]).mapN
    // In following case, (Vector[List[A]], Vector[A]), cross-product of List[A] and [A], starting with empty list

    // Vector(List(1), List(2)) = all single elements
    println(listSequenceExpanded[Vector, Int](List(Vector(1, 2))))

    // Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4)) = all pairs
    println(listSequenceExpanded[Vector, Int](List(Vector(1, 2), Vector(3, 4))))

    // Vector(List(1, 3, 5), List(1, 3, 6), List(1, 4, 5), List(1, 4, 6),
    //        List(2, 3, 5), List(2, 3, 6), List(2, 4, 5), List(2, 4, 6)) = all triples
    println(listSequence[Vector, Int](List(Vector(1, 2), Vector(3, 4), Vector(5, 6))))

    println(filterAsOption(List(2, 4, 6))(_ % 2 == 0)) // all true, Some(List(2, 4, 6))
    println(filterAsOption(List(1, 2, 3))(_ % 2 == 0)) // some true, None

    println(filterAsValidated(List(2, 4, 6))(_ % 2 == 0)) // all true, Valid(List(2, 4, 6))
    println(filterAsValidated(List(1, 2, 3))(_ % 2 == 0)) // some true, Invalid(List(predicate for 1 failed, predicate for 3 failed))

    println(Await.result(allBandwidthsCats, 1.seconds))

    // Needs to implement methods from Foldable[List] (can't use implicit Foldable[List])
    object MyTraverseList extends MyTraverse[List] {
      override def traverse[F[_] : Applicative, A, B](container: List[A])(func: A => F[B]): F[List[B]] =
        container.traverse(func)

      override def foldLeft[A, B](fa: List[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      override def foldRight[A, B](fa: List[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.foldRight(lb)(f)
    }

    println(MyTraverseList.map(List(1, 2, 3, 4))(_ * 3))
    println(MyTraverseList.map2(List(1, 2, 3, 4))(_ * 3))

    sys.exit(0)
  }

  // Cats TC type hierarchy:
  /*
      ____________          _____________          __________       ______________
     |           |         |            |         |         |      |             |
     | Semigroup |         |  Foldable  |         | Functor |      | Semigroupal |
     |  combine  |         |            |         |   map   |      |   product   |
     |___________|         |____________|         |_________|      |_____________|
          ^                       ^                 ^     ^               ^
          |                       |_________________|     |_______________|
          |                                |                      |
      ____________                   _____________          ______________
     |           |                  |            |         |             |
     |  Monoid   |                  |  Traverse  |         |    Apply    |
     |  empty    |                  |  traverse  |         |     ap      |
     |___________|                  |____________|         |_____________|
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
