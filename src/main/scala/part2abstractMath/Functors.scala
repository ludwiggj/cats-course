package part2abstractMath

import part2abstractMath.Functors.Tree.{branch, leaf}
import scala.util.Try

object Functors {

  val aModifiedList: List[Int] = List(1, 2, 3).map(_ + 1)
  val aModifiedOption: Option[Int] = Option(2).map(_ + 1)
  val aModifiedTry: Try[Int] = Try(42).map(_ + 1)

  // simplified definition
  trait MyFunctor[F[_]] {
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  import cats.Functor
  import cats.instances.list._ // includes Functor[List]

  val listFunctor: Functor[List] = Functor[List]

  val incrementedNumbers: Seq[Int] = listFunctor.map(List(1, 2, 3))(_ + 1)

  import cats.instances.option._ // includes Functor[Option]

  val optionFunctor: Functor[Option] = Functor[Option]

  val incrementedOption: Option[Int] = optionFunctor.map(Option(2))(_ + 1)

  import cats.instances.try_._
  val anIncrementedTry: Try[Int] = Functor[Try].map(Try(42))(_ + 1)

  // generalised API
  def do10xList(list: List[Int]): List[Int] = list.map(_ * 10)
  def do10xOption(option: Option[Int]): Option[Int] = option.map(_ * 10)
  def do10xTry(attempt: Try[Int]): Try[Int] = attempt.map(_ * 10)

  def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] = functor.map(container)(_ * 10)

  // A tree functor
  trait Tree[+T]
  case class Leaf[+T](value: T) extends Tree[T]
  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  object Tree {
    // smart constructors
    def leaf[T](value: T): Tree[T] = Leaf(value)
    def branch[T](value: T, lt: Tree[T], rt: Tree[T]): Tree[T] = Branch(value, lt, rt)
  }

  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(a) => Leaf(f(a))
      case Branch(a, lt, rt) => Branch(f(a), map(lt)(f), map(rt)(f))
    }
  }

  // Extension method - map
  import cats.syntax.functor._
  val tree: Tree[Int] = Tree.branch(40, Tree.branch(5, Tree.leaf(10), Tree.leaf(30)), Tree.leaf(20))

  def do10xShorter[F[_]: Functor](container: F[Int]): F[Int] = container.map(_ * 10)

  def main(args: Array[String]): Unit = {
    println(incrementedNumbers)
    println(incrementedOption)
    println(anIncrementedTry)

    println(do10x(List(1, 2, 3)))
    println(do10x(Option(3)))
    println(do10x(Try(16)))

    println(do10x[Tree](Leaf(6)))
    println(do10x(leaf(21)))
    println(do10x(branch(1, branch(2, leaf(3), leaf(4)), leaf(5))))
    println(tree.map(_ + 1))
    println(do10xShorter(branch(1, branch(2, leaf(3), leaf(4)), leaf(5))))
  }
}
