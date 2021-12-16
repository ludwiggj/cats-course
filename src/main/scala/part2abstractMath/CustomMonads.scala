package part2abstractMath

import scala.annotation.tailrec

object CustomMonads {

  import cats.Monad

  implicit object OptionMonad extends Monad[Option] {
    override def pure[A](x: A): Option[A] = Option(x)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa flatMap f

    // must not overflow stack
    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match {
      case None => None
      case Some(Left(v)) => tailRecM(v)(f)
      case Some(Right(b)) => Some(b)
    }
  }

  // Define a monad for an entity type
  type Identity[T] = T
  val aNumber: Identity[Int] = 42

  implicit object IdentityMonad extends Monad[Identity] {
    override def pure[A](a: A): Identity[A] = a

    override def flatMap[A, B](a: Identity[A])(f: A => Identity[B]): Identity[B] = f(a)

    // must not overflow stack
    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] = f(a) match {
      case Left(v) => tailRecM(v)(f)
      case Right(b) => b
    }
  }

  // Tree - harder
  sealed trait Tree[+A]

  final case class Leaf[+A](value: A) extends Tree[A]

  final case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

  implicit object TreeMonad extends Monad[Tree] {
    override def pure[A](x: A): Tree[A] = Leaf(x)

    override def flatMap[A, B](ta: Tree[A])(f: A => Tree[B]): Tree[B] = ta match {
      case Leaf(a) => f(a)
      case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
    }

    def tailRecMStackR[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      def stackRec(t: Tree[Either[A, B]]): Tree[B] = t match {
        case Leaf(Left(a)) => stackRec(f(a))
        case Leaf(Right(b)) => pure(b)
        case Branch(lt, rt) => Branch(stackRec(lt), stackRec(rt))
      }

      stackRec(f(a))
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      /*
               ----1----
            --2--       --3--
           /     \     /     \
          L1     R2   R3     R4

       tr([1], [], [])
       tr([3, 2, 1], [1], [])
       tr([R4, R3, 3, 2, 1], [3, 1], [])
       tr([R3, 3, 2, 1], [3, 1], [B4])
       tr([3, 2, 1], [3, 1], [B3, B4])
       tr([2, 1], [3, 1], [B34])
       tr([R2, L1, 2, 1], [2, 3, 1], [B34])
       tr([L1, 2, 1], [2, 3, 1], [B2, B34])
       tr([R1, 2, 1], [2, 3, 1], [B2, B34]) // Assumes f(L1) gives a right
       tr([2, 1], [2, 3, 1], [B1, B2, B34])
       tr([1], [2, 3, 1], [B12, B34])
       tr([], [2, 3, 1], [B1234])
       B1234
       */
      @tailrec
      def tailRec(todo: List[Tree[Either[A, B]]], expanded: List[Tree[Either[A, B]]], done: List[Tree[B]]): Tree[B] =
        if (todo.isEmpty)
          done.head
        else todo.head match {
          case Leaf(Left(v)) => tailRec(f(v) :: todo.tail, expanded, done)
          case Leaf(Right(b)) => tailRec(todo.tail, expanded, Leaf(b) :: done)
          case node @ Branch(left, right) =>
            if (!expanded.contains(node)) {
              tailRec(right :: left :: todo, node :: expanded, done)
            } else {
              val newLeft = done.head
              val newRight = done.tail.head
              val newBranch = Branch(newLeft, newRight)
              tailRec(todo.tail, expanded, newBranch :: done.drop(2))
            }
        }
      tailRec(List(f(a)), List(), List())
    }
  }

  def main(args: Array[String]): Unit = {
    val tree: Tree[Int] = Branch(Leaf(10), Leaf(20))
    val changedTree = TreeMonad.flatMap(tree)(v => Branch(Leaf(v + 1), Leaf(v + 2)))
    println(tree)
    println(changedTree)
  }
}
