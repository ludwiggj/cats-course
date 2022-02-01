package part4typeclasses

object S2_Applicatives {
  // Applicatives = Functors + pure

  import cats.Applicative
  import cats.instances.list._

  val listApplicative: Applicative[List] = Applicative[List]

  val aList: Seq[Int] = listApplicative.pure(2) // List(2)

  import cats.instances.option._ // implicit Applicative[Option]

  val optionApplicative: Applicative[Option] = Applicative[Option]

  val anOption: Option[Int] = optionApplicative.pure(2) // Some(2)

  // pure extension method
  import cats.syntax.applicative._
  val aSweetList: Seq[Int] = 2.pure[List]
  val aSweetOption: Option[Int] = 2.pure[Option]

  // Monads extend applicatives (which is where monad gets its pure method from)
  // Applicatives extend functors

  // Applicatives rarely used in and of themselves, most data structures have monadic property (which is also applicative)]
  // Main exception is Validated

  // Validated does not respect monadic laws, but can wrap values and map them
  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]

  val aValidValue: ErrorsOr[Int] = Validated.valid(42)                      // "pure" valid is like a pure method
                                                                            // ErrorsOr is the wrapper type
  val aModifiedValue: Validated[List[String], Int] = aValidValue.map(_ + 1) // map

  // Validated is not a monad, but is a candidate for Applicative as its strongest typeclass
  // Cats can construct an Applicative, where pure is translated into valid i.e. in Validated.scala:

  // private[data] class ValidatedApplicative[E: Semigroup] extends CommutativeApplicative[Validated[E, *]] {
  //  override def map[A, B](fa: Validated[E, A])(f: A => B): Validated[E, B] =
  //    fa.map(f)
  //
  //  def pure[A](a: A): Validated[E, A] = Validated.valid(a)
  //
  //  etc...
  //}
  val validApplicative: Applicative[ErrorsOr] = Applicative[ErrorsOr]

  // TODO
  def productWithApplicatives[W[_], A, B](wa: W[A], wb: W[B])(implicit applicative: Applicative[W]): W[(A, B)] = {
    //noinspection ScalaUnusedSymbol
    val xxx: W[W[(A, B)]] = applicative.map(wa)(a => applicative.map(wb)(b => (a, b))) // oh dear!
    applicative.product(wa, wb)                                                        // this is implemented using ap (see next bit)
  }

  //noinspection ScalaUnusedSymbol,NotImplementedCode
  // Assume the following function - can we now implement productWithApplicatives?
  def ap[W[_], A, B](wf: W[A => B], wa: W[A]): W[B] = ??? // this is already implemented in cats

  def productWithApplicatives2[W[_], A, B](wa: W[A], wb: W[B])(implicit applicative: Applicative[W]): W[(A, B)] = {
    val functionWrapper: W[B => (A, B)] = applicative.map(wa)(a => (b: B) => (a, b))
    ap(functionWrapper, wb)
  }

  // Applicatives have the ap method defined above i.e.
  def productWithApplicatives3[W[_], A, B](wa: W[A], wb: W[B])(implicit applicative: Applicative[W]): W[(A, B)] = {
    val functionWrapper: W[B => (A, B)] = applicative.map(wa)(a => (b: B) => (a, b))
    applicative.ap(functionWrapper)(wb)
  }

  // Applicatives can implement product from semigroupal, via ap
  // Applicatives can extend Semigroupals

  def main(args: Array[String]): Unit = {
    println(aList)
    println(anOption)
    println(validApplicative.pure(12))
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
     |  Monoid   |                    | Applicative |
     |  empty    |                    |  ap  pure   |
     |___________|                    |_____________|
                                            ^
                                            |
                                         ________
                                        |       |
                                        | Monad |
                                        |_______|
  */
}
