package part4typeclasses

import cats.{Eval, Monoid}

object S6_Folding {

  // TODO - implement all in terms of FoldLeft or FoldRight
  object ListExercises {
    def map[A, B](list: List[A])(f: A => B): List[B] =
      list.foldLeft(List.empty[B]) {
        case (lb, a) =>
          lb :+ f(a)
      }

    def map2[A, B](list: List[A])(f: A => B): List[B] =
      list.foldRight(List.empty[B])((a, lb) => {
        f(a) :: lb
      })

    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
      list.foldLeft(List.empty[B]) {
        case (lb, a) => lb ++ f(a)
      }

    def flatMapAlt[A, B](list: List[A])(f: A => List[B]): List[B] =
      list.foldLeft(List.empty[B]) {
        case (lb, a) =>
          val fa = f(a)
          // This effectively appends f(a) to the end of the list, by prepending everything else to it in reverse order
          lb.foldRight(fa) {
            case (newVal, acc) =>
              newVal :: acc
          }
      }

    def flatMap2[A, B](list: List[A])(f: A => List[B]): List[B] =
      list.foldRight(List.empty[B])((a, lb) => f(a) ++ lb)

    def filter[A](list: List[A])(predicate: A => Boolean): List[A] =
      list.foldLeft(List.empty[A]) {
        case (la, a) => if (predicate(a)) la :+ a  else la
      }

    def filter2[A](list: List[A])(predicate: A => Boolean): List[A] =
      list.foldRight(List.empty[A])((a, la) => if (predicate(a)) a :: la  else la)

    def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A =
      list.foldLeft(monoid.empty)((a1, a2) => {
        monoid.combine(a1, a2)
        // a1 is accumulator
        // a2 is new value
        // "", "1"
        // "1", "2"
        // "12", "3"
        // "123", "4"
      })

    def combineAll2[A](list: List[A])(implicit monoid: Monoid[A]): A =
      list.foldRight(monoid.empty)((a1, a2) => {
        monoid.combine(a1, a2)
        // a1 is new value
        // a2 is accumulator
        // "4", ""
        // "3", "4"
        // "2", "34"
        // "1", "234"
      })
  }

  import cats.Foldable
  import cats.instances.list._ // implicit Foldable[List] ... foldLeft, foldRight etc.

  val sum: Int = Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _) // 6

  import cats.instances.option._ // implicit Foldable[Option]

  val multOption: Int = Foldable[Option].foldLeft(Option(2), 30)(_ * _) // 60

  // foldRight may use stack recursion, so eval makes it stack-safe, regardless of your container
  // it's stack-safe as it chains evals together
  val sumRight: Eval[Int] = Foldable[List].foldRight(List(1, 2, 3), Eval.now(0)) {
    (num, eval) => eval.map(_ + num)
  }

  // convenience methods
  import cats.instances.int._ // implicit Monoid[Int]
  val anotherSum: Int = Foldable[List].combineAll(List(1, 2, 3))

  import cats.instances.string._ // implicit Monoid[String]
  val mappedConcat: String = Foldable[List].foldMap(List(1, 2, 3))(_.toString)

  // nested data structures
  val intsNested = List(Vector(1, 2, 3), Vector(4, 5, 6))

  import cats.instances.vector._ // Foldabled[Vector]

  val composedFoldable: Foldable[({
    //noinspection NonAsciiCharacters
    type λ[α] = List[Vector[α]]
  })#λ] = Foldable[List] compose Foldable[Vector]

  val bigSum: Int = composedFoldable.combineAll(intsNested)

  // extension methods
  import cats.syntax.foldable._

  val sum3: Int = List(1, 2, 3).combineAll                   // requires Foldable[List] and Monoid[Int]
  val mappedConcat2: String = List(1, 2, 3).foldMap(_.toString) // requires Foldable[List] and Monoid[String]

  def main(args: Array[String]): Unit = {
    import ListExercises._
    import cats.instances.int._
    import cats.instances.string._

    val numbers = (1 to 10).toList
    val stringyNumbers = numbers.map(_.toString)

    println(map(stringyNumbers)(_.toInt))
    println(map2(stringyNumbers)(_.toInt))
    println(flatMap(numbers)(x => (1 to x).toList))
    println(flatMapAlt(numbers)(x => (1 to x).toList))
    println(flatMap2(numbers)(x => (1 to x).toList))
    println(filter(numbers)(_ % 2 == 0))
    println(filter2(numbers)(_ % 2 == 0))
    println(combineAll(numbers))
    println(combineAll2(numbers))
    println(combineAll(stringyNumbers))
    println(combineAll2(stringyNumbers))
    println(sum)
    println(multOption)
    println(sumRight.value)
    println(anotherSum)
    println(mappedConcat)
    println(bigSum)
    println(sum3)
    println(mappedConcat2)
  }

  // Cats TC type hierarchy:
  /*
      ____________          _____________          __________       ______________
     |           |         |            |         |         |      |             |
     | Semigroup |         |  Foldable  |         | Functor |      | Semigroupal |
     |  combine  |         |            |         |   map   |      |   product   |
     |___________|         |____________|         |_________|      |_____________|
          ^                                            ^                  ^
          |                                            |__________________|
          |                                                       |
      ____________                                          ______________
     |           |                                         |             |
     |  Monoid   |                                         |    Apply    |
     |  empty    |                                         |     ap      |
     |___________|                                         |_____________|
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
