package part3datamanipulation

import cats.kernel.Semigroup

object DataValidation {

  import cats.data.Validated

  val aValidValue: Validated[String, Int] = Validated.valid(42) // right value
  val anInvalidValue: Validated[String, Int] = Validated.invalid("Something went wrong") // left value
  val aTest: Validated[String, Int] = Validated.cond(42 > 39, 99, "meaning of life is too small")

  // TODO: use Either
  // n must be a prime
  // n must be non-negative
  // n <= 100
  // n must be even
  def isPrime(i: Int): Boolean =
    if (i <= 1)
      false
    else if (i == 2)
      true
    else
      !(2 until i).exists(n => i % n == 0)

  import cats.syntax.either._

  def testNumber(n: Int): Either[List[String], Int] = {
    def isAPrime(n: Int): Either[List[String], Int] =
      if (isPrime(n))
        n.asRight[List[String]]
      else
        List(s"$n is not a prime").asLeft[Int]

    def isNotNegative(n: Int): Either[List[String], Int] =
      if (n >= 0)
        n.asRight[List[String]]
      else
        List(s"$n is negative").asLeft[Int]

    def isOneHundredOrLess(n: Int): Either[List[String], Int] =
      if (n <= 100)
        n.asRight[List[String]]
      else
        List(s"$n is greater than 100").asLeft[Int]

    def isEven(n: Int): Either[List[String], Int] =
      if (n % 2 == 0)
        n.asRight[List[String]]
      else
        List(s"$n is odd").asLeft[Int]

    val rules = List(isAPrime(n), isNotNegative(n), isOneHundredOrLess(n), isEven(n))

    rules.foldRight(n.asRight[List[String]])((a, b) => (a, b) match {
      case (r@Right(_), Right(_)) => r
      case (Left(l1), Left(l2)) => Left(l1 ++ l2)
      case (l@Left(_), _) => l
      case (_, l@Left(_)) => l
    })
  }

  // import cats.instances.int._ // Not appropriate as we don't want to add the numbers together
  implicit val combineIntMax: Semigroup[Int] = Semigroup.instance[Int](Math.max)

  import cats.instances.list._

  def validateNumber(n: Int): Validated[List[String], Int] =
    Validated.cond(isPrime(n), n, List(s"$n is not a prime")).combine(
      Validated.cond(n >= 0, n, List(s"$n is negative"))
    ).combine(
      Validated.cond(n <= 100, n, List(s"$n is greater than 100"))
    ).combine(
      Validated.cond(n % 2 == 0, n, List(s"$n is odd"))
    )

  def main(args: Array[String]): Unit = {
    println(testNumber(2))
    println(validateNumber(2))

    println(testNumber(5))
    println(validateNumber(5))

    println(testNumber(18))
    println(validateNumber(18))

    println(testNumber(-6))
    println(validateNumber(-6))

    println(testNumber(101))
    println(validateNumber(101))

    println(testNumber(102))
    println(validateNumber(102))

    println(testNumber(0))
    println(validateNumber(0))
  }
}
