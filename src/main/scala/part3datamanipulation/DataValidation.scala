package part3datamanipulation

import cats.kernel.Semigroup

import scala.util.Try

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

  // Not appropriate as we don't want to add the numbers together (result below would be Right(8) instead of Right(2))
  //import cats.instances.int._
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

  // chain
  aValidValue.andThen(_ => anInvalidValue)

  def validateNumberFailFast(n: Int): Validated[List[String], Int] =
    Validated.cond(isPrime(n), n, List(s"$n is not a prime")).andThen(_ =>
      Validated.cond(n >= 0, n, List(s"$n is negative"))
    ).andThen(_ =>
      Validated.cond(n <= 100, n, List(s"$n is greater than 100"))
    ).andThen(_ =>
      Validated.cond(n % 2 == 0, n, List(s"$n is odd"))
    )

  // test a valid value
  aValidValue.ensure(List("something went wrong"))(_ % 2 == 0)

  // transform
  aValidValue.map(_ + 1)
  aValidValue.leftMap(_.length)
  aValidValue.bimap(_.length, _ + 1)

  // interoperates with standard lib
  val eitherToValidated: Validated[List[String], Int] = Validated.fromEither(Right(42))
  val optionToValidated: Validated[List[String], Int] = Validated.fromOption(None, List("nothing present here"))
  val tryToValidated: Validated[Throwable, Int] = Validated.fromTry(Try("something".toInt))

  // backwards
  eitherToValidated.toEither
  optionToValidated.toOption
  // there's no toTry method

  // TODO - form validation
  object FormValidation {
    type FormValidation[T] = Validated[List[String], T]

    /* fields are:
    - name
    - email
    - password

    rules are:
    - name, email and password MUST be specified
    - name must not be blank
    - email must have @
    - password must have >= 10 characters
     */
    def validateForm(form: Map[String, String]): FormValidation[String] = {
      import cats.instances.string._

      def getValue(form: Map[String, String], fieldName: String): FormValidation[String] =
        Validated.fromOption(form.get(fieldName), List(s"The field $fieldName must be specified"))

      def nonBlank(value: String, fieldName: String): FormValidation[String] =
        Validated.cond(value.nonEmpty, s"[$value]", List(s"The field $fieldName must not be blank"))

      def containsCharacter(value: String, fieldName: String, character: Char): FormValidation[String] =
        Validated.cond(value.contains(character), s"[$value]", List(s"The field $fieldName must contain a $character"))

      def minLength(value: String, fieldName: String, minLength: Int): FormValidation[String] =
        Validated.cond(
          value.length >= minLength,
          s"[$value]",
          List(s"The field $fieldName must have at least $minLength characters (it has ${value.length})")
        )

      val nameValidation = getValue(form, "name").andThen(
        n => nonBlank(n, "name")
      )

      val emailValidation = getValue(form, "email").andThen(
        e => containsCharacter(e, "email", '@')
      )

      val passwordValidation = getValue(form, "password").andThen(
        p => minLength(p, "password", 10)
      )

      nameValidation.combine(emailValidation).combine(passwordValidation).map(_ => "Success")
    }
  }

  import cats.syntax.validated._ // similar to either
  val aValidMeaningOfLife: Validated[List[String], Int] = 42.valid[List[String]]
  val anError: Validated[String, Int] = "Something went wrong".invalid[Int]

  def main(args: Array[String]): Unit = {
    println(testNumber(2))
    println(validateNumber(2))
    println(validateNumberFailFast(2))

    println(testNumber(5))
    println(validateNumber(5))
    println(validateNumberFailFast(5))

    println(testNumber(18))
    println(validateNumber(18))
    println(validateNumberFailFast(18))

    println(testNumber(-6))
    println(validateNumber(-6))
    println(validateNumberFailFast(-6))

    println(testNumber(101))
    println(validateNumber(101))
    println(validateNumberFailFast(101))

    println(testNumber(102))
    println(validateNumber(102))
    println(validateNumberFailFast(102))

    println(testNumber(0))
    println(validateNumber(0))
    println(validateNumberFailFast(0))

    println(FormValidation.validateForm(
      Map("name" -> "Graeme", "email" -> "graeme.ludwig@btopenworld.com", "password" -> "shh, it's a secret")
    ))

    println("Missing name, invalid email")
    println(FormValidation.validateForm(
      Map("email" -> "graeme.ludwigbtopenworld.com", "password" -> "shh, it's a secret")
    ))

    println("Blank name, missing email, password too short")
    println(FormValidation.validateForm(
      Map("name" -> "", "password" -> "shh")
    ))

  }
}
