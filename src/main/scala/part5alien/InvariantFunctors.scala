package part5alien

import cats.{Monoid, Show}

object InvariantFunctors {

  trait Crypto[A] { self =>
    def encrypt(value: A): String
    def decrypt(encrypted: String): A

    def imap[B](back: B => A, forth: A => B): Crypto[B] = new Crypto[B] {
      override def encrypt(value: B): String = self.encrypt(back(value))

      override def decrypt(encrypted: String): B = forth(self.decrypt(encrypted))
    }
  }

  def encrypt[A](value: A)(implicit crypto: Crypto[A]): String = crypto.encrypt(value)
  def decrypt[A](repr: String)(implicit crypto: Crypto[A]): A = crypto.decrypt(repr)

  implicit val caesarCypher: Crypto[String] = new Crypto[String] {
    override def encrypt(value: String): String = value.map(c => (c + 2).toChar)
    override def decrypt(encrypted: String): String = encrypted.map(c => (c - 2).toChar)
  }

  // How can we support the Caesar cypher logic for ints, doubles, Option[String] etc.
  implicit val doubleCrypto: Crypto[Double] = caesarCypher.imap[Double](_.toString, _.toDouble)

  // TO DO - support Option[String]
  implicit val optionStringCrypto: Crypto[Option[String]] = caesarCypher.imap[Option[String]](_.getOrElse(""), Option(_))

  // TO DO
  // If you have a Crypto[T], generate a Crypto[Option[T]], if you have a Monoid[T] in scope
  implicit def optionCrypto[T](implicit crypto: Crypto[T], m: Monoid[T]): Crypto[Option[T]] =
    crypto.imap(_.getOrElse(m.empty), Option(_))

  // cats libraries
  import cats.Invariant
  import cats.instances.string._ // Show[String]
  val showString = Show[String]
  val showOptionString: Show[Option[String]] = Invariant[Show].imap(showString)(Option(_))(_.getOrElse(""))

  import cats.syntax.invariant._
  val showOptionString2 = showString.imap(Option(_))(_.getOrElse(""))

  trait MyInvariant[W[_]] {
    def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B]
  }

  trait MyContravariant[W[_]] extends MyInvariant[W] {
    def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B] =
      contramap(wa)(back)

    def contramap[A, B](wa: W[A])(back: B => A): W[B]
  }

  trait MyFunctor[W[_]] extends MyInvariant[W] {
    def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B] =
      map(wa)(forth)

    def map[A, B](wa: W[A])(forth: A => B): W[B]
  }

  def main(args: Array[String]): Unit = {
    val encrypted = encrypt("Let's encrypt")
    val decrypted = decrypt[String](encrypted)

    println(encrypted)
    println(decrypted)

    println(encrypt(Math.PI))
    println(decrypt[Double](encrypt(Math.PI)))

    println(encrypt(Option("Let's encrypt")))
    println(decrypt[Option[String]](encrypt(Option("Let's encrypt"))))

    println(encrypt(Option.empty[String]))
    println(decrypt[Option[String]](encrypt(Option.empty[String])))

    import cats.instances.double._ // Monoid[Double]

    println(encrypt(Option(Math.PI)))
    println(decrypt[Option[Double]](encrypt(Option(Math.PI))))

    println(encrypt(Option.empty[Double]))
    println(decrypt[Option[Double]](encrypt(Option.empty[Double])))

    println(showOptionString.show(Option("Yes")))
    println(showOptionString2.show(Option("Yes")))
    println(showOptionString.show(Option.empty[String]))
    println(showOptionString2.show(Option.empty[String]))
  }

  // Cats TC type hierarchy:
  /*



                                                 ________________
                                                |               |
                                                |   Invariant   |
                                                |               |
                                                |_______________|
                                                       ^    ^
                                                       |    |________________
                                                       |                    |
                                                       |           ________________
                                                       |          |               |
                                                       |          | Contravariant |
                                                       |          |               |
                                                       |          |_______________|
                                                       |
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
