package part5alien

import cats.Monoid

object ContravariantFunctors {

  trait Format[T] { // contravariant type class
    def format(value: T): String

    def contramap[A](func: A => T): Format[A] = (value: A) => format(func(value))
  }

  def format[A](value: A)(implicit f: Format[A]): String = f.format(value)

  implicit object StringFormat extends Format[String] {
    override def format(value: String): String = "\"" + value + "\""
  }

  implicit object IntFormat extends Format[Int] {
    override def format(value: Int): String = value.toString
  }

  implicit object BooleanFormat extends Format[Boolean] {
    override def format(value: Boolean): String = if (value) "Y" else "N"
  }

  // problem: given Format[MyType], can we have a Format[Option[MyType]], or a List etc.
  def getOptionFormat1[T](implicit f: Format[T]): Format[Option[T]] = {
    case Some(t) => s"Some(${f.format(t)})"
    case None => "None"
  }

  def getOptionFormat2[T](implicit f: Format[T]): Format[Option[T]] = (value: Option[T]) => f.format(value.get)

  def getOptionFormat3[T](implicit f: Format[T]): Format[Option[T]] = contramap[Option[T], T](_.get)

  def contramap[A, T](func: A => T)(implicit f: Format[T]): Format[A] =
    (value: A) => f.format(func(value))

  def getOptionFormat4[T](implicit f: Format[T]): Format[Option[T]] = f.contramap[Option[T]](_.get)

  implicit def getOptionFormat[T](implicit f: Format[T], m: Monoid[T]): Format[Option[T]] =
    f.contramap[Option[T]](_.getOrElse(m.empty))

/*
   IntFormat = Format[Int]
   format(Option(Option(42))))

   f0: Format[Option[Int]]         = IntFormat.contramap[Option[Int]](_.get)   // T = Int in implicit def getOptionFormat[T]...
                                                                               // first get
   f1: Format[Option[Option[Int]]] = f0.contramap[Option[Option[Int]]](_.get)  // T = Option[Int] in implicit def getOptionFormat[T]...
                                                                               // second get

   Now substitute:

   f1 = IntFormat.contramap[Option[Int]](_.get)
                 .contramap[Option[Option[Int]]](_.get)

   Remember:
     def contramap[A](func: A => T): Format[A] = (value: A) => format(func(value))

   f1.format(Option(Option(42)))
   f0.format(secondGet(Option(Option(42))))
   IntFormat.format(firstGet(secondGet(Option(Option(42)))))

   order = REVERSE of written order
   - second get
   - first get
   - format of Int

  i.e.

  IntFormat.contramap[Option[Int]](_.get)          // applied 2nd
           .contramap[Option[Option[Int]]](_.get)  // applied 1st

  Map applies transformations in sequence
  Contramap applies transformations in reverse sequence
*/

  import cats.Contravariant
  import cats.Show
  import cats.instances.string._ // implicit Monoid[String]
  import cats.instances.int._    // implicit Show[Int], Monoid[Int]
  import cats.instances.option._ // implicit Monoid[Option]

  val showInts = Show[Int]
  val showOption: Show[Option[Int]] = Contravariant[Show].contramap(showInts)(_.getOrElse(0))

  import cats.syntax.contravariant._
  val showOptionShorter: Show[Option[Int]] = showInts.contramap(_.getOrElse(0))

  def main(args: Array[String]): Unit = {
    println(format("Yo"))
    println(format(42))
    println(format(true))
    println(format(Option("Yo")))
    println(format(Option(42)))

    // Can't use boolean as cats doesn't define a monoid for it
    // println(format(Option(true)))

    println(format(Option(Option(42))))
    println(format(Option.empty[Int]))

    println(showOption.show(Some(5)))
    println(showOptionShorter.show(Some(5)))

    println(showOption.show(None))
    println(showOptionShorter.show(None))
  }
}
