package part1recap

object TCVariance {
  import cats.Eq
  import cats.instances.int._    // Eq[Int] TC instance
  import cats.instances.option._ // Construct a Eq[Option[Int]] TC instance
  import cats.syntax.eq._        // Supports === operator

  private val comparison = Eq[Option[Int]].eqv(Option(2), Option(2)) // Uses cats.Eq

  private val aComparison = Option(2) === Option(3)

  // variance
  class Animal
  class Cat extends Animal // Cat <: Animal

  // covariant type: subtyping is propagated to the generic type
  class Cage[+T]
  val cage: Cage[Animal] = new Cage[Cat] // Cat <: Animal, so Cage[Cat] <: Cage[Animal]

  // contravariant type: subtyping is propagated BACKWARDS to the generic type
  class Vet[-T]

  // I want a cat vet, but I can use an animal vet instead
  val vet: Vet[Cat] = new Vet[Animal] // Cat <: Animal, then Vet[Animal] <: Vet[Cat]

  // rule of thumb:
  //   "HAS a T"   = covariant
  //   "ACTS on T" = contravariant
  // variance affects how TC instances are fetched

  // contravariant TC
  trait SoundMaker[-T]

  // Cat <: Animal, then SoundMaker[Animal] <: SoundMaker[Cat]
  implicit object AnimalSoundMaker extends SoundMaker[Animal]

  // implementation not important
  def makeSound[T](implicit soundMaker: SoundMaker[T]): Unit = println("making a sound")

  // has implications for subtypes
  implicit object OptionSoundMaker extends SoundMaker[Option[Int]]

  // covariant TC
  trait AnimalShow[+T] {
    def show: String
  }

  implicit object GeneralAnimalShow extends AnimalShow[Animal] {
    override def show = "animals everywhere"
  }

  implicit object CatsShow extends AnimalShow[Cat] {
    override def show = "so many cats!"
  }

  // Cat <: Animal, so AnimalShow[Cat] <: AnimalShow[Animal]
  def organizeShow[T](implicit event: AnimalShow[T]): String = event.show

  def main(args: Array[String]): Unit = {
    println(s"Eq[Option[Int]].eqv(Option(2), Option(2)) = $comparison")
    println(s"Option(2) === Option(3) = $aComparison")

    // rule 1: contravariant TCs can use the superclass instances if nothing is available strictly for that type

    makeSound[Animal] // ok - TC instance defined above
    makeSound[Cat]    // ok - TC instance for Animal is also applicable to Cats

    makeSound[Option[Int]]
    makeSound[Some[Int]]   // Again, generic TC used

    // rule 2: covariant TCs will always use the more specific TC instance for that type
    println(organizeShow[Cat])       // ok - the compiler will inject CatsShow as implicit

    // but won't compile if the general TC is also present
    // println(organizeShow[Animal]) // will not compile - ambiguous values

    // rule 3: you can't have both benefits
    // Cats uses INVARIANT TCs

    // val anInvalidComparison = Some(2) === None // Eq[Some[Int]] not found
    val comp = Option(2) === Option.empty[Int]
    println(s"Option(2) === Option.empty[Int] = $comp")
  }
}