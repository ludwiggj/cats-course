package part3datamanipulation

object Evaluation {

  // Cats makes the distinction between:
  // - evaluating an expression eagerly (once)
  // - evaluating lazily and every time you request it
  // - evaluating lazily and keeping the value (memoizing)

  import cats.Eval

  // Eager evaluation, computes once
  val instantEval: Eval[Int] = Eval.now {
    println("Computing now!")
    64532
  }

  // Lazy, recalculating every time
  val redoEval: Eval[Int] = Eval.always {
    println("Computing again!")
    4234
  }

  // Lazy, memoizing
  val delayedEval: Eval[Int] = Eval.later {
    println("Computing later!")
    1231
  }

  val composedEval: Eval[Int] = instantEval.flatMap(v1 => delayedEval.map(v2 => v1 + v2))

  val anotherComposedEval: Eval[Int] = for {
    v1 <- instantEval
    v2 <- delayedEval
  } yield v1 + v2

  val evalEx1: Eval[Int] = for {
    a <- delayedEval
    b <- redoEval
    c <- instantEval
    d <- redoEval
  } yield a + b + c + d

  // remember a computed value
  val doNotRecompute: Eval[Int] = redoEval.memoize

  val tutorial: Eval[String] = Eval
    .always {
      println("Step 1"); "put guitar on your lap"
    }
    .map { step1 => println("Step 2"); s"$step1, then put your left hand on the neck" }
    .memoize // remember the value up to this point
    .map { steps12 => println("Step 3, more complicated"); s"$steps12 then with the right hand strike the strings"
    }

  // TODO 2: implement defer such that defer(Eval.now) does NOT run the side effects

  // This version isn't stack safe (only map and flaMap are trampolined)
  def defer[T](eval: => Eval[T]): Eval[T] = Eval.later(eval.value)

  // This version isn't stack safe either
  def defer2[T](eval: => Eval[T]): Eval[T] = Eval.later(()).map(_ => eval.value)

  // This version is stack safe - effectively the same as Eval.Defer - uses flatMap
  def deferTextbook[T](eval: => Eval[T]): Eval[T] = Eval.later(()).flatMap(_ => eval)

  // TODO 3
  def reverseList[T](list: List[T]): List[T] =
    if (list.isEmpty) list
    else reverseList(list.tail) :+ list.head

  def reverseEval[T](list: List[T]): Eval[List[T]] =
    defer(Eval.later(reverseList(list)))

  def reverseEvalRewritten[T](list: List[T]): Eval[List[T]] =
    if (list.isEmpty) Eval.now(list)
    else deferTextbook(reverseEvalRewritten(list.tail).map(_ :+ list.head))

  def reverseEvalRewritten2[T](list: List[T]): Eval[List[T]] =
    if (list.isEmpty) Eval.now(list)
    else defer2(reverseEvalRewritten2(list.tail).map(_ :+ list.head))

  def main(args: Array[String]): Unit = {
    //    println(instantEval.value)
    //    println(instantEval.value)

    //    println(redoEval.value)
    //    println(redoEval.value)

    //    println(delayedEval.value)
    //    println(delayedEval.value)

    //    println(composedEval.value)
    //    println(composedEval.value)

    //    println(anotherComposedEval.value)
    //    println(anotherComposedEval.value)

    //    println(evalEx1.value)
    //    println(evalEx1.value)

    // Output:
    //   Computing now!    // instantEval
    //   Computing later!  // delayedEval
    //   Computing again!  // redoEval
    //   Computing again!  // redoEval
    //   <sum>
    //   Computing again!  // redoEval
    //   Computing again!  // redoEval
    //   <sum>

//    println(doNotRecompute.value)
//    println(doNotRecompute.value)

//    println(tutorial.value)
//    println(tutorial.value)

//    println("Deferring...")
//    val deferred = defer(Eval.now {
//      println("Now!")
//      42
//    })
//    println("Now go for it...")
//    println(deferred.value)

    def reverseTest(numOfElems: Int, eval: List[Int] => Eval[List[Int]]): Unit = {
      val l = (1 to numOfElems).toList
      println("Before reversing")
      val delayedReverse = eval(l)

      println("Let's reverse!")
      println(s"Delayed reverse: ${delayedReverse.value}")
    }

    val l = List(4, 3, 2, 1)
    println(s"Reversed: ${reverseList(l)}")

    // Uses defer - crashes and burns as it's not trampolined
    // reverseTest(10000, reverseEval)

    // Uses deferTextbook  - creates a chain of flatMaps - this works as flatMap is trampolined
    reverseTest(10000, reverseEvalRewritten)

    // Uses deferTextbook2 - creates a chain of maps     - this works as map is trampolined, but doesn't work, psyche!
    reverseTest(10000, reverseEvalRewritten2)
  }
}
