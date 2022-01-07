package part3datamanipulation

import cats.Id
import cats.data.WriterT

import java.util.concurrent.Executors
import scala.annotation.tailrec
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}

object Writers {

  import cats.data.Writer

  // 1 - Define writer
  val aWriter: Writer[List[String], Int] = Writer(List("Started something"), 45)

  // 2 - Manipulate with pure fp
  val anIncreaseWriter: WriterT[Id, List[String], Int] = aWriter.map(_ + 1) // value increases, log stays the same

  val aLogWriter: WriterT[Id, List[String], Int] = aWriter.mapWritten(_ :+ "found somethin g interesting") // value stays the same, log changes

  val aWriterWithBoth: WriterT[Id, List[String], Int] = aWriter.bimap(_ :+ "found something interesting", _ + 1) // value and log both change

  // value and log both change
  val aWriterWithBoth2: WriterT[Id, List[String], Int] = aWriter.mapBoth { (log, value) =>
    // have access to both existing values
    (log :+ s"found something interesting, increasing value from $value", value + 1)
  }

  val writerA: WriterT[Id, Vector[String], Int] = Writer(Vector("Log A1", "Log A2"), 10)
  val writerB: WriterT[Id, Vector[String], Int] = Writer(Vector("Log B1"), 40)

  // Flatmap

  // Needs a semigroup for Vector so that it can combine the logs

  import cats.instances.vector._ // imports a Semigroup[Vector]

  val compositeWriter: WriterT[Id, Vector[String], Int] = for {
    va <- writerA
    vb <- writerB
  } yield va + vb

  // reset the logs - need a monoid (which has the empty/zero value)

  import cats.instances.list._ // an implicit Monoid[List[Int]]

  val anEmptyList: WriterT[Id, List[String], Int] = aWriter.reset // clear the logs, keep the value

  // 3 - Dump either value or logs
  val desiredValue: Id[Int] = aWriter.value

  val log: Id[List[String]] = aWriter.written

  val (l, v) = aWriter.run

  // TODO 1: rewrite a function which "prints" things with writers
  def countAndSay(n: Int): Unit = {
    if (n <= 0)
      println("starting!")
    else {
      countAndSay(n - 1)
      println(n)
    }
  }

  // This solution is not tail recursive
  def countAndLog(n: Int): Writer[Vector[String], Int] =
    if (n <= 0)
      Writer(Vector("starting!"), 0)
    else
      countAndLog(n - 1).bimap(_ :+ n.toString, _ => n)

  //noinspection ScalaUnusedSymbol
  // This solution is not tail recursive
  def countAndLog2(n: Int): Writer[Vector[String], Int] =
    if (n <= 0)
      Writer(Vector("starting!"), 0)
    else
      countAndLog2(n - 1).flatMap(_ => Writer(Vector(s"$n"), n))

  // Tail recursive - base case is a bit fiddly
  def countAndLogTailRec(n: Int): Writer[Vector[String], Int] = {
    @tailrec
    def loop(x: Int, acc: Writer[Vector[String], Int]): Writer[Vector[String], Int] = {
      if (x <= 0) {
        val log = acc.written
        val finalValue = log.head.toInt
        Writer(Vector("starting!") ++ log.reverse, finalValue)
      } else
        loop(x - 1, acc.flatMap(_ => Writer(Vector(s"$x"), x)))
    }

    loop(n, Writer(Vector(), 0))
  }

  // Tail recursive - iterating in the other direction is better in this case
  def countAndLogTailRecBetter(n: Int): Writer[Vector[String], Int] = {
    @tailrec
    def loop(x: Int, acc: Writer[Vector[String], Int]): Writer[Vector[String], Int] = {
      if (x > n) {
        acc
      } else
        loop(x + 1, acc.flatMap(_ => Writer(Vector(s"$x"), x)))
    }

    loop(1, Writer(Vector("starting!"), 0))
  }

  // Benefit 1 - we work with pure FP

  // TODO 2
  def naiveSum(n: Int): Int = {
    if (n <= 0) 0
    else {
      println(s"Now at $n")
      val lowerSum = naiveSum(n - 1)
      println(s"Computed sum(${n - 1}) = $lowerSum")
      lowerSum + n
    }
  }

  // This solution is not tail recursive
  // This solution omits the "Now at..." messages
  def sumAndLog(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector(), 0) else {
      sumAndLog(n - 1).mapBoth { (log, value) =>
        (log :+ s"Computed sum(${n - 1}) = $value", value + n)
      }
    }
  }

  // This solution is not tail recursive
  // This solution includes the "Now at..." messages
  def sumAndLogWithNowMessages(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector(), 0) else {
      Writer(Vector(s"Now at $n"), n).flatMap(_ =>
        sumAndLogWithNowMessages(n - 1).mapBoth { (log, value) =>
          (log :+ s"Computed sum(${n - 1}) = $value", value + n)
        }
      )
    }
  }

  // This solution is tail recursive
  // This solution omits the "Now at..." messages
  // In this case the log is wrong - reversing isn't going to help us
  def sumAndLogTailRecWrong(n: Int): Writer[Vector[String], Int] = {
    @tailrec
    def loop(x: Int, writer: Writer[Vector[String], Int]): Writer[Vector[String], Int] = {
      if (x <= 0)
        writer
      else
        loop(x - 1, writer.mapBoth { (log, value) =>
          (log :+ s"Computed sum(${x - 1}) = $value", value + x)
        })
    }

    loop(n, Writer(Vector(), 0))
  }

  // This solution is tail recursive
  // Again, ascending order is better

  // This solution omits the "Now at..." messages
  // Trying to add in the "Now at" messages breaks tail recursion
  def sumAndLogTailRec(n: Int): Writer[Vector[String], Int] = {
    @tailrec
    def loop(x: Int, writer: Writer[Vector[String], Int]): Writer[Vector[String], Int] = {
      if (x > n)
        writer
      else
        loop(x + 1, writer.mapBoth { (log, value) =>
          (log :+ s"Computed sum(${x - 1}) = $value", value + x)
        })
    }

    loop(1, Writer(Vector(), 0))
  }

  // The textbook solution includes the "Now at..." messages, but is not tail recursive
  def sumAndLogTextbook(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector(), 0) else for {
      _ <- Writer(Vector(s"Now at $n"), n)
      lowerSum <- sumAndLogTextbook(n - 1)
      _ <- Writer(Vector(s"Computed sum(${n - 1}) = $lowerSum"), n)
    } yield lowerSum + n
  }

  def main(args: Array[String]): Unit = {
    println(writerA.run)
    println(writerB.run)
    println(compositeWriter.run)

    println(aWriter.run)
    println(anEmptyList.run)

    countAndSay(10)
    println(countAndLog(10).run)
    println(countAndLogTailRec(10).run)
    println(countAndLogTailRecBetter(10).run)

    countAndSay(5)
    println(countAndLog(5).run)
    println(countAndLogTailRec(5).run)
    println(countAndLogTailRecBetter(5).run)

    def showNaiveSumResults(n: Int): Unit = {
      println(s"\nsum($n)...\n")
      println(s"naiveSum($n)                 => ${naiveSum(n)}")
      println(s"sumAndLog($n)                => ${sumAndLog(n).run}")
      println(s"sumAndLogWithNowMessages($n) => ${sumAndLogWithNowMessages(n).run}")
      println(s"sumAndLogTailRecWrong($n)    => ${sumAndLogTailRecWrong(n).run}")
      println(s"sumAndLogTailRec($n)         => ${sumAndLogTailRec(n).run}")
      println(s"sumAndLogTextbook($n)        => ${sumAndLogTextbook(n).run}")
    }

    showNaiveSumResults(0)
    showNaiveSumResults(1)
    showNaiveSumResults(2)
    showNaiveSumResults(3)

    println("\nInto the mingled future!\n")

    implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

    // The output of t two futures intermingles
    val f1 = Future(naiveSum(100))
    val f2 = Future(naiveSum(100))

    println(Await.result(Future.sequence(Vector(f1, f2)), 5.seconds))

    // This is solved via writers

    // Benefit 2 - logs from separate threads are kept apart

    println("\nInto the separated future!\n")

    val sumFuture1 = Future(sumAndLogTextbook(100))
    val sumFuture2 = Future(sumAndLogTextbook(100))

    val logs1 = sumFuture1.map(_.written).map(_.map(l => s"T1 => $l")) // logs from thread 1
    val logs2 = sumFuture2.map(_.written).map(_.map(l => s"T2 => $l")) // logs from thread 2

    val allLogs = for {
      l1 <- logs1
      l2 <- logs2
    } yield l1 ++ l2

    Await.result(allLogs, 5.seconds).foreach(println)

    System.exit(0)
  }
}
