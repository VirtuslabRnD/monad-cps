import cats.data.Reader
import scala.continuations.*
import cats.effect.IO
import cats.data.EitherT
import cats.effect.unsafe.implicits.global
import cats.implicits._
import cats.catsInstancesForId

object TestReader:

  def toInt: String => Int =
    _.toInt

  val r = MonadExecutor[[X] =>> Reader[String, X]].run {
    Reader(toInt).extract
  }

  val e = MonadErrorExecutor[[X] =>> Either[String, X], String].run {
    val a: Either[String, Int] = Right(2)
    a.extractError
  }

  val t = MonadExecutor[[X] =>> EitherT[IO, String, X]].run {
    val a = EitherT(IO(Right(3): Either[String, Int]))
    a.extract
  }

  val t0 = MonadExecutor[[X] =>> EitherT[cats.Id, String, X]].run {
    val a = EitherT[cats.Id, String, Int](Right(3))
    a.extractError
  }

  def fallible1: Either[String, Double] = Right(1.0)

  def asInt(a: Double): Either[String, Int] = if a.isWhole then Right(a.toInt) else Left("Not whole")

  val myEither =  MonadErrorExecutor[[X] =>> Either[String, X], String].run {
    val d: Double = 2.5 * fallible1.extractError
    val r: Int = asInt(d).extract
    // println("I'm here?")
    r + 5
  }

  val myEither2 =  MonadErrorExecutor[[X] =>> Either[String, X], String].run {
    val d: Double = 10 * fallible1.extract
    val r: Int = asInt(d).extract
    // println("I'm here!")
    // println("And r = " + r)
    r + 5
  }

  @main def test =
    // println(r.run("23"))
    println(e)
    // println(t.value.unsafeRunSync())
    println(myEither)
    println(myEither2)

end TestReader
