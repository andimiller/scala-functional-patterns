package example1tagless

import cats.implicits._
import cats.effect._

import scala.concurrent.{ExecutionContext, Future}
import scala.io.StdIn.readLine

// If you're using maths terms, an interface is called an Algebra, sometimes you'll see traits ending in Alg to indicate that
// There's a type of algebra called tagless, where we keep the effect generic so it can be selected later
trait ConsoleAlg[F[_]] {
  // you'll see this interface takes a type parameter of F[_], this is generic so we can pick it later

  // and our methods use the F to contain the output
  def readline: F[String]
  def print(s: String): F[Unit]
}

object ConsoleAlg {
  // we'll implement a few of these here to show how you'd use it

  // in "traditional" scala we might use Future
  def futureConsole(implicit ec: ExecutionContext): ConsoleAlg[Future] = new ConsoleAlg[Future] {
    override def readline: Future[String] = Future { readLine() }
    override def print(s: String): Future[Unit] = Future { println(s) }
  }

  // once we're using a concrete IO monad it's like this
  val ioConsole = new ConsoleAlg[IO] {
    override def readline: IO[String] = IO { readLine() }
    override def print(s: String): IO[Unit] = IO { println(s) }
  }

  // and if we're using some abstract IO monad, where we want to abstract using a typeclass we'd do this
  implicit def syncConsole[F[_]: Sync]: ConsoleAlg[F] = new ConsoleAlg[F] {
    override def readline: F[String] = Sync[F].delay { readLine() }
    override def print(s: String): F[Unit] = Sync[F].delay { println(s) }
  }
}

object Main extends IOApp {
  // we can write our program in a "tagless" way by abstracting it with the typeclass, to delay the choice of the type to even later
  def program[F[_]: Sync](implicit console: ConsoleAlg[F]): F[ExitCode] = for {
    _    <- console.print("hello, what is your name?")
    name <- console.readline
    _    <- console.print(s"hello $name")
  } yield ExitCode.Success

  override def run(args: List[String]): IO[ExitCode] =
    program[IO] // so our main method picks the specific IO monad, and the rest of our code is generic
}