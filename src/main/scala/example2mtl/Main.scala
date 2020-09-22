package example2mtl

import cats.Monad
import cats.data.StateT
import cats.effect.{ExitCode, IO, IOApp, Sync}
import cats.mtl._
import cats.implicits._

import scala.util.Try

// mtl gives us some extra typeclasses to abstract over behaviour
// a common example is ApplicativeError which lets us manage errors, but that's so common it's in base cats
// I'll show you Stateful in stead here

trait BankAccountAlg[F[_]] {
  def add(amount: BigDecimal): F[Unit]
  def remove(amount: BigDecimal): F[Unit]
  def balance: F[BigDecimal]
}

object BankAccountAlg {
  // so let's make one where it depends on having some state
  implicit def bankAccountAlgFromState[F[_]: Monad](implicit state: Stateful[F, BigDecimal]): BankAccountAlg[F] = new BankAccountAlg[F] {
    override def add(amount: BigDecimal): F[Unit] =
      state.modify(_ + amount)
    override def remove(amount: BigDecimal): F[Unit] =
      state.modify(_ - amount)
    override def balance: F[BigDecimal] =
      state.get
  }

  // see how simple was that? we've asked for a `Stateful[F, BigDecimal]` and we can just modify, get or set it
}

// I'm going to make this a bit interactive so let's make a console
trait Console[F[_]] {
  def print(s: String): F[Unit]
  def ask(s: String): F[String]
}

object Console {
  // we'll allow a console to be created for anything that can contain a Synchronous effect
  implicit def consoleForSync[F[_]: Sync]: Console[F] = new Console[F] {
    override def print(s: String): F[Unit] = Sync[F].delay {
      println(s)
    }
    override def ask(s: String): F[String] = Sync[F].delay {
      scala.io.StdIn.readLine(s)
    }
  }
}

object Main extends IOApp {
  // so, let's define a program that's tagless, in this case we're going to use both a Console and a BankAccountAlg
  def program[F[_]: Sync](implicit console: Console[F], account: BankAccountAlg[F]): F[Unit] = for {
    // we'll open by printing a welcome message
    _ <- console.print(
      """Welcome to the bank
        |To add to your balance say "add 12.34"
        |To remove from your balance say "remove 43.21"
        |And to quit say "quit"""".stripMargin
    )
    // now, we're going to declare the looping part in a value called `go`
    go = for {
      // we open by asking for some input with a chevron at the start of the line
      line <- console.ask("> ")
      // we're going to match on what it is, and this will return if the program should exit
      shouldExit <- line.split(" ").toList match {
          // if they've asked to exit, we'll just return true, to tell the loop to end
        case "quit" :: _ => true.pure[F]
          // if they've asked to add some money we'll do this
        case "add" :: arg :: _ =>
          for {
            // parse out the bigdecimal in their second argument
            parsed  <- Sync[F].fromEither(Try { BigDecimal(arg) }.toEither)
            // add it to the account
            _       <- account.add(parsed)
            // ask for the new balance
            balance <- account.balance
            // tell them what happened
            _       <- console.print(s"$parsed has been added, your new balance is $balance")
          } yield false
          // and similarly for remove, we'll do the same stuff as add but flipped
        case "remove" :: arg :: _ =>
          for {
            parsed  <- Sync[F].fromEither(Try { BigDecimal(arg) }.toEither)
            _       <- account.remove(parsed)
            balance <- account.balance
            _       <- console.print(s"$parsed has been removed, your new balance is $balance")
          } yield false
          // anything else we can print an error and loop
        case other =>
          console.print(s"unrecognised input: $line").as(false)
      }
    } yield shouldExit
    // we take a pure action that does nothing, and loop it while `go` returns false
    _ <- ().pure[F].whileM_(go.map(!_))
  } yield ()

  // and when we run our program for real, we pick the StateT monad transformer as our state
  // we run it with empty BigDecimal Monoid as it's input, ie. BigDecimal(0)
  // and we'll just return Success whatever happens
  override def run(args: List[String]): IO[ExitCode] =
    program[StateT[IO, BigDecimal, *]].runEmpty.as(ExitCode.Success)
}
