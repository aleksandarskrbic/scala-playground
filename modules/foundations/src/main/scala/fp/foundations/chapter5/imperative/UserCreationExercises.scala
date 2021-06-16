package fp.foundations.chapter5.imperative

import fp.foundations.chapter5.imperative._

import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDate}
import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

// Run the App using the green arrow next to object (if using IntelliJ)
// or run `sbt` in the terminal to open it in shell mode, then type:
// exercises/runMain exercises.action.imperative.UserCreationApp
object UserCreationApp extends App {
  import UserCreationExercises._

  readUser(Console.system, Clock.system)
}

object UserCreationExercises {
  // If you use `y` you should also use `G`. Since `G` is rarely used,
  // the correct year symbol is `u`, not `y`, otherwise a non-positive year
  // will show incorrectly.
  // https://stackoverflow.com/a/41178418
  val dateOfBirthFormatter: DateTimeFormatter =
  DateTimeFormatter.ofPattern("dd-MM-uuuu")

  case class User(name: String, dateOfBirth: LocalDate, subscribed: Boolean, createdAt: Instant)

  def readSubscribeToMailingList(): Boolean = {
    println("Would you like to subscribe to our mailing list? [Y/N]")
    parseYesNo(StdIn.readLine())
  }

  def parseYesNo(line: String): Boolean =
    line match {
      case "Y"   => true
      case "N"   => false
      case other => throw new IllegalArgumentException(s"""Expected "Y" or "N" but received $other""")
    }

  def formatYesNo(yesNo: Boolean): String =
    if (yesNo) "Y" else "N"

  def readSubscribeToMailingList(console: Console): Boolean = {
    console.writeLine("Would you like to subscribe to our mailing list? [Y/N]")
    parseYesNo(console.readLine())
  }

  def readDateOfBirth(console: Console): LocalDate = {
    console.writeLine("What's your date of birth? [dd-mm-yyyy]")
    parseDate(console.readLine())
  }

  def parseDate(line: String): LocalDate =
    Try(LocalDate.parse(line, dateOfBirthFormatter))
      .getOrElse(
        throw new IllegalArgumentException(s"Expected a date with format dd-mm-yyyy but received $line")
      )

  def readName(console: Console): String = {
    console.writeLine("What's your name?")
    console.readLine()
  }

  def readUser(console: Console, clock: Clock): User = {
    val name        = readName(console)
    val dateOfBirth = readDateOfBirthRetryV2(console, 3)
    val subscribed  = readSubscribeToMailingListRetryV2(console, 3)
    val now         = clock.now()
    val user        = User(name, dateOfBirth, subscribed, now)
    console.writeLine(s"User is $user")
    user
  }

  def readSubscribeToMailingListRetry(console: Console, maxAttempt: Int): Boolean =
    retry(maxAttempt) {
      console.writeLine("Would you like to subscribe to our mailing list? [Y/N]")
      val line = console.readLine()
      onError(
        action = parseYesNo(line),
        cleanup = _ => console.writeLine("""Incorrect format, enter "Y" for Yes or "N" for "No"""")
      )
    }


  // same but with a while loop instead of recursion
  def readSubscribeToMailingListRetryWhileLoop(console: Console, maxAttempt: Int): Boolean = {
    var subscribed: Try[Boolean] = Failure(new IllegalArgumentException("maxAttempt must be greater than 0"))
    var remaining: Int           = maxAttempt

    while (subscribed.isFailure && remaining > 0) {
      remaining -= 1
      console.writeLine("Would you like to subscribe to our mailing list? [Y/N]")
      subscribed = Try(parseYesNo(console.readLine()))

      if (subscribed.isFailure)
        console.writeLine(s"""Incorrect format, enter "Y" for Yes or "N" for "No"""")
    }

    subscribed.get
  }

  @tailrec
  def readDateOfBirthRetry(console: Console, maxAttempt: Int): LocalDate = {
    require(maxAttempt > 0, "maxAttempt must be greater than 0")

    console.writeLine("What's your date of birth? [dd-mm-yyyy]")
    val line = console.readLine()
    Try(parseDate(line)) match {
      case Success(value) => value
      case Failure(error) =>
        console.writeLine("""Incorrect format, for example enter "18-03-2001" for 18th of March 2001""")
        if (maxAttempt > 1) readDateOfBirthRetry(console, maxAttempt - 1)
        else throw error
    }
  }

  def readSubscribeToMailingListRetryV2(console: Console, maxAttempt: Int): Boolean =
    retry(maxAttempt)(
      onError(
        action = {
          console.writeLine("Would you like to subscribe to our mailing list? [Y/N]")
          parseYesNo(console.readLine())
        },
        cleanup = _ => console.writeLine(s"""Incorrect format, enter "Y" for Yes or "N" for "No"""")
      )
    )

  def readDateOfBirthRetryV2(console: Console, maxAttempt: Int): LocalDate =
    retry(maxAttempt)(
      onError(
        action = {
          console.writeLine("What's your date of birth? [dd-mm-yyyy]")
          parseDate(console.readLine())
        },
        cleanup = _ => console.writeLine("""Incorrect format, for example enter "18-03-2001" for 18th of March 2001""")
      )
    )

}