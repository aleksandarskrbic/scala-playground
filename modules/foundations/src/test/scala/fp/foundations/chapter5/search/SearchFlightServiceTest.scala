package fp.foundations.chapter5.search

import fp.foundations.chapter5.DateGenerator.dateGen
import fp.foundations.chapter5.fp.IO
import fp.foundations.chapter5.fp.search.Airport.{ londonGatwick, parisOrly }
import fp.foundations.chapter5.fp.search.{ Flight, SearchFlightClient, SearchFlightService, SearchResult }
import fp.foundations.chapter5.search.SearchFlightGenerator.{ airportGen, clientGen }
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.time.{ Duration, Instant, LocalDate }
import scala.concurrent.ExecutionContext

// Run the test using the green arrow next to class name (if using IntelliJ)
// or run `sbt` in the terminal to open it in shell mode, then type:
// testOnly exercises.action.fp.search.SearchFlightServiceTest
class SearchFlightServiceTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("fromClients example") {
    val now   = Instant.now()
    val today = LocalDate.now()

    val flight1a = Flight("1", "BA", parisOrly, londonGatwick, now, Duration.ofMinutes(100), 0, 89.5, "")
    val flight1b = Flight("1", "BA", parisOrly, londonGatwick, now, Duration.ofMinutes(100), 0, 83.5, "")
    val flight2  = Flight("2", "LH", parisOrly, londonGatwick, now, Duration.ofMinutes(105), 0, 96.5, "")
    val flight3  = Flight("3", "BA", parisOrly, londonGatwick, now, Duration.ofMinutes(140), 1, 234.0, "")
    val flight4  = Flight("4", "LH", parisOrly, londonGatwick, now, Duration.ofMinutes(210), 2, 55.5, "")

    val client1 = SearchFlightClient.constant(IO(List(flight3, flight1a)))
    val client2 = SearchFlightClient.constant(IO(List(flight1b, flight2, flight4)))
    val client3 = SearchFlightClient.constant(IO.fail(new Exception("Boom")))

    val service = SearchFlightService.fromClients(List(client1, client2, client3))(ExecutionContext.global)
    val result  = service.search(parisOrly, londonGatwick, today).unsafeRun()

    assert(result == SearchResult(List(flight1b, flight2, flight3, flight4)))
  }

  test("search doesn't fail") {
    forAll(airportGen, airportGen, dateGen, Gen.listOf(clientGen)) { (from, to, date, clients) =>
      val service = SearchFlightService.fromClients(clients)(ExecutionContext.global)
      val result  = service.search(from, to, date).attempt.unsafeRun()

      assert(result.isSuccess)
    }
  }

  test("search use all clients") {
    forAll(airportGen, airportGen, dateGen, Gen.listOf(clientGen), Gen.listOf(clientGen)) {
      (from, to, date, clients1, clients2) =>
        val service1 = SearchFlightService.fromClients(clients1)(ExecutionContext.global)
        val service2 = SearchFlightService.fromClients(clients2)(ExecutionContext.global)
        val service3 = SearchFlightService.fromClients(clients1 ++ clients2)(ExecutionContext.global)

        val result1 = service1.search(from, to, date).unsafeRun()
        val result2 = service2.search(from, to, date).unsafeRun()
        val result3 = service3.search(from, to, date).unsafeRun()

        val combined = SearchResult(result1.flights ++ result2.flights)

        assert(result3 == combined)
    }
  }

  test("fromClients - all results must match the from, to and date requested") {
    forAll(Gen.listOf(clientGen), airportGen, airportGen, dateGen, MinSuccessful(100)) { (clients, from, to, date) =>
      val service = SearchFlightService.fromClients(clients)(ExecutionContext.global)
      val result  = service.search(from, to, date).unsafeRun()

      result.flights.foreach { flight =>
        assert(flight.from == from)
        assert(flight.to == to)
        assert(flight.departureDate == date)
      }
    }
  }
}
