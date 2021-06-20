package fp.foundations.chapter5.fp.search

import java.time.{ Duration, Instant, LocalDate }

case class Flight(
  flightId: String, // unique id per flight
  airline: String,
  from: Airport,
  to: Airport,
  departureAt: Instant,
  duration: Duration,
  numberOfStops: Int, // 0 for a direct flight
  unitPrice: Double,  // in dollars
  redirectLink: String
) {
  def departureDate: LocalDate =
    departureAt.atZone(from.timeZone).toLocalDate
}
