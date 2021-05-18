package group.scala.karazin.circe.literal.extras

import cats.implicits._
import io.circe.syntax._
import io.circe.{Json, JsonNumber, JsonObject, parser}
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import io.circe.Encoder.encodeJsonObject
import java.time._
import scala.jdk.CollectionConverters._


trait ArbitrariesJavaDateTime {
  val instantGenerator: Gen[Instant] = Gen.choose(Instant.MIN, Instant.MAX)

  val periodGenerator: Gen[Period] =
    for {
      day <- Gen.posNum[Int]
    } yield Period.ofDays(day)

  val zoneIdGenerator: Gen[ZoneId] = {
    for {
      zone <- Gen.oneOf((ZoneId.getAvailableZoneIds).asScala)
    } yield ZoneId.of(zone)

  }

  val localDateGenerator: Gen[LocalDate] = Gen.choose(LocalDate.MIN, LocalDate.MAX)

  val localTimeGenerator: Gen[LocalTime] = Gen.choose(LocalTime.MIN, LocalTime.MAX)

  val localDateTimeGenerator: Gen[LocalDateTime] = Gen.choose(LocalDateTime.MIN, LocalDateTime.MAX)

  val monthDayGenerator: Gen[MonthDay] =
    for {
      month <- Gen.choose(0, 11)
      day   <- Gen.choose(1, LocalDate.of(2020, month, 1).getDayOfMonth)
    } yield MonthDay.of(month, day)

  val offsetTimeGenerator: Gen[OffsetTime] = Gen.choose(OffsetTime.MIN, OffsetTime.MAX)

  val offsetDateTimeGenerator: Gen[OffsetDateTime] = Gen.choose(OffsetDateTime.MIN, OffsetDateTime.MAX)

  val yearGenerator: Gen[Year] =
    for {
      year <- Gen.choose(Year.MIN_VALUE, Year.MAX_VALUE)
    } yield Year.of(year)

  val yearMonthGenerator: Gen[YearMonth] =
    for {
      year  <- Gen.choose(Year.MIN_VALUE, Year.MAX_VALUE)
      month <- Gen.choose(1, 12)
    } yield YearMonth.of(year, month)

  val zonedDateTimeGenerator: Gen[ZonedDateTime] =
    for {
      localDate <- Gen.choose(LocalDate.MIN, LocalDate.MAX)
      localTime <- Gen.choose(LocalTime.MIN, LocalTime.MAX)
      zone      <- Gen.oneOf((ZoneId.getAvailableZoneIds).asScala)
    } yield ZonedDateTime.of(localDate, localTime, ZoneId.of(zone))

  val zoneOffsetGenerator: Gen[ZoneOffset] = Gen.choose(ZoneOffset.MIN, ZoneOffset.MAX)

  given Arbitrary[Instant] = Arbitrary(instantGenerator)
  given Arbitrary[Period] = Arbitrary(periodGenerator)
  given Arbitrary[ZoneId] = Arbitrary(zoneIdGenerator)
  given Arbitrary[LocalDate] = Arbitrary(localDateGenerator)
  given Arbitrary[LocalTime] = Arbitrary(localTimeGenerator)
  given Arbitrary[LocalDateTime] = Arbitrary(localDateTimeGenerator)
  given Arbitrary[MonthDay] = Arbitrary(monthDayGenerator)
  given Arbitrary[OffsetTime] = Arbitrary(offsetTimeGenerator)
  given Arbitrary[OffsetDateTime] = Arbitrary(offsetDateTimeGenerator)
  given Arbitrary[Year] = Arbitrary(yearGenerator)
  given Arbitrary[YearMonth] = Arbitrary(yearMonthGenerator)
  given Arbitrary[ZonedDateTime] = Arbitrary(zonedDateTimeGenerator)
  given Arbitrary[ZoneOffset] = Arbitrary(zoneOffsetGenerator)

}


