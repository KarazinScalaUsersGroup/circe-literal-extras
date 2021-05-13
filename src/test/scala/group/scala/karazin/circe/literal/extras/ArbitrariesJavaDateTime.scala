package group.scala.karazin.circe.literal.extras

import cats.implicits._
import io.circe.syntax._
import io.circe.{Json, JsonNumber, JsonObject, parser}
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import io.circe.Encoder.encodeJsonObject
import java.time._


trait ArbitrariesJavaDateTime {
  val instantGenerator: Gen[Instant] = Gen.choose(Instant.MIN, Instant.MAX)

  val periodGenerator: Gen[Period] =
    for {
      d <- Gen.posNum[Int]
    } yield Period.ofDays(d)

  val zoneIdGenerator: Gen[ZoneId] =
    for {
      s1 <- Gen.oneOf("GMT", "UTC", "UT", "");
      s2 <- Gen.choose(0,23)
    } yield ZoneId.of(s1+s2.toString)

  val localDateGenerator: Gen[LocalDate] = Gen.choose(LocalDate.MIN, LocalDate.MAX)

  val localTimeGenerator: Gen[LocalTime] = Gen.choose(LocalTime.MIN, LocalTime.MAX)

  val localDateTimeGenerator: Gen[LocalDateTime] = Gen.choose(LocalDateTime.MIN, LocalDateTime.MAX)

  val monthDayGenerator: Gen[MonthDay] =
    for {
      m <- Gen.choose(0,11)
      d <- Gen.choose(1, LocalDate.of(2020,m,1).getDayOfMonth)
    } yield MonthDay.of(m, d)

  val offsetTimeGenerator: Gen[OffsetTime] = Gen.choose(OffsetTime.MIN, OffsetTime.MAX)

  val offsetDateTimeGenerator: Gen[OffsetDateTime] = Gen.choose(OffsetDateTime.MIN, OffsetDateTime.MAX)

  given Arbitrary[Instant] = Arbitrary(instantGenerator)
  given Arbitrary[Period] = Arbitrary(periodGenerator)
  given Arbitrary[ZoneId] = Arbitrary(zoneIdGenerator)
  given Arbitrary[LocalDate] = Arbitrary(localDateGenerator)
  given Arbitrary[LocalTime] = Arbitrary(localTimeGenerator)
  given Arbitrary[LocalDateTime] = Arbitrary(localDateTimeGenerator)
  given Arbitrary[MonthDay] = Arbitrary(monthDayGenerator)
  given Arbitrary[OffsetTime] = Arbitrary(offsetTimeGenerator)
  given Arbitrary[OffsetDateTime] = Arbitrary(offsetDateTimeGenerator)


}


