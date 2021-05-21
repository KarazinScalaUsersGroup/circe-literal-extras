package group.scala.karazin.circe.literal.extras

import cats.implicits._
import io.circe.syntax._
import io.circe.parser
import io.circe.{Json, JsonObject, Encoder, Codec}
import org.scalacheck._
import org.scalacheck.Prop._
import group.scala.karazin.circe.literal.extras.arbitraries.instances.{given, _}

import java.time._

class JavaTimeEncodeSuite extends munit.ScalaCheckSuite:

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(1)

  property("inlined Year value") {

    case class Primitive(value: Year) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        $ {macros.encode[Primitive]('sc, 'args)}

    forAll { (year: Year) =>

      val primitive = Primitive(year)

      val result: Json = JsonObject("value" -> primitive.value.asJson).asJson

      val expected: Json =
        encode"""
          {
            "value": ${primitive.value}
          }
         """

      assertEquals(result, expected)
    }

  }

  property("inlined primitive with Year value") {

    case class Primitive(value: Year) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        $ {macros.encode[Primitive]('sc, 'args)}

    forAll { (year: Year) =>

      val primitive = Primitive(year)

      val result: Json = JsonObject("value" -> year.asJson).asJson

      val expected: Json = encode"$primitive"

      assertEquals(result, expected)
    }

  }

  property("inlined ZoneOffset value") {

    case class Primitive(value: ZoneOffset) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        $ {macros.encode[Primitive]('sc, 'args)}

    forAll { (zoneOffset: ZoneOffset) =>

      val primitive = Primitive(zoneOffset)

      val result: Json = JsonObject("value" -> primitive.value.asJson).asJson

      val expected: Json =
        encode"""
          {
            "value": ${primitive.value}
          }
         """

      assertEquals(result, expected)
    }

  }

  property("inlined primitive with ZoneOffset value") {

    case class Primitive(value: ZoneOffset) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        $ {macros.encode[Primitive]('sc, 'args)}

    forAll { (zoneOffset: ZoneOffset) =>

      val primitive = Primitive(zoneOffset)

      val result: Json = JsonObject("value" -> zoneOffset.asJson).asJson

      val expected: Json = encode"$primitive"

      assertEquals(result, expected)
    }

  }

  property("inlined ZonedDateTime value") {

    case class Primitive(value: ZonedDateTime) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        $ {macros.encode[Primitive]('sc, 'args)}

    forAll { (zonedDateTime: ZonedDateTime) =>

      val primitive = Primitive(zonedDateTime)

      val result: Json = JsonObject("value" -> primitive.value.asJson).asJson

      val expected: Json =
        encode"""
          {
            "value": ${primitive.value}
          }
         """

      assertEquals(result, expected)
    }

  }

  property("inlined primitive with ZonedDateTime value") {

    case class Primitive(value: ZonedDateTime) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        $ {macros.encode[Primitive]('sc, 'args)}

    forAll { (zonedDateTime: ZonedDateTime) =>

      val primitive = Primitive(zonedDateTime)

      val result: Json = JsonObject("value" -> zonedDateTime.asJson).asJson

      val expected: Json = encode"$primitive"

      assertEquals(result, expected)
    }

  }

  property("inlined YearMonth value") {

    case class Primitive(value: YearMonth) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        $ {macros.encode[Primitive]('sc, 'args)}

    forAll { (yearMonth: YearMonth) =>

      val primitive = Primitive(yearMonth)

      val result: Json = JsonObject("value" -> primitive.value.asJson).asJson

      val expected: Json =
        encode"""
          {
            "value": ${primitive.value}
          }
         """

      assertEquals(result, expected)
    }

  }

  property("inlined primitive with YearMonth value") {

    case class Primitive(value: YearMonth) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        $ {macros.encode[Primitive]('sc, 'args)}

    forAll { (yearMonth: YearMonth) =>

      val primitive = Primitive(yearMonth)

      val result: Json = JsonObject("value" -> yearMonth.asJson).asJson

      val expected: Json = encode"$primitive"

      assertEquals(result, expected)
    }

  }

  property("inlined OffsetDateTime value") {

    case class Primitive(value: OffsetDateTime) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        $ {macros.encode[Primitive]('sc, 'args)}

    forAll { (offsetDateTime: OffsetDateTime) =>

      val primitive = Primitive(offsetDateTime)

      val result: Json = JsonObject("value" -> primitive.value.asJson).asJson

      val expected: Json =
        encode"""
          {
            "value": ${primitive.value}
          }
         """

      assertEquals(result, expected)
    }

  }

  property("inlined primitive with OffsetDateTime value") {

    case class Primitive(value: OffsetDateTime) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        $ {macros.encode[Primitive]('sc, 'args)}

    forAll { (offsetDateTime: OffsetDateTime) =>

      val primitive = Primitive(offsetDateTime)

      val result: Json = JsonObject("value" -> offsetDateTime.asJson).asJson

      val expected: Json = encode"$primitive"

      assertEquals(result, expected)
    }

  }

  property("inlined OffsetTime value") {

    case class Primitive(value: OffsetTime) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        $ {macros.encode[Primitive]('sc, 'args)}

    forAll { (offsetTime: OffsetTime) =>

      val primitive = Primitive(offsetTime)

      val result: Json = JsonObject("value" -> primitive.value.asJson).asJson

      val expected: Json =
        encode"""
          {
            "value": ${primitive.value}
          }
         """

      assertEquals(result, expected)
    }

  }

  property("inlined primitive with OffsetTime value") {

    case class Primitive(value: OffsetTime) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        $ {macros.encode[Primitive]('sc, 'args)}

    forAll { (offsetTime: OffsetTime) =>

      val primitive = Primitive(offsetTime)

      val result: Json = JsonObject("value" -> offsetTime.asJson).asJson

      val expected: Json = encode"$primitive"

      assertEquals(result, expected)
    }

  }

  property("inlined MonthDay value") {

    case class Primitive(value: MonthDay) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        $ {macros.encode[Primitive]('sc, 'args)}

    forAll { (monthDay: MonthDay) =>

      val primitive = Primitive(monthDay)

      val result: Json = JsonObject("value" -> primitive.value.asJson).asJson

      val expected: Json =
        encode"""
          {
            "value": ${primitive.value}
          }
         """

      assertEquals(result, expected)
    }

  }

  property("inlined primitive with MonthDay value") {

    case class Primitive(value: MonthDay) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        $ {macros.encode[Primitive]('sc, 'args)}

    forAll { (monthDay: MonthDay) =>

      val primitive = Primitive(monthDay)

      val result: Json = JsonObject("value" -> monthDay.asJson).asJson

      val expected: Json = encode"$primitive"

      assertEquals(result, expected)
    }

  }

  property("inlined LocalDateTime value") {

    case class Primitive(value: LocalDateTime) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        $ {macros.encode[Primitive]('sc, 'args)}

    forAll { (localDateTime: LocalDateTime) =>

      val primitive = Primitive(localDateTime)

      val result: Json = JsonObject("value" -> primitive.value.asJson).asJson

      val expected: Json =
        encode"""
          {
            "value": ${primitive.value}
          }
         """

      assertEquals(result, expected)
    }

  }

  property("inlined primitive with LocalDateTime value") {

    case class Primitive(value: LocalDateTime) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        $ {macros.encode[Primitive]('sc, 'args)}

    forAll { (localDateTime: LocalDateTime) =>

      val primitive = Primitive(localDateTime)

      val result: Json = JsonObject("value" -> localDateTime.asJson).asJson

      val expected: Json = encode"$primitive"

      assertEquals(result, expected)
    }

  }

  property("inlined LocalTime value") {

    case class Primitive(value: LocalTime) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        $ {macros.encode[Primitive]('sc, 'args)}

    forAll { (localTime: LocalTime) =>

      val primitive = Primitive(localTime)

      val result: Json = JsonObject("value" -> primitive.value.asJson).asJson

      val expected: Json =
        encode"""
          {
            "value": ${primitive.value}
          }
         """

      assertEquals(result, expected)
    }

  }

  property("inlined primitive with LocalTime value") {

    case class Primitive(value: LocalTime) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        $ {macros.encode[Primitive]('sc, 'args)}

    forAll { (localTime: LocalTime) =>

      val primitive = Primitive(localTime)

      val result: Json = JsonObject("value" -> localTime.asJson).asJson

      val expected: Json = encode"$primitive"

      assertEquals(result, expected)
    }

  }

  property("inlined LocalDate value") {

    case class Primitive(value: LocalDate) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        $ {macros.encode[Primitive]('sc, 'args)}

    forAll { (localDate: LocalDate) =>

      val primitive = Primitive(localDate)

      val result: Json = JsonObject("value" -> primitive.value.asJson).asJson

      val expected: Json =
        encode"""
          {
            "value": ${primitive.value}
          }
         """

      assertEquals(result, expected)
    }

  }

  property("inlined primitive with LocalDate value") {

    case class Primitive(value: LocalDate) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        $ {macros.encode[Primitive]('sc, 'args)}

    forAll { (localDate: LocalDate) =>

      val primitive = Primitive(localDate)

      val result: Json = JsonObject("value" -> localDate.asJson).asJson

      val expected: Json = encode"$primitive"

      assertEquals(result, expected)
    }

  }

  property("inlined ZoneId value") {

    case class Primitive(value: ZoneId) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        $ {macros.encode[Primitive]('sc, 'args)}

    forAll { (zoneId: ZoneId) =>

      val primitive = Primitive(zoneId)

      val result: Json = JsonObject("value" -> primitive.value.asJson).asJson

      val expected: Json =
        encode"""
          {
            "value": ${primitive.value}
          }
         """

      assertEquals(result, expected)
    }

  }

  property("inlined primitive with ZoneId value") {

    case class Primitive(value: ZoneId) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        $ {macros.encode[Primitive]('sc, 'args)}

    forAll { (zoneId: ZoneId) =>

      val primitive = Primitive(zoneId)

      val result: Json = JsonObject("value" -> zoneId.asJson).asJson

      val expected: Json = encode"$primitive"

      assertEquals(result, expected)
    }

  }

  property("inlined Period value") {

    case class Primitive(value: Period) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        $ {macros.encode[Primitive]('sc, 'args)}

    forAll { (period: Period) =>

      val primitive = Primitive(period)

      val result: Json = JsonObject("value" -> primitive.value.asJson).asJson

      val expected: Json =
        encode"""
          {
            "value": ${primitive.value}
          }
         """

      assertEquals(result, expected)
    }

  }

  property("inlined primitive with Period value") {

    case class Primitive(value: Period) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        $ {macros.encode[Primitive]('sc, 'args)}

    forAll { (period: Period) =>

      val primitive = Primitive(period)

      val result: Json = JsonObject("value" -> period.asJson).asJson

      val expected: Json = encode"$primitive"

      assertEquals(result, expected)
    }

  }

  property("inlined Instant value") {

    case class Primitive(value: Instant) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        $ {macros.encode[Primitive]('sc, 'args)}

    forAll { (instant: Instant) =>

      val primitive = Primitive(instant)

      val result: Json = JsonObject("value" -> primitive.value.asJson).asJson

      val expected: Json =
        encode"""
          {
            "value": ${primitive.value}
          }
         """

      assertEquals(result, expected)
    }

  }

  property("inlined primitive with Instant value") {

    case class Primitive(value: Instant) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        $ {macros.encode[Primitive]('sc, 'args)}

    forAll { (instant: Instant) =>

      val primitive = Primitive(instant)

      val result: Json = JsonObject("value" -> instant.asJson).asJson

      val expected: Json = encode"$primitive"

      assertEquals(result, expected)
    }

  }

