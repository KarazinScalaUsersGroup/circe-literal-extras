package group.scala.karazin.circe.literal.extras

import cats.implicits._
import io.circe.syntax._
import io.circe.parser
import io.circe.{Json, JsonObject, Encoder, Codec}
import org.scalacheck._
import org.scalacheck.Prop._
import group.scala.karazin.circe.literal.extras.arbitraries.instances.{given, _}
import java.util.Currency
import java.time._

class PlainPrimitiveEncodeSuite extends munit.ScalaCheckSuite:

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(1)

  property("inlined Unit value") {
    case class Primitive(value: Unit) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: Unit) =>

      val primitive = Primitive(value)

      lazy val result: Json =
        encode"""
              {
                "value": ${primitive.value}
              }
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": { }
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined primitive with Unit value") {
    case class Primitive(value: Unit) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: Unit) =>

      val primitive = Primitive(value)

      lazy val result: Json =
        encode"""
              $primitive
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": { }
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined Boolean value") {
    case class Primitive(value: Boolean) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: Boolean) =>

      val primitive = Primitive(value)

      lazy val result: Json =
        encode"""
              {
                "value": ${primitive.value}
              }
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": ${primitive.value}
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined primitive with Boolean value") {
    case class Primitive(value: Boolean) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: Boolean) =>

      val primitive = Primitive(value)

      lazy val result: Json =
        encode"""
              $primitive
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": ${primitive.value}
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined java.lang.Boolean value") {
    case class Primitive(value: java.lang.Boolean) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: Boolean) =>

      val primitive = Primitive(value)

      lazy val result: Json =
        encode"""
              {
                "value": ${primitive.value}
              }
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": ${primitive.value}
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined primitive with java.lang.Boolean value") {
    case class Primitive(value: java.lang.Boolean) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: Boolean) =>

      val primitive = Primitive(value)

      lazy val result: Json =
        encode"""
              $primitive
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": ${primitive.value}
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined Byte value") {
    case class Primitive(value: Byte) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: Byte) =>

      val primitive = Primitive(value)

      lazy val result: Json =
        encode"""
              {
                "value": ${primitive.value}
              }
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": ${primitive.value}
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined primitive with Byte value") {
    case class Primitive(value: Byte) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: Byte) =>

      val primitive = Primitive(value)

      lazy val result: Json =
        encode"""
              $primitive
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": ${primitive.value}
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined java.lang.Byte value") {
    case class Primitive(value: java.lang.Byte) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: Byte) =>

      val primitive = Primitive(value)

      lazy val result: Json =
        encode"""
              {
                "value": ${primitive.value}
              }
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": ${primitive.value}
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined primitive with java.lang.Byte value") {
    case class Primitive(value: java.lang.Byte) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: Byte) =>

      val primitive = Primitive(value)

      lazy val result: Json =
        encode"""
              $primitive
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": ${primitive.value}
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined Short value") {
    case class Primitive(value: Short) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: Short) =>

      val primitive = Primitive(value)

      lazy val result: Json =
        encode"""
              {
                "value": ${primitive.value}
              }
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": ${primitive.value}
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined primitive with Short value") {
    case class Primitive(value: Short) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: Short) =>

      val primitive = Primitive(value)

      lazy val result: Json =
        encode"""
              $primitive
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": ${primitive.value}
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined java.lang.Short value") {
    case class Primitive(value: java.lang.Short) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: Short) =>

      val primitive = Primitive(value)

      lazy val result: Json =
        encode"""
              {
                "value": ${primitive.value}
              }
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": ${primitive.value}
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined primitive with java.lang.Short value") {
    case class Primitive(value: java.lang.Short) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: Short) =>

      val primitive = Primitive(value)

      lazy val result: Json =
        encode"""
              $primitive
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": ${primitive.value}
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined Long value") {
    case class Primitive(value: Long) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: Long) =>

      val primitive = Primitive(value)

      lazy val result: Json =
        encode"""
              {
                "value": ${primitive.value}
              }
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": ${primitive.value}
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined primitive with Long value") {
    case class Primitive(value: Long) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: Long) =>

      val primitive = Primitive(value)

      lazy val result: Json =
        encode"""
              $primitive
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": ${primitive.value}
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined java.lang.Long value") {
    case class Primitive(value: java.lang.Long) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: Long) =>

      val primitive = Primitive(value)

      lazy val result: Json =
        encode"""
              {
                "value": ${primitive.value}
              }
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": ${primitive.value}
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined primitive with java.lang.Long value") {
    case class Primitive(value: java.lang.Long) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: Long) =>

      val primitive = Primitive(value)

      lazy val result: Json =
        encode"""
              $primitive
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": ${primitive.value}
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined Float value") {
    case class Primitive(value: Float) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: Float) =>

      val primitive = Primitive(value)

      lazy val result: Json =
        encode"""
              {
                "value": ${primitive.value}
              }
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": ${primitive.value}
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined primitive with Float value") {
    case class Primitive(value: Float) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: Float) =>

      val primitive = Primitive(value)

      lazy val result: Json =
        encode"""
              $primitive
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": ${primitive.value}
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined java.lang.Float value") {
    case class Primitive(value: java.lang.Float) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: Float) =>

      val primitive = Primitive(value)

      lazy val result: Json =
        encode"""
              {
                "value": ${primitive.value}
              }
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": ${primitive.value}
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined primitive with java.lang.Float value") {
    case class Primitive(value: java.lang.Float) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: Float) =>

      val primitive = Primitive(value)

      lazy val result: Json =
        encode"""
              $primitive
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": ${primitive.value}
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined Double value") {
    case class Primitive(value: Double) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: Double) =>

      val primitive = Primitive(value)

      lazy val result: Json =
        encode"""
              {
                "value": ${primitive.value}
              }
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": ${primitive.value}
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined primitive with Double value") {
    case class Primitive(value: Double) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: Double) =>

      val primitive = Primitive(value)

      lazy val result: Json =
        encode"""
              $primitive
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": ${primitive.value}
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined java.lang.Double value") {
    case class Primitive(value: java.lang.Double) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: Double) =>

      val primitive = Primitive(value)

      lazy val result: Json =
        encode"""
              {
                "value": ${primitive.value}
              }
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": ${primitive.value}
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined primitive with java.lang.Double value") {
    case class Primitive(value: java.lang.Double) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: Double) =>

      val primitive = Primitive(value)

      lazy val result: Json =
        encode"""
              $primitive
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": ${primitive.value}
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined Char value") {
    case class Primitive(value: Char) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: Char) =>

      val primitive = Primitive(value)

      lazy val result: Json =
        encode"""
              {
                "value": ${primitive.value}
              }
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": "${primitive.value}"
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined primitive with Char value") {
    case class Primitive(value: Char) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: Char) =>

      val primitive = Primitive(value)

      lazy val result: Json =
        encode"""
              $primitive
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": "${primitive.value}"
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined String value") {
    case class Primitive(value: String) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: String) =>

      val primitive = Primitive(value)

      lazy val result: Json =
        encode"""
              {
                "value": ${primitive.value}
              }
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": "${primitive.value}"
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined primitive with String value") {
    case class Primitive(value: String) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: String) =>

      val primitive = Primitive(value)

      lazy val result: Json =
        encode"""
              $primitive
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": "${primitive.value}"
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined BigInt value") {
    case class Primitive(value: BigInt) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: BigInt) =>

      val primitive = Primitive(value)

      lazy val result: Json =
        encode"""
              {
                "value": ${primitive.value}
              }
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": ${primitive.value}
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined primitive with BigInt value") {
    case class Primitive(value: BigInt) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: BigInt) =>

      val primitive = Primitive(value)

      lazy val result: Json =
        encode"""
              $primitive
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": ${primitive.value}
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined java.math.BigInteger value") {
    case class Primitive(value: java.math.BigInteger) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: BigInt) =>

      val primitive = Primitive(value.bigInteger)

      lazy val result: Json =
        encode"""
              {
                "value": ${primitive.value}
              }
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": ${primitive.value}
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined primitive with java.math.BigInteger value") {
    case class Primitive(value: java.math.BigInteger) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: BigInt) =>

      val primitive = Primitive(value.bigInteger)

      lazy val result: Json =
        encode"""
              $primitive
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": ${primitive.value}
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined BigDecimal value") {
    case class Primitive(value: BigDecimal) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: BigDecimal) =>

      val primitive = Primitive(value)

      lazy val result: Json =
        encode"""
              {
                "value": ${primitive.value}
              }
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": ${primitive.value}
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined primitive with BigDecimal value") {
    case class Primitive(value: BigDecimal) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: BigDecimal) =>

      val primitive = Primitive(value)

      lazy val result: Json =
        encode"""
              $primitive
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": ${primitive.value}
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined java.math.BigDecimal value") {
    case class Primitive(value: java.math.BigDecimal) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: BigDecimal) =>

      val primitive = Primitive(value.bigDecimal)

      lazy val result: Json =
        encode"""
              {
                "value": ${primitive.value}
              }
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": ${primitive.value}
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined primitive with java.math.BigDecimal value") {
    case class Primitive(value: java.math.BigDecimal) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: BigDecimal) =>

      val primitive = Primitive(value.bigDecimal)

      lazy val result: Json =
        encode"""
              $primitive
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": ${primitive.value}
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined java.util.UUID value") {
    case class Primitive(value: java.util.UUID) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: java.util.UUID) =>

      val primitive = Primitive(value)

      lazy val result: Json =
        encode"""
              {
                "value": ${primitive.value}
              }
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": "${primitive.value}"
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined primitive with java.util.UUID value") {
    case class Primitive(value: java.util.UUID) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    forAll { (value: java.util.UUID) =>

      val primitive = Primitive(value)

      lazy val result: Json =
        encode"""
              $primitive
            """

      val expected: Json =
        parser.parse(
          s"""
          {
            "value": "${primitive.value}"
          }
         """
        ).toOption.get

      assertEquals(result, expected)
    }

  }

  property("inlined JsonObject value") {

    case class Primitive(value: JsonObject) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        $ {macros.encode[Primitive]('sc, 'args)}

    forAll { (jsonObject: JsonObject) =>

      val primitive = Primitive(jsonObject)

      val result: Json = JsonObject("value" -> jsonObject.asJson).asJson

      val expected: Json =
        encode"""
        {
          "value": ${primitive.value}
        }"""

      assertEquals(result, expected)
    }
  }

  property("inlined primitive with JsonObject value") {

    case class Primitive(value: JsonObject) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        $ {macros.encode[Primitive]('sc, 'args)}

    forAll { (jsonObject: JsonObject) =>

      val primitive = Primitive(jsonObject)

      val result: Json = JsonObject("value" -> jsonObject.asJson).asJson

      val expected: Json = encode"$primitive"

      assertEquals(result, expected)
    }
  }

  property("inlined Currency value") {

    case class Primitive(value: Currency) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        $ {macros.encode[Primitive]('sc, 'args)}

    forAll { (currency: Currency) =>

      val primitive = Primitive(currency)

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

  property("inlined primitive with Currency value") {

    case class Primitive(value: Currency) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        $ {macros.encode[Primitive]('sc, 'args)}

    forAll { (currency: Currency) =>

      val primitive = Primitive(currency)

      val result: Json = JsonObject("value" -> currency.asJson).asJson

      val expected: Json = encode"$primitive"

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

