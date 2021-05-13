package group.scala.karazin.circe.literal.extras

import cats.implicits._
import io.circe.syntax._
import io.circe.parser
import io.circe.{Json, JsonObject, Encoder, Codec}
import org.scalacheck._
import org.scalacheck.Prop._
import group.scala.karazin.circe.literal.extras.arbitraries.instances.{given, _}

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
