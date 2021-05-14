package group.scala.karazin.circe.literal.extras

import munit._
import org.scalacheck._
import org.scalacheck.Prop._
import io.circe.syntax._
import io.circe.{Json, JsonObject, Encoder, Codec}

class IterablePrimitiveEncodeSuite extends munit.ScalaCheckSuite:

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(1)

  property("inlined Iterable of Int values") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Iterable[Int]]('sc, 'args)}

    forAll { (intIterable: Iterable[Int]) =>

      val jsonIntArray: Iterable[Json] = intIterable.map(Json.fromInt)

      val result: Json = encode"${intIterable}"

      val expected: Json = Json.fromValues(jsonIntArray)

      assertEquals(result, expected)

    }

  }

  property("inlined Iterable of primitives with Int values") {

    case class Primitive(value: Int) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Iterable[Primitive]]('sc, 'args)}

    forAll { (intIterable: Iterable[Int]) =>

      val iterablePrimitives: Iterable[Primitive] = intIterable.map(Primitive(_))

      val jsonArrayPrimitives: Iterable[Json] =
        intIterable.map(value => JsonObject("value" -> Json.fromInt(value)).asJson)

      val result: Json = encode"${iterablePrimitives}"

      val expected: Json = Json.fromValues(jsonArrayPrimitives)

      assertEquals(result, expected)

    }

  }
