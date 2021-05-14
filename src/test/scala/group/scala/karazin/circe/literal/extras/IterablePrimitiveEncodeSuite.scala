package group.scala.karazin.circe.literal.extras

import munit._
import org.scalacheck._
import org.scalacheck.Prop._
import io.circe.syntax._
import io.circe.{Json, JsonObject, Encoder, Codec}
import group.scala.karazin.circe.literal.extras.arbitraries.instances.{given, _}

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

  property("inlined Iterable of Double values") {

    case class Primitive(value: Double) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Iterable[Double]]('sc, 'args)}

    forAll { (doubleIterable: Iterable[Double]) =>

      val jsonDoubleArray: Iterable[Json] = doubleIterable.map(Json.fromDoubleOrString)

      val result: Json = encode"${doubleIterable}"

      val expected: Json = Json.fromValues(jsonDoubleArray)

      assertEquals(result, expected)

    }

  }

  property("inlined Iterable of primitives with Double values") {

    case class Primitive(value: Double) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Iterable[Primitive]]('sc, 'args)}

    forAll { (doubleIterable: Iterable[Double]) =>

      val iterablePrimitives: Iterable[Primitive] = doubleIterable.map(Primitive(_))

      val jsonArrayPrimitives: Iterable[Json] =
        doubleIterable.map(value => JsonObject("value" -> Json.fromDoubleOrString(value)).asJson)

      val result: Json = encode"${iterablePrimitives}"

      val expected: Json = Json.fromValues(jsonArrayPrimitives)

      assertEquals(result, expected)

    }

  }

  property("inlined Iterable of JsonObject values") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Iterable[JsonObject]]('sc, 'args)}

    forAll { (jsonObjectIterable: Iterable[JsonObject]) =>

      val jsonObjectsArray: Iterable[Json] = jsonObjectIterable.map(Json.fromJsonObject)

      val result: Json = encode"${jsonObjectIterable}"

      val expected: Json = Json.fromValues(jsonObjectsArray)

      assertEquals(result, expected)

    }

  }

  property("inlined Iterable of primitives with JsonObject values") {

    case class Primitive(value: JsonObject) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Iterable[Primitive]]('sc, 'args)}

    forAll { (jsonObjectIterable: Iterable[JsonObject]) =>

      val primitives: Iterable[Primitive] = jsonObjectIterable.map(Primitive(_))

      val jsonObjectsArray: Iterable[Json] =
        jsonObjectIterable.map(js => JsonObject("value" -> js.asJson).asJson)

      val result: Json = encode"$primitives"

      val expected: Json = Json.fromValues(jsonObjectsArray)

      assertEquals(result, expected)

    }

  }
