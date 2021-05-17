package group.scala.karazin.circe.literal.extras

import munit._
import org.scalacheck._
import org.scalacheck.Prop._
import io.circe.syntax._
import io.circe.parser
import io.circe.{Json, JsonObject, Codec}
import group.scala.karazin.circe.literal.extras.arbitraries.instances.{given, _}

class IterableEncodeSuite extends munit.ScalaCheckSuite:

  property("inlined Iterable of Int values") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Iterable[Int]]('sc, 'args)}

    forAll { (intIterable: Iterable[Int]) =>

      val result: Json = encode"${intIterable}"

      val expected: Json = parser.parse(
        s"""
          [${intIterable.mkString(",")}]
        """
      ).toOption.get

      assertEquals(result, expected)

    }

  }

  property("inlined Iterable of Option Int values") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Iterable[Option[Int]]]('sc, 'args)}

    forAll { (intIterable: Iterable[Option[Int]]) =>

      val result: Json = encode"${intIterable}"

      val expected: Json = parser.parse(
        s"""
          [${intIterable.map(_.fold(Json.Null)(Json.fromInt(_))).mkString(",")}]
        """
      ).toOption.get

      assertEquals(result, expected)

    }

  }

  property("inlined Iterable of primitives with Int values") {

    case class Primitive(value: Int) derives Codec.AsObject

    given Arbitrary[Primitive] = Arbitrary(Arbitrary.arbitrary[Int].map(Primitive(_)))

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Iterable[Primitive]]('sc, 'args)}

    forAll { (iterablePrimitives: Iterable[Primitive]) =>

      val jsonArrayPrimitives: Iterable[Json] = iterablePrimitives.map(_.asJson)

      val result: Json = encode"${iterablePrimitives}"

      val expected: Json = Json.fromValues(jsonArrayPrimitives)

      assertEquals(result, expected)

    }

  }


  property("inlined Iterable of Option primitives") {

    case class Primitive(value: Int) derives Codec.AsObject

    given Arbitrary[Primitive] = Arbitrary(Arbitrary.arbitrary[Int].map(Primitive(_)))

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Iterable[Option[Primitive]]]('sc, 'args)}

    forAll { (optionIterable: Iterable[Option[Primitive]]) =>

      val jsonObjectsArray: Iterable[Json] = optionIterable.map(_.asJson)

      val result: Json = encode"$optionIterable"

      val expected: Json = Json.fromValues(jsonObjectsArray)

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

    given Arbitrary[Primitive] = Arbitrary(Arbitrary.arbitrary[JsonObject].map(Primitive(_)))

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Iterable[Primitive]]('sc, 'args)}

    forAll { (jsonObjectIterable: Iterable[Primitive]) =>

      val result: Json = encode"$jsonObjectIterable"

      val expected: Json = jsonObjectIterable.asJson

      assertEquals(result, expected)

    }

  }
