package group.scala.karazin.circe.literal.extras

import munit._
import org.scalacheck._
import org.scalacheck.Prop._
import io.circe.syntax._
import io.circe.parser
import io.circe.{Json, JsonObject, Codec}
import group.scala.karazin.circe.literal.extras.arbitraries.instances.{given, _}

/** !!! by default, scalacheck generates empty container in `forAll` !!! */
class IterableEncodeSuite extends munit.ScalaCheckSuite:

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(1)

  def genIterableOf[T: Arbitrary] =
    Gen.nonEmptyContainerOf[Iterable, T](Arbitrary.arbitrary[T])

  property("inlined empty Iterable of Int values") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Iterable[Int]]('sc, 'args)}

    forAll { (intIterable: Iterable[Int]) =>

      val result: Json = encode"${intIterable}"

      val expected: Json = Json.arr()

      assertEquals(result, expected)

    }

  }

  property("inlined non-empty Iterable of Int values") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Iterable[Int]]('sc, 'args)}

    forAll(genIterableOf[Int]) { (intIterable: Iterable[Int]) =>

      val result: Json = encode"${intIterable}"

      val expected: Json = parser.parse(
        s"""
          [${intIterable.mkString(",")}]
        """
      ).toOption.get

      assertEquals(result, expected)

    }

  }

  property("inlined non-empty Iterable of Option Int values") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Iterable[Option[Int]]]('sc, 'args)}

    forAll(genIterableOf[Option[Int]]) { (intIterable: Iterable[Option[Int]]) =>

      val result: Json = encode"${intIterable}"

      val expected: Json = parser.parse(
        s"""
          [${intIterable.map(_.fold(Json.Null)(Json.fromInt(_))).mkString(",")}]
        """).toOption.get

      assertEquals(result, expected)

    }

  }

  property("inlined non-empty Iterable with None") {

    val genIterableWithNone =
      genIterableOf[Option[Int]].retryUntil(_.exists(_.isEmpty))

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Iterable[Option[Int]]]('sc, 'args)}

    forAll(genIterableWithNone) { (intIterable: Iterable[Option[Int]]) =>

      val result: Json = encode"${intIterable}"

      val expected: Json = Json.arr(Json.Null)

      assertEquals(result, expected)

    }

  }

  property("inlined non-empty Iterable of primitives with Int values") {

    case class Primitive(value: Int) derives Codec.AsObject

    given Arbitrary[Primitive] = Arbitrary(Arbitrary.arbitrary[Int].map(Primitive(_)))

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Iterable[Primitive]]('sc, 'args)}

    forAll(genIterableOf[Primitive]) { (iterablePrimitives: Iterable[Primitive]) =>

      val jsonArrayPrimitives: Iterable[Json] = iterablePrimitives.map(_.asJson)

      val result: Json = encode"${iterablePrimitives}"

      val expected: Json = Json.fromValues(jsonArrayPrimitives)

      assertEquals(result, expected)

    }

  }


  property("inlined non-empty Iterable of Option primitives") {

    case class Primitive(value: Int) derives Codec.AsObject

    given Arbitrary[Primitive] = Arbitrary(Arbitrary.arbitrary[Int].map(Primitive(_)))

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Iterable[Option[Primitive]]]('sc, 'args)}

    forAll(genIterableOf[Option[Primitive]]) { (optionIterable: Iterable[Option[Primitive]]) =>

      val jsonObjectsArray: Iterable[Json] = optionIterable.map(_.asJson)

      val result: Json = encode"$optionIterable"

      val expected: Json = Json.fromValues(jsonObjectsArray)

      assertEquals(result, expected)

    }

  }

  property("inlined non-empty Iterable of JsonObject values") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Iterable[JsonObject]]('sc, 'args)}

    forAll(genIterableOf[JsonObject]) { (jsonObjectIterable: Iterable[JsonObject]) =>

      val jsonObjectsArray: Iterable[Json] = jsonObjectIterable.map(Json.fromJsonObject)

      val result: Json = encode"${jsonObjectIterable}"

      val expected: Json = Json.fromValues(jsonObjectsArray)

      assertEquals(result, expected)

    }

  }

  property("inlined non-empty Iterable of primitives with JsonObject values") {

    case class Primitive(value: JsonObject) derives Codec.AsObject

    given Arbitrary[Primitive] = Arbitrary(Arbitrary.arbitrary[JsonObject].map(Primitive(_)))

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Iterable[JsonObject]]('sc, 'args)}

    forAll(genIterableOf[JsonObject]) { (jsonObjectIterable: Iterable[JsonObject]) =>

      val result: Json = encode"$jsonObjectIterable"

      val expected: Json = jsonObjectIterable.asJson

      assertEquals(result, expected)

    }

  }
