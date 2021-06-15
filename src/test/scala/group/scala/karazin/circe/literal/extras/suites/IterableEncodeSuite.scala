package group.scala.karazin.circe.literal.extras.suites

import munit._
import org.scalacheck._
import org.scalacheck.Prop._
import io.circe.syntax._
import io.circe.parser
import io.circe.{Json, JsonObject, Codec}

import group.scala.karazin.circe.literal.extras.macros
import group.scala.karazin.circe.literal.extras.arbitraries.instances.{given, _}

class IterableEncodeSuite extends munit.ScalaCheckSuite:

  property("inlined Iterable of Int values") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Iterable[Int]]('sc, 'args)}

    forAll { (iterable: Iterable[Int]) =>

      val result: Json = encode"${iterable}"

      val expected: Json = iterable.asJson

      assertEquals(result, expected)

    }

  }

  property("inlined Iterable of Option Int values") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Iterable[Option[Int]]]('sc, 'args)}

    forAll { (iterable: Iterable[Option[Int]]) =>

      val result: Json = encode"$iterable"

      val expected: Json = iterable.asJson

      assertEquals(result, expected)

    }

  }

  property("inlined Iterable of primitives with Int values") {

    case class Primitive(value: Int) derives Codec.AsObject

    given Arbitrary[Primitive] = Arbitrary(Arbitrary.arbitrary[Int].map(Primitive(_)))

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Iterable[Primitive]]('sc, 'args)}

    forAll { (iterable: Iterable[Primitive]) =>

      val result: Json = encode"$iterable"

      val expected: Json = iterable.asJson

      assertEquals(result, expected)

    }

  }


  property("inlined Iterable of Option primitives") {

    case class Primitive(value: Int) derives Codec.AsObject

    given Arbitrary[Primitive] = Arbitrary(Arbitrary.arbitrary[Int].map(Primitive(_)))

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Iterable[Option[Primitive]]]('sc, 'args)}

    forAll { (iterable: Iterable[Option[Primitive]]) =>

      val result: Json = encode"$iterable"

      val expected: Json = iterable.asJson

      assertEquals(result, expected)

    }

  }

  property("inlined Iterable of JsonObject values") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Iterable[JsonObject]]('sc, 'args)}

    forAll { (iterable: Iterable[JsonObject]) =>

      val result: Json = encode"$iterable"

      val expected: Json = iterable.asJson

      assertEquals(result, expected)

    }

  }

  property("inlined Iterable of primitives with JsonObject values") {

    case class Primitive(value: JsonObject) derives Codec.AsObject

    given Arbitrary[Primitive] = Arbitrary(Arbitrary.arbitrary[JsonObject].map(Primitive(_)))

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Iterable[Primitive]]('sc, 'args)}

    forAll { (iterable: Iterable[Primitive]) =>

      val result: Json = encode"$iterable"

      val expected: Json = iterable.asJson

      assertEquals(result, expected)

    }

  }

  test("corrupted ints parsing compile error") {
    scala.compiletime.testing.typeCheckErrors(
      """
          extension (inline sc: StringContext)
            inline def encode(inline args: Any*): Json =
              ${ macros.encode[Iterable[Int]]('sc, 'args) }

          encode"[ -1, null ]"
        """
    ).headOption match
      case Some(error) => assert(error.message.startsWith("Encode error:"))
      case _           => fail("No compilation error was found.")
  }

  test("corrupted options parsing compile error") {
    scala.compiletime.testing.typeCheckErrors(
      """
          extension (inline sc: StringContext)
            inline def encode(inline args: Any*): Json =
              ${ macros.encode[Iterable[Option[Int]]]('sc, 'args) }
          encode"[ null, 42, { } ]"
      """
    ).headOption match
      case Some(error) => assert(error.message.startsWith("Encode error:"))
      case _           => fail("No compilation error was found.")
  }
