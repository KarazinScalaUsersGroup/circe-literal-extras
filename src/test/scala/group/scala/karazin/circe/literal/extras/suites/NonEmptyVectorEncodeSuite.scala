package group.scala.karazin.circe.literal.extras.suites

import cats.implicits._
import cats.data.NonEmptyVector
import cats.laws.discipline.arbitrary._
import io.circe._
import io.circe.syntax._
import org.scalacheck.Prop._
import org.scalacheck._

import group.scala.karazin.circe.literal.extras.macros
import group.scala.karazin.circe.literal.extras.arbitraries.instances.{given, _}

class NonEmptyVectorEncodeSuite extends munit.ScalaCheckSuite:

  property("inlined NonEmptyVector with Int values") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[NonEmptyVector[Int]]('sc, 'args) }

    forAll { (vector: NonEmptyVector[Int]) =>

      val result = encode"$vector"

      val expected = vector.asJson

      assertEquals(result, expected)

    }
  }

  property("inlined NonEmptyVector of Option Int values") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[NonEmptyVector[Option[Int]]]('sc, 'args)}

    forAll { (vector: NonEmptyVector[Option[Int]]) =>

      val result: Json = encode"$vector"

      val expected: Json = vector.asJson

      assertEquals(result, expected)

    }

  }

  property("inlined NonEmptyVector of primitives with Int values") {

    case class Primitive(value: Int) derives Codec.AsObject

    given Arbitrary[Primitive] = Arbitrary(Arbitrary.arbitrary[Int].map(Primitive(_)))

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[NonEmptyVector[Primitive]]('sc, 'args)}

    forAll { (vector: NonEmptyVector[Primitive]) =>

      val result: Json = encode"$vector"

      val expected: Json = vector.asJson

      assertEquals(result, expected)

    }

  }


  property("inlined NonEmptyVector of Option primitives") {

    case class Primitive(value: Int) derives Codec.AsObject

    given Arbitrary[Primitive] = Arbitrary(Arbitrary.arbitrary[Int].map(Primitive(_)))

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[NonEmptyVector[Option[Primitive]]]('sc, 'args)}

    forAll { (vector: NonEmptyVector[Option[Primitive]]) =>

      val result: Json = encode"$vector"

      val expected: Json = vector.asJson

      assertEquals(result, expected)

    }

  }

  property("inlined NonEmptyVector of JsonObject values") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[NonEmptyVector[JsonObject]]('sc, 'args)}

    forAll { (vector: NonEmptyVector[JsonObject]) =>

      val result: Json = encode"$vector"

      val expected: Json = vector.asJson

      assertEquals(result, expected)

    }

  }

  property("inlined NonEmptyVector of primitives with JsonObject values") {

    case class Primitive(value: JsonObject) derives Codec.AsObject

    given Arbitrary[Primitive] = Arbitrary(Arbitrary.arbitrary[JsonObject].map(Primitive(_)))

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[NonEmptyVector[Primitive]]('sc, 'args)}

    forAll { (vector: NonEmptyVector[Primitive]) =>

      val result: Json = encode"$vector"

      val expected: Json = vector.asJson

      assertEquals(result, expected)

    }

  }

  test("corrupted empty vector parsing compile error") {
    scala.compiletime.testing.typeCheckErrors(
      """
          extension (inline sc: StringContext)
            inline def encode(inline args: Any*): Json =
              ${ macros.encode[NonEmptyVector[Int]]('sc, 'args) }

          encode"[ ]"
        """
    ).headOption match
      case Some(error) => assert(error.message.startsWith("Encode error:"))
      case _           => fail("No compilation error was found.")
  }

  test("corrupted ints parsing compile error") {
    scala.compiletime.testing.typeCheckErrors(
      """
          extension (inline sc: StringContext)
            inline def encode(inline args: Any*): Json =
              ${ macros.encode[NonEmptyVector[Int]]('sc, 'args) }

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
              ${ macros.encode[NonEmptyVector[Option[Int]]]('sc, 'args) }
          encode"[ null, 42, { } ]"
      """
    ).headOption match
      case Some(error) => assert(error.message.startsWith("Encode error:"))
      case _           => fail("No compilation error was found.")
  }
