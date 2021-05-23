package group.scala.karazin.circe.literal.extras

import io.circe._
import io.circe.syntax._
import org.scalacheck._
import org.scalacheck.Prop._
import group.scala.karazin.circe.literal.extras.arbitraries.instances.{given, _}

class VectorEncodeSuite extends munit.ScalaCheckSuite:

  property("inlined Vector with Int values") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Vector[Int]]('sc, 'args) }

    forAll { (vector: Vector[Int]) =>

      val result = encode"$vector"

      val expected = vector.asJson

      assertEquals(result, expected)

    }
  }

  property("inlined Vector of Option Int values") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Vector[Option[Int]]]('sc, 'args)}

    forAll { (vector: Vector[Option[Int]]) =>

      val result: Json = encode"$vector"

      val expected: Json = vector.asJson

      assertEquals(result, expected)

    }

  }

  property("inlined Vector of primitives with Int values") {

    case class Primitive(value: Int) derives Codec.AsObject

    given Arbitrary[Primitive] = Arbitrary(Arbitrary.arbitrary[Int].map(Primitive(_)))

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Vector[Primitive]]('sc, 'args)}

    forAll { (vector: Vector[Primitive]) =>

      val result: Json = encode"$vector"

      val expected: Json = vector.asJson

      assertEquals(result, expected)

    }

  }


  property("inlined Vector of Option primitives") {

    case class Primitive(value: Int) derives Codec.AsObject

    given Arbitrary[Primitive] = Arbitrary(Arbitrary.arbitrary[Int].map(Primitive(_)))

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Vector[Option[Primitive]]]('sc, 'args)}

    forAll { (vector: Vector[Option[Primitive]]) =>

      val result: Json = encode"$vector"

      val expected: Json = vector.asJson

      assertEquals(result, expected)

    }

  }

  property("inlined Vector of JsonObject values") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Vector[JsonObject]]('sc, 'args)}

    forAll { (vector: Vector[JsonObject]) =>

      val result: Json = encode"$vector"

      val expected: Json = vector.asJson

      assertEquals(result, expected)

    }

  }

  property("inlined Vector of primitives with JsonObject values") {

    case class Primitive(value: JsonObject) derives Codec.AsObject

    given Arbitrary[Primitive] = Arbitrary(Arbitrary.arbitrary[JsonObject].map(Primitive(_)))

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Vector[Primitive]]('sc, 'args)}

    forAll { (vector: Vector[Primitive]) =>

      val result: Json = encode"$vector"

      val expected: Json = vector.asJson

      assertEquals(result, expected)

    }

  }

  test("corrupted ints parsing compile error") {
    scala.compiletime.testing.typeCheckErrors(
      """
          extension (inline sc: StringContext)
            inline def encode(inline args: Any*): Json =
              ${ macros.encode[Vector[Int]]('sc, 'args) }

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
              ${ macros.encode[Vector[Option[Int]]]('sc, 'args) }
          encode"[ null, 42, { } ]"
      """
    ).headOption match
      case Some(error) => assert(error.message.startsWith("Encode error:"))
      case _           => fail("No compilation error was found.")
  }
