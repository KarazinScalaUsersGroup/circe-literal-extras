package group.scala.karazin.circe.literal.extras.suites

import io.circe._
import io.circe.syntax._
import org.scalacheck._
import org.scalacheck.Prop._
import cats.laws.discipline.arbitrary._
import cats.data.Chain
import group.scala.karazin.circe.literal.extras.macros
import group.scala.karazin.circe.literal.extras.arbitraries.instances.{given, _}

class ChainEncodeSuite extends munit.ScalaCheckSuite:

  property("inlined Chain with Int values") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Chain[Int]]('sc, 'args) }

    forAll { (chain: Chain[Int]) =>

      val result = encode"$chain"

      val expected = chain.asJson

      assertEquals(result, expected)

    }
  }

  property("inlined Chain of Option Int values") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Chain[Option[Int]]]('sc, 'args)}

    forAll { (chain: Chain[Option[Int]]) =>

      val result: Json = encode"$chain"

      val expected: Json = chain.asJson

      assertEquals(result, expected)

    }

  }

  property("inlined Chain of primitives with Int values") {

    case class Primitive(value: Int) derives Codec.AsObject

    given Arbitrary[Primitive] = Arbitrary(Arbitrary.arbitrary[Int].map(Primitive(_)))

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Chain[Primitive]]('sc, 'args)}

    forAll { (chain: Chain[Primitive]) =>

      val result: Json = encode"$chain"

      val expected: Json = chain.asJson

      assertEquals(result, expected)

    }

  }


  property("inlined Chain of Option primitives") {

    case class Primitive(value: Int) derives Codec.AsObject

    given Arbitrary[Primitive] = Arbitrary(Arbitrary.arbitrary[Int].map(Primitive(_)))

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Chain[Option[Primitive]]]('sc, 'args)}

    forAll { (chain: Chain[Option[Primitive]]) =>

      val result: Json = encode"$chain"

      val expected: Json = chain.asJson

      assertEquals(result, expected)

    }

  }

  property("inlined Chain of JsonObject values") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Chain[JsonObject]]('sc, 'args)}

    forAll { (chain: Chain[JsonObject]) =>

      val result: Json = encode"$chain"

      val expected: Json = chain.asJson

      assertEquals(result, expected)

    }

  }

  property("inlined Chain of primitives with JsonObject values") {

    case class Primitive(value: JsonObject) derives Codec.AsObject

    given Arbitrary[Primitive] = Arbitrary(Arbitrary.arbitrary[JsonObject].map(Primitive(_)))

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Chain[Primitive]]('sc, 'args)}

    forAll { (chain: Chain[Primitive]) =>

      val result: Json = encode"$chain"

      val expected: Json = chain.asJson

      assertEquals(result, expected)

    }

  }

  test("corrupted ints parsing compile error") {
    scala.compiletime.testing.typeCheckErrors(
      """
          extension (inline sc: StringContext)
            inline def encode(inline args: Any*): Json =
              ${ macros.encode[Chain[Int]]('sc, 'args) }

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
              ${ macros.encode[Chain[Option[Int]]]('sc, 'args) }
          encode"[ null, 42, { } ]"
      """
    ).headOption match
      case Some(error) => assert(error.message.startsWith("Encode error:"))
      case _           => fail("No compilation error was found.")
  }
