package group.scala.karazin.circe.literal.extras

import io.circe._
import io.circe.syntax._
import org.scalacheck._
import org.scalacheck.Prop._
import group.scala.karazin.circe.literal.extras.arbitraries.instances.{given, _}

class SeqEncodeSuite extends munit.ScalaCheckSuite:

  property("inlined Seq with Int values") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Seq[Int]]('sc, 'args) }

    forAll { (seq: Seq[Int]) =>

      val result = encode"$seq"

      val expected = seq.asJson

      assertEquals(result, expected)

    }
  }

  property("inlined Seq of Option Int values") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Seq[Option[Int]]]('sc, 'args)}

    forAll { (seq: Seq[Option[Int]]) =>

      val result: Json = encode"$seq"

      val expected: Json = seq.asJson

      assertEquals(result, expected)

    }

  }

  property("inlined Seq of primitives with Int values") {

    case class Primitive(value: Int) derives Codec.AsObject

    given Arbitrary[Primitive] = Arbitrary(Arbitrary.arbitrary[Int].map(Primitive(_)))

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Seq[Primitive]]('sc, 'args)}

    forAll { (Seq: Seq[Primitive]) =>

      val result: Json = encode"$Seq"

      val expected: Json = Seq.asJson

      assertEquals(result, expected)

    }

  }


  property("inlined Seq of Option primitives") {

    case class Primitive(value: Int) derives Codec.AsObject

    given Arbitrary[Primitive] = Arbitrary(Arbitrary.arbitrary[Int].map(Primitive(_)))

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Seq[Option[Primitive]]]('sc, 'args)}

    forAll { (seq: Seq[Option[Primitive]]) =>

      val result: Json = encode"$seq"

      val expected: Json = seq.asJson

      assertEquals(result, expected)

    }

  }

  property("inlined Seq of JsonObject values") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Seq[JsonObject]]('sc, 'args)}

    forAll { (seq: Seq[JsonObject]) =>

      val result: Json = encode"$seq"

      val expected: Json = seq.asJson

      assertEquals(result, expected)

    }

  }

  property("inlined Seq of primitives with JsonObject values") {

    case class Primitive(value: JsonObject) derives Codec.AsObject

    given Arbitrary[Primitive] = Arbitrary(Arbitrary.arbitrary[JsonObject].map(Primitive(_)))

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Seq[Primitive]]('sc, 'args)}

    forAll { (seq: Seq[Primitive]) =>

      val result: Json = encode"$seq"

      val expected: Json = seq.asJson

      assertEquals(result, expected)

    }

  }

  test("corrupted ints parsing compile error") {
    scala.compiletime.testing.typeCheckErrors(
      """
          extension (inline sc: StringContext)
            inline def encode(inline args: Any*): Json =
              ${ macros.encode[Seq[Int]]('sc, 'args) }

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
              ${ macros.encode[Seq[Option[Int]]]('sc, 'args) }
          encode"[ null, 42, { } ]"
      """
    ).headOption match
      case Some(error) => assert(error.message.startsWith("Encode error:"))
      case _           => fail("No compilation error was found.")
  }
