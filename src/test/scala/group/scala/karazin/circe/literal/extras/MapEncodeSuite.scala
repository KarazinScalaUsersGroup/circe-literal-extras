package group.scala.karazin.circe.literal.extras

import io.circe._
import io.circe.syntax._
import org.scalacheck._
import org.scalacheck.Prop._
import group.scala.karazin.circe.literal.extras.arbitraries.instances.{given, _}

class MapEncodeSuite extends munit.ScalaCheckSuite:

  property("inlined Map with Int values") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Map[String, Int]]('sc, 'args) }

    forAll { (map: Map[String, Int]) =>

      val result = encode"$map"

      val expected = map.asJson

      assertEquals(result, expected)

    }
  }

  property("inlined Map of Option Int values") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Map[String, Option[Int]]]('sc, 'args)}

    forAll { (map: Map[String, Option[Int]]) =>

      val result: Json = encode"$map"

      val expected: Json = map.asJson

      assertEquals(result, expected)

    }

  }

  property("inlined Map of primitives with Int values") {

    case class Primitive(value: Int) derives Codec.AsObject

    given Arbitrary[Primitive] = Arbitrary(Arbitrary.arbitrary[Int].map(Primitive(_)))

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Map[String, Primitive]]('sc, 'args)}

    forAll { (map: Map[String, Primitive]) =>

      val result: Json = encode"$map"

      val expected: Json = map.asJson

      assertEquals(result, expected)

    }

  }


  property("inlined Map of Option primitives") {

    case class Primitive(value: Int) derives Codec.AsObject

    given Arbitrary[Primitive] = Arbitrary(Arbitrary.arbitrary[Int].map(Primitive(_)))

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Map[String, Option[Primitive]]]('sc, 'args)}

    forAll { (map: Map[String, Option[Primitive]]) =>

      val result: Json = encode"$map"

      val expected: Json = map.asJson

      assertEquals(result, expected)

    }

  }

  property("inlined Map of JsonObject values") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Map[String, JsonObject]]('sc, 'args)}

    forAll { (map: Map[String, JsonObject]) =>

      val result: Json = encode"$map"

      val expected: Json = map.asJson

      assertEquals(result, expected)

    }

  }

  property("inlined Map of primitives with JsonObject values") {

    case class Primitive(value: JsonObject) derives Codec.AsObject

    given Arbitrary[Primitive] = Arbitrary(Arbitrary.arbitrary[JsonObject].map(Primitive(_)))

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Map[String, Primitive]]('sc, 'args)}

    forAll { (map: Map[String, Primitive]) =>

      val result: Json = encode"$map"

      val expected: Json = map.asJson

      assertEquals(result, expected)

    }

  }

  test("corrupted ints parsing compile error") {
    scala.compiletime.testing.typeCheckErrors(
      """
          extension (inline sc: StringContext)
            inline def encode(inline args: Any*): Json =
              ${ macros.encode[Map[String, Int]]('sc, 'args) }

          encode""""" + """"{ "key": { } }""""" + """"
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
              ${ macros.encode[Map[String, Option[Int]]]('sc, 'args) }

          encode""""" + """"{ "hello" : 42, "world": true }""""" + """"
      """
    ).headOption match
      case Some(error) => assert(error.message.startsWith("Encode error:"))
      case _           => fail("No compilation error was found.")
  }

  test("unexpected json structure error") {
    scala.compiletime.testing.typeCheckErrors(
      """
          extension (inline sc: StringContext)
            inline def encode(inline args: Any*): Json =
              ${ macros.encode[Map[String, Int]]('sc, 'args) }

          encode""""" + """"[ "hello", 42 ]""""" + """"
      """
    ).headOption match
      case Some(error) => assert(error.message.startsWith("Encode error:"))
      case _           => fail("No compilation error was found.")
  }

  test("incorrect key type compile error") {
    scala.compiletime.testing.typeCheckErrors(
      """
          extension (inline sc: StringContext)
            inline def encode(inline args: Any*): Json =
              ${ macros.encode[Map[Int, String]]('sc, 'args) }

          encode""""" + """"{ 42: "hello" }""""" + """"
      """
    ).headOption match
      case Some(error) => assert(error.message.toLowerCase.startsWith("cannot prove json structure"))
      case _           => fail("No compilation error was found.")
  }
