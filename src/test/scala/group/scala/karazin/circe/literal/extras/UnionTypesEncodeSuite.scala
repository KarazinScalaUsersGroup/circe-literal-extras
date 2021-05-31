package group.scala.karazin.circe.literal.extras

import io.circe.syntax._
import io.circe.parser
import io.circe.Json
import org.scalacheck._
import org.scalacheck.Prop._

import group.scala.karazin.circe.literal.extras.arbitraries.instances.{given, _}

class UnionTypesEncodeSuite extends munit.ScalaCheckSuite:

  property("inlined Int | String value") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Int | String]('sc, 'args) }

    forAll { (value: Int | String) =>

      val result = encode"$value"

      val expected: Json = value.asJson

      assertEquals(result, expected)

    }
  }

  property("inlined String | Int value") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Int | String]('sc, 'args) }

    forAll { (value: String | Int) =>

      val result = encode"$value"

      val expected: Json = value.asJson

      assertEquals(result, expected)

    }
  }

  property("inlined Int value") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Int | String]('sc, 'args) }

    forAll { (value: Int) =>

      val result = encode"$value"

      val expected: Json = value.asJson

      assertEquals(result, expected)

    }
  }

  property("inlined String value") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Int | String]('sc, 'args) }

    forAll { (value: String) =>

      val result = encode"$value"

      val expected: Json = value.asJson

      assertEquals(result, expected)

    }
  }

//  property("inlined Int | Boolean value compile error") {
//
//    extension (inline sc: StringContext)
//      inline def encode(inline args: Any*): Json =
//        ${ macros.encode[Int | String]('sc, 'args) }
//
//    forAll { (value: Int | Boolean) =>
//
//      val result = encode"$value"
//
//      val expected: Json = value.asJson
//
//      assertEquals(result, expected)
//
//    }
//  }

  test("corrupted ints parsing compile error") {
    scala.compiletime.testing.typeCheckErrors(
      """
          extension (inline sc: StringContext)
            inline def encode(inline args: Any*): Json =
              ${ macros.encode[Int | String]('sc, 'args) }

          encode"true"
        """
    ).headOption match
      case Some(error) => assert(error.message.startsWith("Encode error:"))
      case _           => fail("No compilation error was found.")
  }