package group.scala.karazin.circe.literal.extras

import cats.implicits._
import cats.laws.discipline.arbitrary._
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import io.circe.syntax._
import io.circe.{Json, parser}
import io.circe.disjunctionCodecs.encodeValidated
import org.scalacheck._
import org.scalacheck.Prop._

class ValidatedEncodeSuite extends munit.ScalaCheckSuite:

  property("inlined primitive with Currency value") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        $ {macros.encode[Validated[Int, String]]('sc, 'args)}

    forAll { (value: Validated[Int, String]) =>

      val expected: Json = encode"$value"

      val result: Json = value.asJson

      assertEquals(result, expected)

    }

  }

  property("inlined int parsing") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Validated[Int, String]]('sc, 'args) }

    forAll { (value: Int) =>

      val validated: Validated[Int, String] = Invalid(value)

      val result: Json =
        encode""" {
                      "Invalid": $value
                    }
                    """

      val expected: Json = validated.asJson

      assertEquals(result, expected)
    }

  }

  property("inlined string parsing") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Validated[Int, String]]('sc, 'args) }

    forAll { (value: String) =>

      val validated: Validated[Int, String] = Valid(value)

      val result: Json =
        encode""" {
                      "Valid": $value
                    }
                    """

      val expected: Json = validated.asJson

      assertEquals(result, expected)
    }

  }

  property("inlined Validated[Option[Int], List[String]] like object parsing") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Validated[Option[Int], List[String]]]('sc, 'args) }

    forAll { (value: Validated[Int, List[String]]) =>

      val result: Json = encode""" $value """

      val expected: Json = value.asJson

      assertEquals(result, expected)
    }

  }

  test("corrupted Validated.Valid type parsing compile error") {
    scala.compiletime.testing.typeCheckErrors(
      """
            extension (inline sc: StringContext)
              inline def encode(inline args: Any*): Json =
                ${ macros.encode[Validated[Option[Int], List[String]]]('sc, 'args) }

            val value: Validated[Option[Int], String] = Invalid(Some(3))

            encode""""" + """" $value """"" + """"
          """
    ).headOption match
      case Some(error) => assert(error.message.startsWith("Encode error:"))
      case _           => fail("No compilation error was found.")
  }

  test("corrupted Validated.Invalid type parsing compile error") {
    scala.compiletime.testing.typeCheckErrors(
      """
            extension (inline sc: StringContext)
              inline def encode(inline args: Any*): Json =
                ${ macros.encode[Validated[List[String], Int]]('sc, 'args) }

            val value: Validated[String, Int] = Invalid("")

            encode""""" + """" $value """"" + """"
          """
    ).headOption match
      case Some(error) => assert(error.message.startsWith("Encode error:"))
      case _           => fail("No compilation error was found.")
  }

  test("corrupted Validated with extra fields parsing compile error") {
    scala.compiletime.testing.typeCheckErrors(
      """
            extension (inline sc: StringContext)
              inline def encode(inline args: Any*): Json =
                ${ macros.encode[Validated[Option[Int], List[String]]]('sc, 'args) }

            encode""""" + """" 
                {
                  "Invalid": 0,
                  "extra": 0
                } 
                """"" + """"
          """
    ).headOption match
      case Some(error) => assert(error.message.startsWith("Encode error:"))
      case _           => fail("No compilation error was found.")
  }

  test("corrupted Validated.Valid parsing compile error") {
    scala.compiletime.testing.typeCheckErrors(
      """
            extension (inline sc: StringContext)
              inline def encode(inline args: Any*): Json =
                ${ macros.encode[Validated[Option[Int], List[String]]]('sc, 'args) }

            encode""""" + """" 
                {
                  "Valid": 0
                } 
                """"" + """"
          """
    ).headOption match
      case Some(error) => assert(error.message.startsWith("Encode error:"))
      case _           => fail("No compilation error was found.")
  }

  test("corrupted Validated parsing compile error") {
    scala.compiletime.testing.typeCheckErrors(
      """
            extension (inline sc: StringContext)
              inline def encode(inline args: Any*): Json =
                ${ macros.encode[Validated[Option[Int], List[String]]]('sc, 'args) }

            val value: Validated[List[String], Option[Int]] = ???

            encode""""" + """" 
                    $value
                """"" + """"
          """
    ).headOption match
      case Some(error) => assert(error.message.startsWith("Encode error:"))
      case _           => fail("No compilation error was found.")
  }

