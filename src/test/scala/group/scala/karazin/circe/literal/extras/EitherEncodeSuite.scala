package group.scala.karazin.circe.literal.extras

import group.scala.karazin.circe.literal.extras.macros
import io.circe._
import io.circe.disjunctionCodecs._
import io.circe.syntax._
import org.scalacheck.Prop._
import org.scalacheck._

class EitherEncodeSuite extends munit.ScalaCheckSuite:

  property("inlined Either[Int, String] parsing") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Either[Int, String]]('sc, 'args) }

    forAll { (value: Either[Int, String]) =>

      val result: Json = encode""" $value """

      val expected: Json = value.asJson

      assertEquals(result, expected)
    }

  }

  property("inlined int parsing") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Either[Int, String]]('sc, 'args) }

    forAll { (value: Int) =>
      
      val either: Either[Int, String] = Left(value)
      
      val result: Json =
        encode""" {
                    "Left": $value
                  }
                  """
      
      val expected: Json = either.asJson

      assertEquals(result, expected)
    }

  }

  property("inlined string parsing") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Either[Int, String]]('sc, 'args) }

    forAll { (value: String) =>

      val either: Either[Int, String] = Right(value)

      val result: Json =
        encode""" {
                    "Right": $value
                  }
                  """

      val expected: Json = either.asJson

      assertEquals(result, expected)
    }

  }

  property("inlined Either[Option[Int], List[String]] parsing") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Either[Option[Int], List[String]]]('sc, 'args) }

    forAll { (value: Either[Int, List[String]]) =>

      val result: Json = encode""" $value """

      val expected: Json = value.asJson

      assertEquals(result, expected)
    }

  }

  test("corrupted Either.Right type parsing compile error") {
    scala.compiletime.testing.typeCheckErrors(
      """
          extension (inline sc: StringContext)
            inline def encode(inline args: Any*): Json =
              ${ macros.encode[Either[Option[Int], List[String]]]('sc, 'args) }
              
          val value: Either[Option[Int], String] = Left(Some(3))
           
          encode""""" + """" $value """"" + """"
        """
    ).headOption match
      case Some(error) => assert(error.message.startsWith("Encode error:"))
      case _           => fail("No compilation error was found.")
  }

  test("corrupted Either with extra fields parsing compile error") {
    scala.compiletime.testing.typeCheckErrors(
      """
          extension (inline sc: StringContext)
            inline def encode(inline args: Any*): Json =
              ${ macros.encode[Either[Option[Int], List[String]]]('sc, 'args) }
           
          encode""""" + """" 
              {
                "Left": 0,
                "extra": 0
              } 
              """"" + """"
        """
    ).headOption match
      case Some(error) => assert(error.message.startsWith("Encode error:"))
      case _           => fail("No compilation error was found.")
  }

  test("corrupted Either.Right parsing compile error") {
    scala.compiletime.testing.typeCheckErrors(
      """
          extension (inline sc: StringContext)
            inline def encode(inline args: Any*): Json =
              ${ macros.encode[Either[Option[Int], List[String]]]('sc, 'args) }
           
          encode""""" + """" 
              {
                "Right": 0
              } 
              """"" + """"
        """
    ).headOption match
      case Some(error) => assert(error.message.startsWith("Encode error:"))
      case _           => fail("No compilation error was found.")
  }

  test("corrupted Either parsing compile error") {
    scala.compiletime.testing.typeCheckErrors(
      """
          extension (inline sc: StringContext)
            inline def encode(inline args: Any*): Json =
              ${ macros.encode[Either[Option[Int], List[String]]]('sc, 'args) }
          
          val value: Either[List[String], Option[Int]] = ???
           
          encode""""" + """" 
                  $value
              """"" + """"
        """
    ).headOption match
      case Some(error) => assert(error.message.startsWith("Encode error:"))
      case _           => fail("No compilation error was found.")
  }
