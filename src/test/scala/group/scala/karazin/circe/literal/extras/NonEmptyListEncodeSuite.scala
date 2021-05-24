package group.scala.karazin.circe.literal.extras

import cats.implicits._
import cats.data.NonEmptyList
import cats.laws.discipline.arbitrary._
import group.scala.karazin.circe.literal.extras.macros
import io.circe._
import io.circe.syntax._
import org.scalacheck.Prop._
import org.scalacheck._

class NonEmptyListEncodeSuite extends munit.ScalaCheckSuite:
  
  property("inlined NonEmptyList of ints") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[NonEmptyList[Int]]('sc, 'args) }
    
    forAll { (value: NonEmptyList[Int]) =>
      
      lazy val result: Json = encode"""$value"""

      val expected: Json = value.asJson
      
      assertEquals(result, expected)
    }

  }

  property("inlined nested NonEmptyLists parsing") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[NonEmptyList[NonEmptyList[Int]]]('sc, 'args) }

    forAll { (value: NonEmptyList[NonEmptyList[Int]]) =>

      val result: Json = encode"""$value"""

      val expected = value.asJson

      assertEquals(result, expected)
    }
  }

  property("inlined NonEmptyList of options") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[NonEmptyList[Option[Int]]]('sc, 'args) }

    forAll { (options: NonEmptyList[Option[Int]]) =>

      lazy val result: Json = encode"""$options"""

      val expected = options.asJson

      assertEquals(result, expected)

    }
  }

  property("inlined container like object parsing") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[NonEmptyList[Option[Int]]]('sc, 'args) }

    forAll { (value: Iterable[Int]) =>

      val result: Json = encode"""$value"""

      val expected = value.asJson

      assertEquals(result, expected)
    }
  }

  property("inlined NonEmptyList of Primitive objects") {

    case class Primitive(value: Int) derives Codec.AsObject
    given Arbitrary[Primitive] = Arbitrary(Arbitrary.arbitrary[Int] map {v => Primitive(v)})

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[NonEmptyList[Primitive]]('sc, 'args) }
    
    forAll { (value: NonEmptyList[Primitive]) =>

      val result: Json = encode"$value"

      val expected = value.asJson

      assertEquals(result, expected)
    }
  }

  test("corrupted empty list parsing compile error") {
    scala.compiletime.testing.typeCheckErrors(
      """
          extension (inline sc: StringContext)
            inline def encode(inline args: Any*): Json =
              ${ macros.encode[NonEmptyList[Int]]('sc, 'args) }
           
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
              ${ macros.encode[NonEmptyList[Int]]('sc, 'args) }
           
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
              ${ macros.encode[NonEmptyList[Option[Int]]]('sc, 'args) }

          encode"[ null, 42, { } ]" 
      """
    ).headOption match
      case Some(error) => assert(error.message.startsWith("Encode error:"))
      case _           => fail("No compilation error was found.")
  }
