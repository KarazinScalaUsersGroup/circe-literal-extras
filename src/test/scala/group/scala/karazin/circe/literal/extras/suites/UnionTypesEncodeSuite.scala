package group.scala.karazin.circe.literal.extras.suites

import io.circe.syntax._
import io.circe.parser
import io.circe.Json
import org.scalacheck._
import org.scalacheck.Prop._

import group.scala.karazin.circe.literal.extras.macros
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

  property("inlined List of Union types") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[List[Int | String]]('sc, 'args) }

    forAll { (value: List[Int | String]) =>

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

  property("inlined Int parsing value") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Int | String | Boolean]('sc, 'args) }

    forAll { (value: Int) =>

      val result = encode"$value"

      val expected: Json = value.asJson

      assertEquals(result, expected)

    }
  }

  property("inlined String parsing value") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Int | String | Boolean]('sc, 'args) }

    forAll { (value: String) =>

      val result = encode"$value"

      val expected: Json = value.asJson

      assertEquals(result, expected)

    }
  }

  property("inlined Boolean parsing value") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Int | String | Boolean]('sc, 'args) }

    forAll { (value: Boolean) =>

      val result = encode"$value"

      val expected: Json = value.asJson

      assertEquals(result, expected)

    }
  }

  property("inlined String parsing value") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Int | String | Boolean]('sc, 'args) }

    forAll { (value: Boolean) =>

      val result = encode"$value"

      val expected: Json = value.asJson

      assertEquals(result, expected)

    }
  }

  property("inlined Union of List[List[Int]] and Option[String] parsing value") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[List[List[Int]] | Option[String]]('sc, 'args) }

    forAll { (valueLists: List[List[Int]], valueOption: Option[String]) =>

      val resultLists = encode"$valueLists"
      val resultOption = encode"$valueOption"

      val expectedList: Json = valueLists.asJson
      val expectedOption: Json = valueOption.asJson

      assertEquals(resultLists, expectedList)
      assertEquals(resultOption, expectedOption)

    }

  }

  property("inlined Union List[Option[Int]] and Option[List[Int]] parsing value") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[List[Option[Int]] | Option[List[Int]]]('sc, 'args) }

    forAll { (valueListOpt: List[Option[Int]], valueOptList: Option[List[Int]]) =>

      val resultListOpt = encode"$valueListOpt"
      val resultOptList = encode"$valueOptList"

      val expectedListOpt: Json = resultListOpt.asJson
      val expectedOptList: Json = resultOptList.asJson

      assertEquals(resultListOpt, expectedListOpt)
      assertEquals(resultOptList, expectedOptList)

    }
  }

  test("corrupted Int | Boolean parsing compile error") {
    scala.compiletime.testing.typeCheckErrors(
      """
          extension (inline sc: StringContext)
            inline def encode(inline args: Any*): Json =
              ${ macros.encode[Int | String]('sc, 'args) }

          val value: Int | Boolean = 0

          encode"$value"
        """
    ).headOption match
      case Some(error) => assert(error.message.startsWith("Encode error:"))
      case _           => fail("No compilation error was found.")
  }

  test("corrupted double parsing compile error") {
    scala.compiletime.testing.typeCheckErrors(
      """
          extension (inline sc: StringContext)
            inline def encode(inline args: Any*): Json =
              ${ macros.encode[Int | String | Boolean]('sc, 'args) }

          encode"0.1"
        """
    ).headOption match
      case Some(error) => assert(error.message.startsWith("Encode error:"))
      case _           => fail("No compilation error was found.")
  }

  test("type not present in Union compile error") {
    scala.compiletime.testing.typeCheckErrors(
      """
          extension (inline sc: StringContext)
            inline def encode(inline args: Any*): Json =
              ${ macros.encode[Option[String] | String | Boolean]('sc, 'args) }

          encode"1"
        """
    ).headOption match
      case Some(error) => assert(error.message.startsWith("Encode error:"))
      case _           => fail("No compilation error was found.")
  }

  test("corrupted boolean parsing compile error") {
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

  test("corrupted array parsing compile error") {
    scala.compiletime.testing.typeCheckErrors(
      """
          extension (inline sc: StringContext)
            inline def encode(inline args: Any*): Json =
              ${ macros.encode[Int | String]('sc, 'args) }

          encode"[0]"
        """
    ).headOption match
      case Some(error) => assert(error.message.startsWith("Encode error:"))
      case _           => fail("No compilation error was found.")
  }

  test("corrupted array parsing compile error") {
    scala.compiletime.testing.typeCheckErrors(
      """
          extension (inline sc: StringContext)
            inline def encode(inline args: Any*): Json =
              ${ macros.encode[Int | Boolean]('sc, 'args) }

          encode"[0, true]"
        """
    ).headOption match
      case Some(error) => assert(error.message.startsWith("Encode error:"))
      case _           => fail("No compilation error was found.")
  }
