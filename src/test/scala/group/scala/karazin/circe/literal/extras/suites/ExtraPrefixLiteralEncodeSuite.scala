package group.scala.karazin.circe.literal.extras.suites

import cats.implicits._
import io.circe.syntax._
import io.circe.parser
import io.circe.{Json, JsonObject}
import org.scalacheck._
import org.scalacheck.Prop._
import scala.compiletime.testing.typeCheckErrors

import group.scala.karazin.circe.literal.extras.macros
import group.scala.karazin.circe.literal.extras.model.{given, _}
import group.scala.karazin.circe.literal.extras.model._

class ExtraPrefixLiteralEncodeSuite extends munit.FunSuite:

  test("raw json parsing with missing int key") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Bar](
          'sc, 'args,
          """
            "str": "str",
          """
        ) }

    val result: Json =
      encode"""
              {
                "bool": true,
                "unit": { }
               }
              """

    val expected =
      parser.parse(
        s"""
        {
          "str": "str",
          "bool": true,
          "unit": { }
        }
        """
      ).toOption.get

    assert(result == expected)

  }

  test("raw json parsing with missing int and bar keys") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Foo](
          'sc, 'args,
          """
            "int": 42,
            "bar": {
              "str": "str",
              "bool": true,
              "unit": { }
            },
          """
        ) }

    lazy val result: Json =
      encode"""
              {
                "buzzes": [
                  {
                    "int": 42,
                    "bool": true,
                    "short": 4
                  },
                  {
                    "int": 42,
                    "bool": false,
                    "short": 4
                  }
                ],
                "qux": {
                  "bool": true
                }
              }
              """

    val expected =
      parser.parse(
        s"""
        {
          "int": 42,
          "bar": {
             "str": "str",
             "bool": true,
             "unit": { }
          },
          "buzzes": [
            {
              "int": 42,
              "bool": true,
              "short": 4
            },
            {
              "int": 42,
              "bool": false,
              "short": 4
            }
          ],
          "qux": {
            "bool": true
          }
        }
        """
      ).toOption.get

    assert(result == expected)

  }
